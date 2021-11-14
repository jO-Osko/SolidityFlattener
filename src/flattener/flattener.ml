module Parser = Solidity_parser
module Common = Solidity_common
module Ast = Solidity_ast
module Sm = Common.StringMap
module Ss = Common.StringSet

let ends_with suffix s =
  let len_s = String.length s and len_suf = String.length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf then true
    else if String.unsafe_get s (diff + i) <> String.unsafe_get suffix i then
      false
    else aux (i + 1)
  in
  diff >= 0 && aux 0

let default_library_prefixes = [ "@openzeppelin"; "@gnosis.pm" ]

let make_replacer node_modules replacable_bindings =
  let replacer parent str =
    (* Printf.printf "Locating: %s\n" str; *)
    let _ = Option.value parent ~default:"root" in
    let rec replacer_aux = function
      | [] -> str
      | x :: xs -> (
          let re = Str.regexp x in
          try
            let _index = Str.search_forward re str 0 in
            (* Printf.printf "Found: %s -> %s\n" (node_modules ^ str) x; *)
            node_modules ^ str
          with Not_found -> replacer_aux xs)
    in

    replacer_aux replacable_bindings
  in
  let checker parent str =
    let _ = Option.value parent ~default:"root" in
    let rec checker_aux = function
      | [] -> true
      | x :: xs -> (
          let re = Str.regexp x in
          try
            let _index = Str.search_forward re str 0 in
            false
          with Not_found -> checker_aux xs)
    in

    checker_aux replacable_bindings
  in
  (replacer, checker)

let github_replacer =
  Str.regexp
    "\\(http\\|https\\)://github.com/\\(.*?\\)/\\(.*?\\)/\\(blob\\)/\\(.*\\)"

let github_matrix = "https://raw.githubusercontent.com/\\2/\\3/\\5"

let gitlab_replacer =
  Str.regexp
    "\\(http\\|https\\)://gitlab.com/\\(.*?\\)/\\(.*?\\)/\\(blob\\)/\\(.*\\)"

let gitlab_matrix = "\\1://gitlab.com/\\2/\\3/raw/\\5"

type url_replacer = { match_regex : Str.regexp; target : string }

let url_replacers =
  [
    { match_regex = github_replacer; target = github_matrix };
    { match_regex = gitlab_replacer; target = gitlab_matrix };
  ]

let file_cache = ref Sm.empty

let file_reader get_remote_body fname =
  match Sm.find_opt fname !file_cache with
  | Some x -> x
  | None ->
      let result =
        if
          Solidity_common.starts_with ~prefix:"http://" fname
          || Solidity_common.starts_with ~prefix:"https://" fname
        then
          match
            List.find_map
              (fun { match_regex; target } ->
                if Str.string_match match_regex fname 0 then
                  let url = Str.global_replace match_regex target fname in
                  let contents = get_remote_body url in
                  Some contents
                else None)
              url_replacers
          with
          | Some c -> c
          | None -> failwith "Unknown url"
        else EzFile.read_file fname
      in
      file_cache := Sm.add fname result !file_cache;
      result

(*****)

type 'a mapped = { value : 'a; uid : int }

let cmp { uid = v1; _ } { uid = v2; _ } = Int.compare v1 v2

type fname = Filename of string

type filename = fname mapped

let filename_to_string { value = Filename s; _ } = s

type cname = ContractName of string

type contract = Ast.contract_definition mapped

let contract_to_string { value = { Solidity_ast.contract_name = cname; _ }; _ }
    =
  cname.contents |> Common.Ident.to_string

module FileMap = Common.ExtMap.Make (struct
  type t = filename

  let compare = cmp

  let to_string = filename_to_string
end)

module FilenameSet = Set.Make (struct
  type t = filename

  let compare = cmp
end)

module ContractMap = Common.ExtMap.Make (struct
  type t = contract

  let compare = cmp

  let to_string = contract_to_string
end)

module IntSet = Set.Make (Int)

let print x = Format.fprintf Format.std_formatter x

type ('a, 'b) maps' = { contract_map : 'a; file_map : 'b }

type maps = (int ContractMap.t, int FileMap.t) maps'

let enumerate f l = List.mapi (fun i (v, _) -> { value = f v; uid = i }) l

let create_maps (program : Ast.program) =
  (* TODO WHAT HAPPENS WITH CONTRACTS WITH THE SAME NAME?? *)
  let bindings = Sm.bindings program.program_modules_by_file in
  let files =
    enumerate (fun name -> Filename name) bindings
    (* List.mapi (fun i (fname, _) -> { value = Filename fname; uid = i }) bindings *)
  in
  let (contract_to_file_map, file_to_contract_map, _), _contracts =
    List.map
      (fun (_, module_) ->
        let contracts =
          List.filter_map
            (fun c ->
              match c.Solidity_common.contents with
              | Solidity_ast.ContractDefinition c -> Some c
              | _ -> None)
            module_.Solidity_ast.module_units
        in
        contracts)
      bindings
    |> List.map2 (fun fname contracts -> (fname, contracts)) files (* zip *)
    |> List.fold_left_map
         (fun (cf_map, fc_map, num) (fname, contracts) ->
           let contracts : contract list =
             List.mapi (fun i c -> { value = c; uid = i + num }) contracts
           in
           let num = num + List.length contracts in
           let fc_map = FileMap.add_uniq fname contracts fc_map in
           let cf_map =
             List.fold_left
               (fun cf_map c -> ContractMap.add_uniq c fname cf_map)
               cf_map contracts
           in
           ((cf_map, fc_map, num), contracts))
         (ContractMap.empty, FileMap.empty, 0)
  in
  let file_converter_map : filename Sm.t =
    files
    |> List.map (fun file -> (filename_to_string file, file))
    |> Sm.of_bindings
  in
  let file_converter (Filename x) = Sm.find x file_converter_map in
  let contract_converter_map =
    contract_to_file_map |> ContractMap.bindings
    |> List.map (fun (contract, _) -> (contract_to_string contract, contract))
    |> Sm.of_bindings
  in
  let contract_converter (ContractName x) = Sm.find x contract_converter_map in
  ( contract_to_file_map,
    file_to_contract_map,
    file_converter,
    contract_converter )

let build_graph
    ( contract_to_file_map,
      file_to_contract_map,
      _file_converter,
      contract_converter ) : FilenameSet.t FileMap.t =
  ContractMap.fold
    (fun (contract : contract) _ graph ->
      let in_file : filename = ContractMap.find contract contract_to_file_map in
      let extends =
        contract.value.Ast.contract_inheritance
        |> List.map (fun (ident, _) ->
               ContractName (Common.LongIdent.to_string ident.Common.contents))
        |> List.map contract_converter
      in
      List.fold_right
        (fun contract graph ->
          let out_file : filename =
            ContractMap.find contract contract_to_file_map
          in
          (* Graph is reversed *)
          FileMap.update out_file
            (function
              | Some x -> Some (FilenameSet.add in_file x)
              | None -> Some (FilenameSet.singleton in_file))
            graph)
        (* FileMap.update in_file
           (function
             | Some x -> Some (FilenameSet.add out_file x)
             | None -> assert false (* Some (FilenameSet.singleton out_file) *))
           graph) *)
        extends graph)
    contract_to_file_map
    (FileMap.of_bindings
       (FileMap.bindings file_to_contract_map
       |> List.map (fun (f, _) -> (f, FilenameSet.empty))))

let toposort graph =
  let permanent = FilenameSet.empty in
  let temp = FilenameSet.empty in
  let l = [] in
  let rec visit n (temp, permanent, l) =
    if FilenameSet.mem n permanent then (temp, permanent, l)
    else (
      (* Check for cycle -> Should not happen *)
      assert (FilenameSet.mem n temp |> not);

      let temp = FilenameSet.add n temp in
      let outgoing = graph |> FileMap.find n in
      let temp, permanent, l =
        FilenameSet.fold
          (fun m (temp, permanent, l) -> visit m (temp, permanent, l))
          outgoing (temp, permanent, l)
      in
      let temp = FilenameSet.remove n temp in
      let permanent = FilenameSet.add n permanent in
      (temp, permanent, n :: l))
  in
  let _, _, l =
    FileMap.fold
      (fun f _ (temp, permanent, l) ->
        if FilenameSet.mem f permanent then (temp, permanent, l)
        else visit f (temp, permanent, l))
      graph (temp, permanent, l)
  in
  l

type line_type = CommentOut | LicenseLine

let range start stop = List.init (stop - start) (( + ) start)

let bad_lines =
  let rec bad_lines licences abicoders l = function
    | [] -> (licences, abicoders, l)
    | x :: xs -> (
        let _, pos1, pos2 = x.Common.pos in
        let line1 = fst pos1 in
        let line2 = fst pos2 in
        let ex_lines = range line1 (line2 + 1) in
        match x.Common.contents with
        | Solidity_ast.License license ->
            bad_lines
              (Ss.add (license |> String.trim) licences)
              abicoders
              ((LicenseLine, ex_lines) :: l)
              xs
        | Ast.Pragma (idt, str) ->
            let str = String.trim str in
            let idt = idt |> Common.Ident.to_string |> String.trim in
            let new_abicoders, prepend =
              if idt = "abicoder" then
                (Ss.add str abicoders, [ (CommentOut, ex_lines) ])
              else (abicoders, [])
            in
            bad_lines licences new_abicoders (prepend @ l) xs
        | Ast.Import _ ->
            bad_lines licences abicoders ((CommentOut, ex_lines) :: l) xs
        | Ast.GlobalTypeDefinition _ | Ast.GlobalFunctionDefinition _
        | Ast.GlobalVariableDefinition _ | Ast.ContractDefinition _ ->
            bad_lines licences abicoders l xs)
  in
  bad_lines Ss.empty Ss.empty []

let print_flattened original_file sorted _program file_reader node_modules
    ((licences, abicoders), selected_lines) =
  let license = Ss.choose_opt licences in
  if Ss.cardinal licences > 2 then failwith "Conflicting licences";
  let abi_coder = Ss.choose_opt abicoders in
  if Ss.cardinal abicoders > 2 then failwith "Conflicting abi coders";
  let buffer = Buffer.create 100_000 in
  (* Header *)
  Buffer.add_string buffer "// Flattened with jO-Osko's flattener\n";
  (match license with
  | Some l ->
      Buffer.add_string buffer ("// SPDX-License-Identifier: " ^ l ^ "\n")
  | None -> ());
  (match abi_coder with
  | Some l -> Buffer.add_string buffer ("pragma abicoder " ^ l ^ ";\n")
  | None -> ());
  List.iter2
    (fun fname selected_lines ->
      let contract_lines =
        fname |> filename_to_string |> file_reader |> String.split_on_char '\n'
      in
      let selected_lines = selected_lines in
      let lc, abi =
        List.fold_left
          (fun (lc, abi) (reason, lst) ->
            let new_ = IntSet.of_list lst in
            match reason with
            | CommentOut -> (lc, IntSet.union new_ abi)
            | LicenseLine -> (IntSet.union new_ lc, abi))
          (IntSet.empty, IntSet.empty)
          selected_lines
      in
      let s_fname = filename_to_string fname in
      let relative_path =
        if Solidity_common.is_web_resource s_fname then s_fname
        else if Solidity_common.starts_with ~prefix:node_modules s_fname then
          Str.replace_first (Str.regexp node_modules) "" s_fname
        else FilePath.make_relative original_file s_fname
      in
      let relative_path =
        if relative_path = "" then FilePath.basename original_file
        else relative_path
      in
      Buffer.add_string buffer ("// Inlining: " ^ relative_path ^ "\n\n");
      List.iteri
        (fun i line ->
          let i = i + 1 in
          (* 1 based in lexer *)
          Buffer.add_string buffer
            ((if
              IntSet.mem i lc
              &&
              try
                ignore
                  (Str.search_forward
                     (Str.regexp "SPDX-License-Identifier:")
                     line 0);
                true
              with Not_found -> false
             then
              Str.replace_first
                (Str.regexp "SPDX-License-Identifier")
                "INLINED-SPDX-INLINED-License-Identifier" line
             else if IntSet.mem i abi then "// " ^ line
             else line)
            ^ "\n"))
        contract_lines;

      Buffer.add_string buffer ("// End of inlining: " ^ relative_path ^ "\n\n"))
    sorted selected_lines;
  Buffer.contents buffer

(*  *)

let usage_msg =
  "flattener <input> [--node-modules <node_modules_location>] [--contract-libs \
   <lib1>,<lib2>,<lib3>] -o <output>"

type arg_config = {
  node_modules : string;
  contract_libs : string list;
  input_file : string option;
  output_file : string option;
  debug : bool;
}

let args (get_arg_list : unit -> string Array.t) =
  let node_modules = ref "node_modules/" in
  let contract_libs = ref default_library_prefixes in
  let input_file = ref None in
  let output_file = ref None in
  let debug = false in

  let speclist =
    [
      ( "--node-modules",
        Arg.Set_string node_modules,
        "Path to node modules for additional contracts" );
      ( "--contract-libs",
        Arg.String
          (fun s ->
            contract_libs := String.split_on_char ',' s |> List.map String.trim),
        "Additional node libraries that provide contract implementations" );
      ("-o", Arg.String (fun s -> output_file := Some s), "Set output file name");
    ]
  in
  let arg_list = get_arg_list () in
  (* List cwd *)
  let has_node_modules dir =
    (* Printf.printf "Trying: %s\n" dir; *)
    Sys.readdir dir
    |> Array.exists (fun x ->
           Sys.is_directory x && String.equal "node_modules" x)
  in

  let parents = [ "./"; "../"; "../../"; "../../../" ] in
  (node_modules :=
     match
       List.find_opt
         (fun x ->
           has_node_modules
             (Solidity_common.make_absolute_path (Sys.getcwd ()) x))
         parents
     with
     | Some p -> p ^ "node_modules/"
     | None -> "node_modules/");
  (* Printf.printf "Found node modules: %s\n" !node_modules; *)
  Arg.parse_argv arg_list speclist
    (fun s ->
      (* Printf.printf "Setting: %s" s; *)
      input_file := Some s)
    usage_msg;

  node_modules :=
    Solidity_common.make_absolute_path (Sys.getcwd ()) !node_modules;
  if !node_modules.[String.length !node_modules - 1] <> Filename.dir_sep.[0]
  then node_modules := !node_modules ^ Filename.dir_sep;
  {
    node_modules = !node_modules;
    contract_libs = !contract_libs;
    input_file = !input_file;
    output_file = !output_file;
    debug;
  }

let script_names = [ "jooskos_flattener.js"; "jooskos_flattener" ]

let main get_arg_list get_remote_body =
  let file_reader = file_reader get_remote_body in
  let t1 = Sys.time () in
  Printexc.record_backtrace true;
  let arg_config = args get_arg_list in

  let replacer, _checker =
    make_replacer arg_config.node_modules arg_config.contract_libs
  in
  match arg_config.input_file with
  | None -> failwith "No input file specified\n"
  | Some input_file ->
      if List.exists (fun x -> ends_with x input_file) script_names then
        Printf.printf "Missing input file\n"
      else
        (* Printf.printf "FILE: %s" input_file; *)
        let t2 = Sys.time () in
        let parsed =
          Parser.parse_file ~file_locator:replacer ~file_reader input_file
        in
        let t3 = Sys.time () in
        let ((_, _, _, _) as maps) = create_maps parsed in
        let graph = build_graph maps in
        let sorted = toposort graph in
        let bad =
          sorted
          |> List.map (fun fname ->
                 let s = fname |> filename_to_string in
                 let module_ = Sm.find s parsed.program_modules_by_file in
                 let units = module_.Ast.module_units in
                 bad_lines units)
          |> List.fold_left_map
               (fun (full_lic, full_abicoder) (licences, abicoders, lines) ->
                 ( (Ss.union full_lic licences, Ss.union full_abicoder abicoders),
                   lines ))
               (Ss.empty, Ss.empty)
        in
        let t4 = Sys.time () in
        let s =
          print_flattened
            (Solidity_common.make_absolute_path (Sys.getcwd ()) input_file)
            sorted parsed file_reader arg_config.node_modules bad
        in
        let t5 = Sys.time () in
        (match arg_config.output_file with
        | Some f ->
            let oc = open_out f in
            Printf.fprintf oc "%s" s;
            close_out oc
        | None -> Printf.printf "%s\n" s);
        let t6 = Sys.time () in
        if arg_config.debug then
          Printf.printf "%f parse:%f sort:%f pf:%f print:%f\n" (t2 -. t1)
            (t3 -. t2) (t4 -. t3) (t5 -. t4) (t6 -. t5)
