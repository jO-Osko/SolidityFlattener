let get_arg_list () =
  let args =
    Js_of_ocaml.Js.Unsafe.(variable "process.argv")
    |> Js_of_ocaml.Js.array_map Js_of_ocaml.Js.to_string
    |> Js_of_ocaml.Js.to_array
  in
  args

let next_tick (_callback : 'a -> 'b) =
  Js_of_ocaml.Js.Unsafe.(
    fun_call
      (js_expr "process.nextTick")
      [| inject (Js_of_ocaml.Js.wrap_callback _callback) |])

let get_remote_body url =
  let rec run t =
    Lwt.wakeup_paused ();
    match Lwt.poll t with Some x -> x | None -> next_tick (fun () -> run t)
  in
  let url = Uri.of_string url in
  let _response, body = Cohttp_lwt_jsoo.Client_sync.get url |> run in
  let body_value = Cohttp_lwt.Body.to_string body |> run in
  body_value

let main = Flattener.main get_arg_list get_remote_body
