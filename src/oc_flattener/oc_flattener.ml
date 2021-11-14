let get_arg_list () = Sys.argv

let get_remote_body _s =
  failwith "get_remote_body not yet supported in native build"

let main = Flattener.main get_arg_list get_remote_body
