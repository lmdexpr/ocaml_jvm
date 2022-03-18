type primitive = String of string | Not_implemented
type 'a java_method = primitive list -> 'a

let print_stream_println =
  let print_stream = new Io.print_stream in
  function String s :: _ -> print_stream#println s | _ -> invalid_arg ""

let get_method = function
  | "println" -> print_stream_println
  | name -> invalid_arg @@ "not implemented for " ^ name

let call method_name arguments = get_method method_name arguments
