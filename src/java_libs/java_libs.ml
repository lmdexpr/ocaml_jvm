type primitive =
  [ `String of string
  | `Int of int
  | `Not_implemented
  ]

type 'a java_method = primitive list -> 'a

let routing mtd = function
  | `String s :: _ -> mtd s
  | `Int i :: _ -> mtd @@ string_of_int i
  | [] -> invalid_arg "empty arguments"
  | _ -> invalid_arg "print_stream#println"

let print_stream = new Io.print_stream

let get_method = function
  | "println" -> routing print_stream#println
  | "print" -> routing print_stream#print
  | name -> invalid_arg @@ "not implemented for " ^ name

let call method_name arguments = get_method method_name arguments
