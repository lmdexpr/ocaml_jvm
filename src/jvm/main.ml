open Utils

let class_file_name = ref ""
let spec = []

let unwind ~(protect : 'a -> unit) f x =
  try
    let y = f x in
    protect x;
    y
  with e ->
    protect x;
    raise e

let () =
  Arg.parse spec (fun f -> class_file_name := f) "ocaml_jvm <class_file>";
  let entry_class =
    open_in !class_file_name |> unwind Classfile.read ~protect:close_in
  in
  let entry_point = Classfile.entry_point entry_class in
  Machine.make entry_class |> Machine.invoke entry_point |> Try.get
