let class_file = ref ""
let spec = []

let () =
  Arg.parse spec (fun f -> class_file := f) "ocaml_jvm <class_file>";
  let ic = open_in !class_file in
  try 
    Classfile.read ic |> Classfile.debug_print;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
