let class_file = ref ""
let spec = []

let () =
  Arg.parse spec (fun f -> class_file := f) "ocaml-jvm <class_file>";
  let ic = open_in !class_file in
  try 
    Reader.read_class_file ic;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
