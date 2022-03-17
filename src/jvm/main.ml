open Stdint
open Utils

let class_file_name = ref ""
let spec = []

(* todo load constructor *)
let run machine =
  match Machine.entry_point machine with
  | Code (value, _attr) ->
    Array.to_list value.code |> List.map Uint8.to_int
    |> Instruction.step machine
  | _ -> print_endline "illegal entry_point"

let () =
  Arg.parse spec (fun f -> class_file_name := f) "ocaml_jvm <class_file>";
  open_in !class_file_name
  |> unwind ~protect:close_in Classfile.read
  |> Machine.create |> run
