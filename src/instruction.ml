open Stdint

let getstatic byte1 byte2 =
  let byte1 = Uint16.of_uint8 byte1 
  and byte2 = Uint16.of_uint8 byte2 in
  let index = Uint16.((shift_left byte1 8) + byte2) |> Uint16.to_int in
  Printf.pritnf "getstatic index: %x\n" index

let ldc operand = Printf.printf "ldc %x\n" (Uint8.to_int operand)

let invokevirtual operand1 operand2 =
  Printf.printf "invokevirtual %x %x\n" (Uint8.to_int operand1) (Uint8.to_int operand2)

let return () = print_endline "return"
