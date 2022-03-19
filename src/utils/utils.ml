module Reader = Reader
module Try = Try

let array_to_string ?(prefix = "") arr f =
  "[\n"
  ^ Array.fold_left (fun acc e -> acc ^ prefix ^ "  " ^ f e ^ ";\n") "" arr
  ^ prefix ^ "]"

let unwind ~(protect : 'a -> unit) f x =
  try
    let y = f x in
    protect x;
    y
  with e ->
    protect x;
    raise e

let to_signed byte1 byte2 =
  let value = (byte1 lsl 8) lor byte2 in
  if value land 0b1000_0000_0000_0000 = 0 then value else value - 0x10000

let not_implemented ~name = Result.error @@ Failure ("not_implemented " ^ name)

exception Requirements_failed of string

let require_in_range ~name ~lower ~n ~upper ~f =
  if lower <= n && n <= upper then f ()
  else
    let n = string_of_int n in
    let msg =
      name ^ " require " ^ string_of_int lower ^ " <= n <= "
      ^ string_of_int upper ^ ", actual: " ^ n
    in
    Result.error @@ Requirements_failed msg
