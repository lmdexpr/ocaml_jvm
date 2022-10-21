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

module Reader = struct
  let read_byte ic = try Some (input_char ic) with End_of_file -> None

  let read_bytes ic length =
    let rec loop count acc =
      if count > 0 then
        match read_byte ic with
        | Some b -> loop (count - 1) (Seq.cons b acc)
        | None -> acc
      else acc
    in
    loop length Seq.empty |> Bytes.of_seq

  module Uint = struct
    module Make_uint_reader (U : Uint.With_bytes_conv) = struct
      include U

      let read ic : U.t = read_bytes ic U.length |> U.of_bytes
    end

    module U8 = struct
      include Make_uint_reader (Uint.U8)
    end

    module U16 = struct
      include Make_uint_reader (Uint.U16)
    end

    module U32 = struct
      include Make_uint_reader (Uint.U32)
    end
  end
end
