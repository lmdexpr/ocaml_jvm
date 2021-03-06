open Stdint

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

let read_u1 ic = read_bytes ic 1 |> fun bs -> Uint8.of_bytes_little_endian bs 0
let read_u2 ic = read_bytes ic 2 |> fun bs -> Uint16.of_bytes_little_endian bs 0
let read_u4 ic = read_bytes ic 4 |> fun bs -> Uint32.of_bytes_little_endian bs 0
