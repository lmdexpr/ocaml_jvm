module Try = Try

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
