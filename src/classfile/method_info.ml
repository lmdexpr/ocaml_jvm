open Ubytes

type t =
  { access_flags : U16.t
  ; name : string
  ; descriptor : string
  ; attributes : Attribute_info.t array
  }

let read ic cp =
  let access_flags = U16.read ic in
  let name_index = (U16.read ic |> U16.to_int) - 1 in
  let name =
    Result.fold ~ok:Cp_info.utf8_to_string ~error:raise
    @@ Cp_info.unwrap_utf8 cp.(name_index)
  in
  let descriptor_index = (U16.read ic |> U16.to_int) - 1 in
  let descriptor =
    Result.fold ~ok:Cp_info.utf8_to_string ~error:raise
    @@ Cp_info.unwrap_utf8 cp.(descriptor_index)
  in
  let attributes_count = U16.read ic in
  let attributes_count = U16.to_int attributes_count in
  let attributes =
    Array.init attributes_count (fun _ -> Attribute_info.read ic cp)
  in
  { access_flags; name; descriptor; attributes }
