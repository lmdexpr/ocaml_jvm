open Utils.Reader.Uint

type t =
  { access_flags : U16.t
  ; name_index : string
  ; descriptor_index : string
  ; attributes : Attribute_info.t array
  }

let read ic cp =
  let access_flags = U16.read ic in
  let name_index = U16.read ic in
  let name_index =
    cp.(U16.to_int name_index - 1)
    |> Cp_info.unwrap_utf8
    |> Result.fold ~ok:Cp_info.utf8_to_string ~error:raise
  in
  let descriptor_index = U16.read ic in
  let descriptor_index =
    cp.(U16.to_int descriptor_index - 1)
    |> Cp_info.unwrap_utf8
    |> Result.fold ~ok:Cp_info.utf8_to_string ~error:raise
  in
  let attributes_count = U16.read ic in
  let attributes_count = U16.to_int attributes_count in
  let attributes =
    Array.init attributes_count (fun _ -> Attribute_info.read ic cp)
  in
  { access_flags; name_index; descriptor_index; attributes }
