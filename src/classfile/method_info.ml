open Stdint
open Utils.Reader

type t =
  { access_flags : uint16
  ; name_index : string
  ; descriptor_index : string
  ; attributes : Attribute_info.t array
  }

let read ic cp =
  let access_flags = read_u2 ic in
  let name_index = read_u2 ic in
  let name_index =
    cp.(Uint16.to_int name_index - 1) |> Cp_info.unsafe_utf8_to_string
  in
  let descriptor_index = read_u2 ic in
  let descriptor_index =
    cp.(Uint16.to_int descriptor_index - 1) |> Cp_info.unsafe_utf8_to_string
  in
  let attributes_count = read_u2 ic in
  let attributes_count = Uint16.to_int attributes_count in
  let attributes =
    Array.init attributes_count (fun _ -> Attribute_info.read ic cp)
  in
  { access_flags; name_index; descriptor_index; attributes }

let to_debug_string ?(prefix = "") mi =
  let s = "{\n" in
  let s =
    s ^ prefix ^ "  access_flags: " ^ Uint16.to_string mi.access_flags ^ ";\n"
  in
  let s = s ^ prefix ^ "  name_index: " ^ mi.name_index ^ ";\n" in
  let s = s ^ prefix ^ "  descriptor_index: " ^ mi.descriptor_index ^ ";\n" in
  let s =
    s ^ prefix ^ "  attributes: "
    ^ Utils.array_to_string ~prefix:(prefix ^ "  ") mi.attributes
        (Attribute_info.to_debug_string ~prefix:(prefix ^ "    "))
    ^ ";\n"
  in
  s ^ prefix ^ "}"
