open Stdint

type t =
  { access_flags : uint16
  ; name_index : uint16
  ; descriptor_index : uint16
  ; attributes_count : uint16
  ; attributes : Attribute_info.t array
  }

let to_string _ = ""
let read _ic = ()
