open Uint

type t =
  { access_flags : U16.t
  ; name_index : U16.t
  ; descriptor_index : U16.t
  ; attributes_count : U16.t
  ; attributes : Attribute_info.t array
  }

let to_string _ = ""
let read _ic = ()
