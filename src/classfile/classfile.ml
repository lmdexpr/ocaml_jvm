open Ubytes.Reader
module Cp_info = Cp_info
module Attribute_info = Attribute_info
module Field_info = Field_info
module Method_info = Method_info

type t =
  { magic : U32.t
  ; minor_version : U16.t
  ; major_version : U16.t
  ; constant_pool_count : U16.t
  ; constant_pool : Cp_info.t array
  ; access_flags : U16.t
  ; this_class : U16.t
  ; super_class : U16.t
  ; interfaces_count : U16.t
  ; interfaces : U16.t array
  ; fields_count : U16.t
  ; fields : Field_info.t array
  ; methods_count : U16.t
  ; methods : Method_info.t array
  ; attributes_count : U16.t
  ; attributes : Attribute_info.t array
  }

let read ic =
  let open Result_ext.Ops in
  let magic = U32.read ic in
  let minor_version = U16.read ic in
  let major_version = U16.read ic in
  let constant_pool_count = U16.read ic in
  let constant_pool = U16.to_int constant_pool_count - 1 |> Cp_info.read ic in
  let access_flags = U16.read ic in
  let this_class = U16.read ic in
  let super_class = U16.read ic in
  let interfaces_count = U16.read ic in
  (* stub *)
  let interfaces =
    Array.init (U16.to_int interfaces_count) (fun _ -> U16.read ic)
  in
  let fields_count = U16.read ic in
  (* stub *)
  (* Array.init (U16.to_int fields_count) (fun _ -> Field_info.read ic)*)
  let fields = [||] in
  let methods_count = U16.read ic in
  let n = U16.to_int methods_count in
  let* methods =
    Result_ext.n_bind n @@ fun _ -> Method_info.read ic constant_pool
  in
  let attributes_count = U16.read ic in
  let n = U16.to_int attributes_count in
  let* attributes =
    Result_ext.n_bind n @@ fun _ -> Attribute_info.read ic constant_pool
  in
  Result.ok
    { magic
    ; minor_version
    ; major_version
    ; constant_pool_count
    ; constant_pool
    ; access_flags
    ; this_class
    ; super_class
    ; interfaces_count
    ; interfaces
    ; fields_count
    ; fields
    ; methods_count
    ; methods
    ; attributes_count
    ; attributes
    }

let rec entry_point ?(entry_point_name = "main") :
    Method_info.t list -> Method_info.t = function
  | hd :: tl ->
    if hd.name = entry_point_name then hd else entry_point ~entry_point_name tl
  | _ -> invalid_arg "not found entry_point"

let entry_point class_file = Array.to_list class_file.methods |> entry_point

module Debug = struct
  open Printf
  include Debug

  let print_classfile cf =
    printf "magic : %s\n" (U32.to_string_hex cf.magic);
    printf "minor_version : %s\n" (U16.to_string_hex cf.minor_version);
    printf "major_version : %s\n" (U16.to_string_hex cf.major_version);
    printf "constant_pool_count : %s\n"
      (U16.to_string_hex cf.constant_pool_count);
    printf "constant_pool : %s\n"
      (array_to_debug_string cf.constant_pool Cp_info.to_string);
    printf "access_flags : %s\n" (U16.to_string_hex cf.access_flags);
    printf "this_class : %s\n" (U16.to_string_hex cf.this_class);
    printf "super_class : %s\n" (U16.to_string_hex cf.super_class);
    printf "interfaces_count : %d\n" (U16.to_int cf.interfaces_count);
    printf "interfaces : %s\n"
      (array_to_debug_string cf.interfaces U16.to_string);
    printf "fields_count : %d\n" (U16.to_int cf.fields_count);
    printf "fields : %s\n"
      (array_to_debug_string cf.fields Field_info.to_string);
    printf "methods_count : %d\n" (U16.to_int cf.methods_count);
    printf "methods : %s\n"
      (array_to_debug_string cf.methods (Method_info.to_string ~prefix:"  "));
    printf "attributes_count : %d\n" (U16.to_int cf.attributes_count);
    printf "attributes : %s\n"
      (array_to_debug_string cf.attributes
         (Attribute_info.to_string ~prefix:"  "))
end
