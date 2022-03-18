open Printf
open Stdint
open Utils.Reader
module Cp_info = Cp_info
module Attribute_info = Attribute_info
module Field_info = Field_info
module Method_info = Method_info

type t =
  { magic : uint32
  ; minor_version : uint16
  ; major_version : uint16
  ; constant_pool_count : uint16
  ; constant_pool : Cp_info.t array
  ; access_flags : uint16
  ; this_class : uint16
  ; super_class : uint16
  ; interfaces_count : uint16
  ; interfaces : uint16 array
  ; fields_count : uint16
  ; fields : Field_info.t array
  ; methods_count : uint16
  ; methods : Method_info.t array
  ; attributes_count : uint16
  ; attributes : Attribute_info.t array
  }

let read ic : t =
  let magic = read_u4 ic in
  let minor_version = read_u2 ic in
  let major_version = read_u2 ic in
  let constant_pool_count = read_u2 ic in
  let constant_pool =
    Uint16.to_int constant_pool_count - 1 |> Cp_info.read ic
  in
  let access_flags = read_u2 ic in
  let this_class = read_u2 ic in
  let super_class = read_u2 ic in
  let interfaces_count = read_u2 ic in
  let interfaces = [||] in
  (* stub *)
  let fields_count = read_u2 ic in
  let fields = [||] in
  (* stub *)
  let methods_count = read_u2 ic in
  let methods =
    Array.init (Uint16.to_int methods_count) (fun _ ->
        Method_info.read ic constant_pool)
  in
  let attributes_count = read_u2 ic in
  let attributes =
    Array.init (Uint16.to_int attributes_count) (fun _ ->
        Attribute_info.read ic constant_pool)
  in
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

let debug_print cf =
  printf "magic : %s\n" (Uint32.to_string_hex cf.magic);
  printf "minor_version : %s\n" (Uint16.to_string_hex cf.minor_version);
  printf "major_version : %s\n" (Uint16.to_string_hex cf.major_version);
  printf "constant_pool_count : %s\n"
    (Uint16.to_string_hex cf.constant_pool_count);
  printf "constant_pool : %s\n"
    (Utils.array_to_string cf.constant_pool Cp_info.to_string);
  printf "access_flags : %s\n" (Uint16.to_string_hex cf.access_flags);
  printf "this_class : %s\n" (Uint16.to_string_hex cf.this_class);
  printf "super_class : %s\n" (Uint16.to_string_hex cf.super_class);
  printf "interfaces_count : %d\n" (Uint16.to_int cf.interfaces_count);
  printf "interfaces : %s\n"
    (Utils.array_to_string cf.interfaces Uint16.to_string);
  printf "fields_count : %d\n" (Uint16.to_int cf.fields_count);
  printf "fields : %s\n" (Utils.array_to_string cf.fields Field_info.to_string);
  printf "methods_count : %d\n" (Uint16.to_int cf.methods_count);
  printf "methods : %s\n"
    (Utils.array_to_string cf.methods
       (Method_info.to_debug_string ~prefix:"  "));
  printf "attributes_count : %d\n" (Uint16.to_int cf.attributes_count);
  printf "attributes : %s\n"
    (Utils.array_to_string cf.attributes
       (Attribute_info.to_debug_string ~prefix:"  "))
