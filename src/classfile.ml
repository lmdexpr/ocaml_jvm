open Printf
open Stdint

open Reader

module Cp_info = struct
  type t_fieldref            = { class_index: uint16; name_and_type_index: uint16 }
  type t_methodref           = { class_index: uint16; name_and_type_index: uint16 }
  type t_interface_methodref = { class_index: uint16; name_and_type_index: uint16 }
  type t_long                = { high_bytes: uint32; low_bytes: uint32 }
  type t_double              = { high_bytes: uint32; low_bytes: uint32 }
  type t_name_and_type       = { name_index: uint16; descriptor_index: uint16 }
  type t_utf8                = { length: uint16; byte_array: uint8 array }
  type t_method_handle       = { reference_kind: uint8; reference_index: uint16 }
  type t_method_type         = { descriptor_index: uint16 }
  type t_dynamic             = { bootstrap_method_attr_index: uint16; name_and_type_index: uint16 }
  type t_invoke_dynamic      = { bootstrap_method_attr_index: uint16; name_and_type_index: uint16 }

  type t =
    | Dummy
    | Class               of uint16
    | Fieldref            of t_fieldref
    | Methodref           of t_methodref
    | Interface_mehotdref of t_interface_methodref
    | String              of uint16
    | Integer             of uint32
    | Float               of uint32
    | Long                of t_long
    | Double              of t_double
    | Name_and_type       of t_name_and_type
    | Utf8                of t_utf8
    | Method_handle       of t_method_handle
    | Method_type         of t_method_type
    | Dynamic             of t_dynamic
    | Invoke_dynamic      of t_invoke_dynamic
    | Module              of uint16
    | Package             of uint16

  let read_fieldref ic  : t_fieldref  = { class_index = read_u2 ic; name_and_type_index = read_u2 ic }
  let read_methodref ic : t_methodref = { class_index = read_u2 ic; name_and_type_index = read_u2 ic }
  let read_interface_methodref ic : t_interface_methodref = { class_index = read_u2 ic; name_and_type_index = read_u2 ic }

  let read_long   ic : t_long   = { high_bytes = read_u4 ic; low_bytes = read_u4 ic }
  let read_double ic : t_double = { high_bytes = read_u4 ic; low_bytes = read_u4 ic }

  let read_name_and_type ic : t_name_and_type = { name_index = read_u2 ic; descriptor_index = read_u2 ic }

  let read_utf8 ic : t_utf8 = 
    let len = read_u2 ic in
    let n   = Uint16.to_int len in
    { length = len; byte_array = Util.array_fill ~n ~init:Uint8.zero ~f:(fun _ -> read_u1 ic) }

  let read_method_handle ic : t_method_handle = { reference_kind = read_u1 ic ; reference_index = read_u2 ic }
  let read_method_type   ic : t_method_type   = { descriptor_index = read_u2 ic }

  let read_dynamic ic : t_dynamic =
    { bootstrap_method_attr_index = read_u2 ic; name_and_type_index = read_u2 ic }
  let read_invoke_dynamic ic : t_invoke_dynamic =
    { bootstrap_method_attr_index = read_u2 ic; name_and_type_index = read_u2 ic }

  exception Illegal_constant_pool_tag
  let read ic n : t array =
    let f () =
      match read_byte ic |> Option.get |> int_of_char with
      |  7 -> Class               (read_u2 ic)
      |  9 -> Fieldref            (read_fieldref ic)
      | 10 -> Methodref           (read_methodref ic)
      | 11 -> Interface_mehotdref (read_interface_methodref ic)
      |  8 -> String              (read_u2 ic)
      |  3 -> Integer             (read_u4 ic)
      |  4 -> Float               (read_u4 ic)
      |  5 -> Long                (read_long ic)
      |  6 -> Double              (read_double ic)
      | 12 -> Name_and_type       (read_name_and_type ic)
      |  1 -> Utf8                (read_utf8 ic)
      | 15 -> Method_handle       (read_method_handle ic)
      | 16 -> Method_type         (read_method_type ic)
      | 17 -> Dynamic             (read_dynamic ic)
      | 18 -> Invoke_dynamic      (read_invoke_dynamic ic)
      | 19 -> Module              (read_u2 ic)
      | 20 -> Package             (read_u2 ic)
      | _  -> raise Illegal_constant_pool_tag
    in
      Util.array_fill ~n ~init:Dummy ~f

  let to_string = function
    | Dummy                 -> "dummy"
    | Class               v -> "class " ^ (Uint16.to_string v) 
    | Fieldref            v -> sprintf "fieldref %s %s" (Uint16.to_string v.class_index) (Uint16.to_string v.name_and_type_index)
    | Methodref           v -> sprintf "methodref %s %s" (Uint16.to_string v.class_index) (Uint16.to_string v.name_and_type_index)
    | Interface_mehotdref _ -> "interface_methodref"
    | String              v -> "string " ^ (Uint16.to_string v)
    | Integer             _ -> "integer"
    | Float               _ -> "float"
    | Long                _ -> "long"
    | Double              _ -> "double"
    | Name_and_type       v -> sprintf "name_and_type %s %s" (Uint16.to_string v.name_index) (Uint16.to_string v.descriptor_index)
    (* todo : handle utf-8 *)
    | Utf8                v -> sprintf "utf8 %d %s" (Uint16.to_int v.length) (Array.fold_left (fun s b -> s ^ (Uint8.to_int b |> Char.chr |> Char.escaped)) "" v.byte_array)
    | Method_handle       _ -> "method_handle"
    | Method_type         _ -> "method_type"
    | Dynamic             _ -> "dynamic"
    | Invoke_dynamic      _ -> "invoke_dynamic"
    | Module              _ -> "module"
    | Package             _ -> "package"
end

module Attribute_info = struct 
  type t =
    (*
    | Code
    | Line_number_table
    | Not_implemented
  *)
    {
      attribute_name_index: uint16;
      attribute_length: uint32;
      info: uint8 array;
    }

  let read ic =
    let attribute_name_index = read_u2 ic in
    let attribute_length = read_u4 ic in
    let n    = Uint32.to_int attribute_length in
    let info = Util.array_fill ~n ~init:Uint8.zero ~f:(fun _ -> read_u1 ic) in
    { attribute_name_index; attribute_length; info }

  let to_string _ = ""

  let empty = { attribute_name_index = Uint16.zero; attribute_length = Uint32.zero; info = [| |] }
end

module Field_info = struct
  type t = {
    access_flags: uint16;
    name_index: uint16;
    descriptor_index: uint16;
    attributes_count: uint16;
    attributes: Attribute_info.t array;
  }

  let to_string _ = ""
end

module Method_info = struct
  type t = {
    access_flags: uint16;
    name_index: uint16;
    descriptor_index: uint16;
    attributes_count: uint16;
    attributes: Attribute_info.t array;
  }

  let to_string _ = ""
end

type t = {
  magic: uint32;
  minor_version: uint16;
  major_version: uint16;
  constant_pool_count: uint16;
  constant_pool: Cp_info.t array;
  access_flags: uint16;
  this_class: uint16;
  super_class: uint16;
  interfaces_count: uint16;
  interfaces: uint16 array;
  fields_count: uint16;
  fields: Field_info.t array;
  methods_count: uint16;
  methods: Method_info.t array;
  attributes_count: uint16;
  attributes: Attribute_info.t array;
}

let read ic : t = 
  let magic               = read_u4 ic in
  let minor_version       = read_u2 ic in
  let major_version       = read_u2 ic in
  let constant_pool_count = read_u2 ic in
  let constant_pool       = Uint16.to_int constant_pool_count - 1 |> Cp_info.read ic in
  let access_flags        = read_u2 ic in
  let this_class          = read_u2 ic in
  let super_class         = read_u2 ic in
  let interfaces_count    = read_u2 ic in
  let interfaces          = [| |] in
  let fields_count        = read_u2 ic in
  let fields              = [| |] in
  let methods_count       = read_u2 ic in
  let methods             = [| |] in
  let attributes_count    = read_u2 ic in
  let n                   = 0 (* stub *) in
  let attributes          = Util.array_fill ~n ~init:Attribute_info.empty ~f:(fun _ -> Attribute_info.read ic) in
  {
    magic;
    minor_version; major_version;
    constant_pool_count; constant_pool;
    access_flags;
    this_class; super_class;
    interfaces_count; interfaces;
    fields_count; fields;
    methods_count; methods;
    attributes_count; attributes;
  }

let debug_print cf = 
  begin 
    printf "magic : %s\n" (Uint32.to_string_hex cf.magic);
    printf "minor_version : %s\n" (Uint16.to_string_hex cf.minor_version);
    printf "major_version : %s\n" (Uint16.to_string_hex cf.major_version);
    printf "constant_pool_count : %s\n" (Uint16.to_string_hex cf.constant_pool_count);
    print_endline "constant_pool [";
    cf.constant_pool |> Array.iter (fun c -> "  " ^ (Cp_info.to_string c) |> print_endline);
    print_endline "]";
    printf "access_flags : %s\n" (Uint16.to_string_hex cf.access_flags);
    printf "this_class : %s\n" (Uint16.to_string_hex cf.this_class);
    printf "super_class : %s\n" (Uint16.to_string_hex cf.super_class);
    printf "interfaces_count : %d\n" (Uint16.to_int cf.interfaces_count);
    print_endline "interfaces : [";
    cf.interfaces |> Array.iter (fun i -> "  " ^ (Uint16.to_string i) |> print_endline);
    print_endline "]";
    printf "fields_count : %d\n" (Uint16.to_int cf.fields_count);
    print_endline "fields : [";
    cf.fields |> Array.iter (fun f -> "  " ^ (Field_info.to_string f) |> print_endline);
    print_endline "]";
    printf "methods_count : %d\n" (Uint16.to_int cf.methods_count);
    print_endline "methods [";
    cf.methods |> Array.iter (fun m -> "  " ^ (Method_info.to_string m) |> print_endline);
    print_endline"]";
    printf "attributes_count : %d\n" (Uint16.to_int cf.attributes_count);
    print_endline "attributes [";
    cf.attributes |> Array.iter (fun a -> "  " ^ (Attribute_info.to_string a) |> print_endline);
    print_endline"]";
  end
