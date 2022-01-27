open Printf
open Stdint
open Reader

module Cp_info = struct
  type t_fieldref = { class_index : uint16; name_and_type_index : uint16 }
  type t_methodref = { class_index : uint16; name_and_type_index : uint16 }

  type t_interface_methodref = {
    class_index : uint16;
    name_and_type_index : uint16;
  }

  type t_long = { high_bytes : uint32; low_bytes : uint32 }
  type t_double = { high_bytes : uint32; low_bytes : uint32 }
  type t_name_and_type = { name_index : uint16; descriptor_index : uint16 }
  type t_utf8 = { length : uint16; byte_array : uint8 array }
  type t_method_handle = { reference_kind : uint8; reference_index : uint16 }
  type t_method_type = { descriptor_index : uint16 }

  type t_dynamic = {
    bootstrap_method_attr_index : uint16;
    name_and_type_index : uint16;
  }

  type t_invoke_dynamic = {
    bootstrap_method_attr_index : uint16;
    name_and_type_index : uint16;
  }

  type t =
    | Dummy
    | Class of uint16
    | Fieldref of t_fieldref
    | Methodref of t_methodref
    | Interface_mehotdref of t_interface_methodref
    | String of uint16
    | Integer of uint32
    | Float of uint32
    | Long of t_long
    | Double of t_double
    | Name_and_type of t_name_and_type
    | Utf8 of t_utf8
    | Method_handle of t_method_handle
    | Method_type of t_method_type
    | Dynamic of t_dynamic
    | Invoke_dynamic of t_invoke_dynamic
    | Module of uint16
    | Package of uint16

  let read_fieldref ic : t_fieldref =
    let class_index = read_u2 ic in
    let name_and_type_index = read_u2 ic in
    { class_index; name_and_type_index }

  let read_methodref ic : t_methodref =
    let class_index = read_u2 ic in
    let name_and_type_index = read_u2 ic in
    { class_index; name_and_type_index }

  let read_interface_methodref ic : t_interface_methodref =
    let class_index = read_u2 ic in
    let name_and_type_index = read_u2 ic in
    { class_index; name_and_type_index }

  let read_long ic : t_long =
    let high_bytes = read_u4 ic in
    let low_bytes = read_u4 ic in
    { high_bytes; low_bytes }

  let read_double ic : t_double =
    let high_bytes = read_u4 ic in
    let low_bytes = read_u4 ic in
    { high_bytes; low_bytes }

  let read_name_and_type ic : t_name_and_type =
    let name_index = read_u2 ic in
    let descriptor_index = read_u2 ic in
    { name_index; descriptor_index }

  let read_utf8 ic : t_utf8 =
    let len = read_u2 ic in
    let n = Uint16.to_int len in
    { length = len; byte_array = Array.init n (fun _ -> read_u1 ic) }

  let read_method_handle ic : t_method_handle =
    let reference_kind = read_u1 ic in
    let reference_index = read_u2 ic in
    { reference_kind; reference_index }

  let read_method_type ic : t_method_type = { descriptor_index = read_u2 ic }

  let read_dynamic ic : t_dynamic =
    let bootstrap_method_attr_index = read_u2 ic in
    let name_and_type_index = read_u2 ic in
    { bootstrap_method_attr_index; name_and_type_index }

  let read_invoke_dynamic ic : t_invoke_dynamic =
    let bootstrap_method_attr_index = read_u2 ic in
    let name_and_type_index = read_u2 ic in
    { bootstrap_method_attr_index; name_and_type_index }

  exception Illegal_constant_pool_tag

  let read ic n : t array =
    let f _ =
      match read_byte ic |> Option.get |> int_of_char with
      | 7 -> Class (read_u2 ic)
      | 9 -> Fieldref (read_fieldref ic)
      | 10 -> Methodref (read_methodref ic)
      | 11 -> Interface_mehotdref (read_interface_methodref ic)
      | 8 -> String (read_u2 ic)
      | 3 -> Integer (read_u4 ic)
      | 4 -> Float (read_u4 ic)
      | 5 -> Long (read_long ic)
      | 6 -> Double (read_double ic)
      | 12 -> Name_and_type (read_name_and_type ic)
      | 1 -> Utf8 (read_utf8 ic)
      | 15 -> Method_handle (read_method_handle ic)
      | 16 -> Method_type (read_method_type ic)
      | 17 -> Dynamic (read_dynamic ic)
      | 18 -> Invoke_dynamic (read_invoke_dynamic ic)
      | 19 -> Module (read_u2 ic)
      | 20 -> Package (read_u2 ic)
      | _ -> raise Illegal_constant_pool_tag
    in
    Array.init n f

  (* todo : handle utf-8 *)
  let utf8_to_string = function
    | Utf8 v ->
        Array.fold_left
          (fun acc byte ->
            acc ^ (Uint8.to_int byte |> Char.chr |> Char.escaped))
          "" v.byte_array
    | _ -> raise @@ Invalid_argument "not utf8 in cp"

  let to_debug_string = function
    | Dummy -> "dummy"
    | Class v -> "class " ^ Uint16.to_string v
    | Fieldref v ->
        sprintf "fieldref %s %s"
          (Uint16.to_string v.class_index)
          (Uint16.to_string v.name_and_type_index)
    | Methodref v ->
        sprintf "methodref %s %s"
          (Uint16.to_string v.class_index)
          (Uint16.to_string v.name_and_type_index)
    | Interface_mehotdref _ -> "interface_methodref"
    | String v -> "string " ^ Uint16.to_string v
    | Integer _ -> "integer"
    | Float _ -> "float"
    | Long _ -> "long"
    | Double _ -> "double"
    | Name_and_type v ->
        sprintf "name_and_type %s %s"
          (Uint16.to_string v.name_index)
          (Uint16.to_string v.descriptor_index)
    | Utf8 v ->
        sprintf "utf8 %d %s" (Uint16.to_int v.length) (utf8_to_string @@ Utf8 v)
    | Method_handle _ -> "method_handle"
    | Method_type _ -> "method_type"
    | Dynamic _ -> "dynamic"
    | Invoke_dynamic _ -> "invoke_dynamic"
    | Module _ -> "module"
    | Package _ -> "package"
end

module Attribute_info = struct
  type t_exception = {
    start_pc : uint16;
    end_pc : uint16;
    handler_pc : uint16;
    catch_type : uint16;
  }

  type t_line_number = { start_pc : uint16; line_number : uint16 }

  type t_code = {
    max_stack : uint16;
    max_locals : uint16;
    code : uint8 array;
    exception_table : t_exception array;
  }

  type t =
    | Code of t_code * t array
    | Line_number_table of t_line_number array
    | Source_file of uint16
    | Not_implemented

  let read_attribute_name ic cp =
    let attribute_name_index = read_u2 ic in
    let _attribute_length = read_u4 ic in
    cp.(Uint16.to_int attribute_name_index - 1) |> Cp_info.utf8_to_string

  let rec read ic cp =
    match read_attribute_name ic cp with
    | "Code" ->
        let max_stack = read_u2 ic in
        let max_locals = read_u2 ic in
        let code_length = read_u4 ic in
        let n = Uint32.to_int code_length in
        let code = Array.init n (fun _ -> read_u1 ic) in
        let exception_table_length = read_u2 ic in
        let n = Uint16.to_int exception_table_length in
        let f _ =
          let start_pc = read_u2 ic in
          let end_pc = read_u2 ic in
          let handler_pc = read_u2 ic in
          let catch_type = read_u2 ic in
          { start_pc; end_pc; handler_pc; catch_type }
        in
        let exception_table = Array.init n f in
        let attributes_count = read_u2 ic in
        let attributes_count = Uint16.to_int attributes_count in
        let attributes = Array.init attributes_count (fun _ -> read ic cp) in
        let code = { max_stack; max_locals; code; exception_table } in
        Code (code, attributes)
    | "LineNumberTable" ->
        let line_number_table_length = read_u2 ic in
        let n = Uint16.to_int line_number_table_length in
        let f _ =
          let start_pc = read_u2 ic in
          let line_number = read_u2 ic in
          { start_pc; line_number }
        in
        Line_number_table (Array.init n f)
    | "SourceFile" -> Source_file (read_u2 ic)
    | _ -> Not_implemented

  let rec to_debug_string ?(prefix = "") = function
    | Code (v, attributes) ->
        let s = "Code : {\n" in
        let s =
          s ^ prefix ^ "  max_stack: " ^ Uint16.to_string v.max_stack ^ ";\n"
        in
        let s =
          s ^ prefix ^ "  max_locals: " ^ Uint16.to_string v.max_locals ^ ";\n"
        in
        let more_nest = prefix ^ "  " in
        let s =
          s ^ prefix ^ "  code: "
          ^ Util.array_to_string ~prefix:more_nest v.code Uint8.to_string
          ^ ";\n"
        in
        let e_to_s (e : t_exception) =
          "{ start_pc: "
          ^ Uint16.to_string e.start_pc
          ^ "; end_pc: " ^ Uint16.to_string e.end_pc ^ "; handler_pc: "
          ^ Uint16.to_string e.handler_pc
          ^ "; catch_type: "
          ^ Uint16.to_string e.catch_type
          ^ ";}"
        in
        let s =
          s ^ prefix ^ "  exception_table: "
          ^ Util.array_to_string ~prefix:more_nest v.exception_table e_to_s
          ^ ";\n"
        in
        let s =
          s ^ prefix ^ "  attributes: "
          ^ Util.array_to_string ~prefix:more_nest attributes
              (to_debug_string ~prefix:(more_nest ^ "  "))
          ^ ";\n"
        in
        s ^ prefix ^ "}"
    | Line_number_table v ->
        "LineNumberTable : "
        ^ Util.array_to_string ~prefix v (fun e ->
              "{ start_pc: "
              ^ Uint16.to_string e.start_pc
              ^ "; line_number: "
              ^ Uint16.to_string e.line_number
              ^ " }")
    | Source_file v -> "SourceFile : { " ^ Uint16.to_string v ^ " }"
    | Not_implemented -> "NOT IMPLEMENTED"
end

module Field_info = struct
  type t = {
    access_flags : uint16;
    name_index : uint16;
    descriptor_index : uint16;
    attributes_count : uint16;
    attributes : Attribute_info.t array;
  }

  let to_string _ = ""
end

module Method_info = struct
  type t = {
    access_flags : uint16;
    name_index : string;
    descriptor_index : string;
    attributes : Attribute_info.t array;
  }

  let read ic cp =
    let access_flags = read_u2 ic in
    let name_index = read_u2 ic in
    let name_index =
      cp.(Uint16.to_int name_index - 1) |> Cp_info.utf8_to_string
    in
    let descriptor_index = read_u2 ic in
    let descriptor_index =
      cp.(Uint16.to_int descriptor_index - 1) |> Cp_info.utf8_to_string
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
      ^ Util.array_to_string ~prefix:(prefix ^ "  ") mi.attributes
          (Attribute_info.to_debug_string ~prefix:(prefix ^ "    "))
      ^ ";\n"
    in
    s ^ prefix ^ "}"
end

type t = {
  magic : uint32;
  minor_version : uint16;
  major_version : uint16;
  constant_pool_count : uint16;
  constant_pool : Cp_info.t array;
  access_flags : uint16;
  this_class : uint16;
  super_class : uint16;
  interfaces_count : uint16;
  interfaces : uint16 array;
  fields_count : uint16;
  fields : Field_info.t array;
  methods_count : uint16;
  methods : Method_info.t array;
  attributes_count : uint16;
  attributes : Attribute_info.t array;
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
  {
    magic;
    minor_version;
    major_version;
    constant_pool_count;
    constant_pool;
    access_flags;
    this_class;
    super_class;
    interfaces_count;
    interfaces;
    fields_count;
    fields;
    methods_count;
    methods;
    attributes_count;
    attributes;
  }

let debug_print cf =
  printf "magic : %s\n" (Uint32.to_string_hex cf.magic);
  printf "minor_version : %s\n" (Uint16.to_string_hex cf.minor_version);
  printf "major_version : %s\n" (Uint16.to_string_hex cf.major_version);
  printf "constant_pool_count : %s\n"
    (Uint16.to_string_hex cf.constant_pool_count);
  printf "constant_pool : %s\n"
    (Util.array_to_string cf.constant_pool Cp_info.to_debug_string);
  printf "access_flags : %s\n" (Uint16.to_string_hex cf.access_flags);
  printf "this_class : %s\n" (Uint16.to_string_hex cf.this_class);
  printf "super_class : %s\n" (Uint16.to_string_hex cf.super_class);
  printf "interfaces_count : %d\n" (Uint16.to_int cf.interfaces_count);
  printf "interfaces : %s\n"
    (Util.array_to_string cf.interfaces Uint16.to_string);
  printf "fields_count : %d\n" (Uint16.to_int cf.fields_count);
  printf "fields : %s\n" (Util.array_to_string cf.fields Field_info.to_string);
  printf "methods_count : %d\n" (Uint16.to_int cf.methods_count);
  printf "methods : %s\n"
    (Util.array_to_string cf.methods (Method_info.to_debug_string ~prefix:"  "));
  printf "attributes_count : %d\n" (Uint16.to_int cf.attributes_count);
  printf "attributes : %s\n"
    (Util.array_to_string cf.attributes
       (Attribute_info.to_debug_string ~prefix:"  "))
