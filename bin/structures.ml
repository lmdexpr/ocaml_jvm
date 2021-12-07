open Stdint

type c_fieldref_v            = { class_index: uint16; name_and_type_index: uint16 }
type c_methodref_v           = { class_index: uint16; name_and_type_index: uint16 }
type c_interface_methodref_v = { class_index: uint16; name_and_type_index: uint16 }
type c_long_v                = { high_bytes: uint32; low_bytes: uint32 }
type c_double_v              = { high_bytes: uint32; low_bytes: uint32 }
type c_name_and_type_v       = { name_index: uint16; descriptor_index: uint16 }
type c_utf8_v                = { length: uint16; byte_array: uint8 array }
type c_method_handle_v       = { reference_kind: uint8; reference_index: uint16 }
type c_method_type_v         = { descriptor_index: uint16 }
type c_dynamic_v             = { bootstrap_method_attr_index: uint16; name_and_type_index: uint16 }
type c_invoke_dynamic_v      = { bootstrap_method_attr_index: uint16; name_and_type_index: uint16 }

type cp_info =
  | C_dummy
  | C_class               of uint16
  | C_fieldref            of c_fieldref_v
  | C_methodref           of c_methodref_v
  | C_interface_methodref of c_interface_methodref_v
  | C_string              of uint16
  | C_integer             of uint32
  | C_float               of uint32
  | C_long                of c_long_v
  | C_double              of c_double_v
  | C_name_and_type       of c_name_and_type_v
  | C_utf8                of c_utf8_v
  | C_method_handle       of c_method_handle_v
  | C_method_type         of c_method_type_v
  | C_dynamic             of c_dynamic_v
  | C_invoke_dynamic      of c_invoke_dynamic_v
  | C_module              of uint16
  | C_package             of uint16

type attribute_info = {
  attribute_name_index: uint16;
  attribute_length: uint32;
  info: uint8 array;
}

type field_info = {
  access_flags: uint16;
  name_index: uint16;
  descriptor_index: uint16;
  attributes_count: uint16;
  attributes: attribute_info array;
}

type method_info = {
  access_flags: uint16;
  name_index: uint16;
  descriptor_index: uint16;
  attributes_count: uint16;
  attributes: attribute_info array;
}

type class_file = {
  magic: uint32;
  minor_version: uint16;
  major_version: uint16;
  constant_pool_count: uint16;
  constant_pool: cp_info array;
  access_flags: uint16;
  this_class: uint16;
  super_class: uint16;
  interfaces_count: uint16;
  interfaces: uint16 array;
  fields_count: uint16;
  fields: field_info array;
  methods_count: uint16;
  methods: method_info array;
  attributes_count: uint16;
  attributes: attribute_info array;
}
