open Stdint

(* type *)
type t

type t_fieldref =
  { class_index : uint16
  ; name_and_type_index : uint16
  }

type t_methodref =
  { class_index : uint16
  ; name_and_type_index : uint16
  }

type t_interface_methodref =
  { class_index : uint16
  ; name_and_type_index : uint16
  }

type t_long =
  { high_bytes : uint32
  ; low_bytes : uint32
  }

type t_double =
  { high_bytes : uint32
  ; low_bytes : uint32
  }

type t_name_and_type =
  { name_index : uint16
  ; descriptor_index : uint16
  }

type t_utf8 =
  { length : uint16
  ; byte_array : uint8 array
  }

type t_method_handle =
  { reference_kind : uint8
  ; reference_index : uint16
  }

type t_method_type = { descriptor_index : uint16 }

type t_dynamic =
  { bootstrap_method_attr_index : uint16
  ; name_and_type_index : uint16
  }

type t_invoke_dynamic =
  { bootstrap_method_attr_index : uint16
  ; name_and_type_index : uint16
  }

(* to_string *)
val utf8_to_string : t_utf8 -> string
val unsafe_utf8_to_string : t -> string
val to_string : t -> string

(* unwrap *)
val unwrap_class : t -> uint16 option
val unwrap_fieldref : t -> t_fieldref option
val unwrap_methodref : t -> t_methodref option
val unwrap_interface_methodref : t -> t_interface_methodref option
val unwrap_string : t -> uint16 option
val unwrap_integer : t -> uint32 option
val unwrap_float : t -> uint32 option
val unwrap_long : t -> t_long option
val unwrap_double : t -> t_double option
val unwrap_name_and_type : t -> t_name_and_type option
val unwrap_utf8 : t -> t_utf8 option
val unwrap_method_handle : t -> t_method_handle option
val unwrap_method_type : t -> t_method_type option
val unwrap_dynamic : t -> t_dynamic option
val unwrap_invoke_dynamic : t -> t_invoke_dynamic option
val unwrap_module : t -> uint16 option
val unwrap_package : t -> uint16 option

(* reader *)
val read_fieldref : in_channel -> t_fieldref
val read_methodref : in_channel -> t_methodref
val read_interface_methodref : in_channel -> t_interface_methodref
val read_long : in_channel -> t_long
val read_double : in_channel -> t_double
val read_name_and_type : in_channel -> t_name_and_type
val read_utf8 : in_channel -> t_utf8
val read_method_handle : in_channel -> t_method_handle
val read_method_type : in_channel -> t_method_type
val read_dynamic : in_channel -> t_dynamic
val read_invoke_dynamic : in_channel -> t_invoke_dynamic
val read : in_channel -> int -> t array
