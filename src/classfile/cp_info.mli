open Ubytes.Uint

(* type *)
type t

type t_fieldref =
  { class_index : U16.t
  ; name_and_type_index : U16.t
  }

type t_methodref =
  { class_index : U16.t
  ; name_and_type_index : U16.t
  }

type t_interface_methodref =
  { class_index : U16.t
  ; name_and_type_index : U16.t
  }

type t_long =
  { high_bytes : U32.t
  ; low_bytes : U32.t
  }

type t_double =
  { high_bytes : U32.t
  ; low_bytes : U32.t
  }

type t_name_and_type =
  { name_index : U16.t
  ; descriptor_index : U16.t
  }

type t_utf8 =
  { length : U16.t
  ; byte_array : U8.t array
  }

type t_method_handle =
  { reference_kind : U8.t
  ; reference_index : U16.t
  }

type t_method_type = { descriptor_index : U16.t }

type t_dynamic =
  { bootstrap_method_attr_index : U16.t
  ; name_and_type_index : U16.t
  }

type t_invoke_dynamic =
  { bootstrap_method_attr_index : U16.t
  ; name_and_type_index : U16.t
  }

(* to_string *)
val utf8_to_string : t_utf8 -> string
val to_string : t -> string

(* unwrap *)
val unwrap_class : t -> (U16.t, exn) result
val unwrap_fieldref : t -> (t_fieldref, exn) result
val unwrap_methodref : t -> (t_methodref, exn) result
val unwrap_interface_methodref : t -> (t_interface_methodref, exn) result
val unwrap_string : t -> (U16.t, exn) result
val unwrap_integer : t -> (U32.t, exn) result
val unwrap_float : t -> (U32.t, exn) result
val unwrap_long : t -> (t_long, exn) result
val unwrap_double : t -> (t_double, exn) result
val unwrap_name_and_type : t -> (t_name_and_type, exn) result
val unwrap_utf8 : t -> (t_utf8, exn) result
val unwrap_method_handle : t -> (t_method_handle, exn) result
val unwrap_method_type : t -> (t_method_type, exn) result
val unwrap_dynamic : t -> (t_dynamic, exn) result
val unwrap_invoke_dynamic : t -> (t_invoke_dynamic, exn) result
val unwrap_module : t -> (U16.t, exn) result
val unwrap_package : t -> (U16.t, exn) result

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
