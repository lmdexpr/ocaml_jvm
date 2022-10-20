open Result
open Printf
open Uint
open Utils.Reader

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

type t =
  | Class of U16.t
  | Fieldref of t_fieldref
  | Methodref of t_methodref
  | Interface_mehotdref of t_interface_methodref
  | String of U16.t
  | Integer of U32.t
  | Float of U32.t
  | Long of t_long
  | Double of t_double
  | Name_and_type of t_name_and_type
  | Utf8 of t_utf8
  | Method_handle of t_method_handle
  | Method_type of t_method_type
  | Dynamic of t_dynamic
  | Invoke_dynamic of t_invoke_dynamic
  | Module of U16.t
  | Package of U16.t

let unwrap_class = function
  | Class v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_class"

let unwrap_fieldref = function
  | Fieldref v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_fieldref"

let unwrap_methodref = function
  | Methodref v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_methodref"

let unwrap_interface_methodref = function
  | Interface_mehotdref v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_interface_methodref"

let unwrap_string = function
  | String v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_string"

let unwrap_integer = function
  | Integer v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_integer"

let unwrap_float = function
  | Float v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_float"

let unwrap_long = function
  | Long v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_long"

let unwrap_double = function
  | Double v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_double"

let unwrap_name_and_type = function
  | Name_and_type v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_name_and_type"

let unwrap_utf8 = function
  | Utf8 v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_utf8"

let unwrap_method_handle = function
  | Method_handle v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_method_handle"

let unwrap_method_type = function
  | Method_type v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_method_type"

let unwrap_dynamic = function
  | Dynamic v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_dynamic"

let unwrap_invoke_dynamic = function
  | Invoke_dynamic v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_invoke_dynamic"

let unwrap_module = function
  | Module v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_module"

let unwrap_package = function
  | Package v -> ok v
  | _ -> error @@ Invalid_argument "unwrap_package"

(* todo : handle utf-8 *)
let utf8_to_string v =
  Array.fold_left
    (fun acc byte -> acc ^ (U8.to_int byte |> Char.chr |> Char.escaped))
    "" v.byte_array

let to_string = function
  | Class v -> "class " ^ U16.to_string v
  | Fieldref v ->
    sprintf "fieldref %s %s"
      (U16.to_string v.class_index)
      (U16.to_string v.name_and_type_index)
  | Methodref v ->
    sprintf "methodref %s %s"
      (U16.to_string v.class_index)
      (U16.to_string v.name_and_type_index)
  | Interface_mehotdref _ -> "interface_methodref"
  | String v -> "string " ^ U16.to_string v
  | Integer _ -> "integer"
  | Float _ -> "float"
  | Long _ -> "long"
  | Double _ -> "double"
  | Name_and_type v ->
    sprintf "name_and_type %s %s"
      (U16.to_string v.name_index)
      (U16.to_string v.descriptor_index)
  | Utf8 v -> sprintf "utf8 %d %s" (U16.to_int v.length) (utf8_to_string v)
  | Method_handle _ -> "method_handle"
  | Method_type _ -> "method_type"
  | Dynamic _ -> "dynamic"
  | Invoke_dynamic _ -> "invoke_dynamic"
  | Module _ -> "module"
  | Package _ -> "package"

(* reader *)
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
  let n = U16.to_int len in
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
