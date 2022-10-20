open Uint

type t_constant_pool = Classfile.Cp_info.t array

type t =
  { (* todo pc_register: Frame.t; *)
    stack : Frame.t Stack.t
  ; (* todo heap: ???; method_area: ???; *)
    constant_pool : t_constant_pool
        (* todo native_method_stacks: ??? Stack.t; *)
  }

let make (entry_class : Classfile.t) =
  { stack = Stack.create (); constant_pool = entry_class.constant_pool }

(* access constant_pool *)
let get_constant cp index = cp.(index - 1)
let get_constant_16 cp byte = get_constant cp @@ U16.to_int byte

let get_constant_8 cp byte1 byte2 =
  let byte1 = U16.of_u8 byte1 and byte2 = U16.of_u8 byte2 in
  get_constant_16 cp U16.(shift_left byte1 8 + byte2)

(* resolution *)
let field_resolution cp c =
  let open Classfile.Cp_info in
  let get_constant_16 = get_constant_16 cp in
  let open Utils.Try.Ops in
  let* fieldref = unwrap_fieldref c in
  let* class_index = get_constant_16 fieldref.class_index |> unwrap_class in
  let callee_class = get_constant_16 class_index in
  let* name_and_type =
    get_constant_16 fieldref.name_and_type_index |> unwrap_name_and_type
  in
  let field = get_constant_16 name_and_type.name_index
  and field_type = get_constant_16 name_and_type.descriptor_index in
  Result.ok @@ Frame.Callable (callee_class, field, field_type)
