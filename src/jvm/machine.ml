open Stdint
open Classfile

module Frame = struct
  type t = Callable of Cp_info.t * Cp_info.t * Cp_info.t | String of string

  let to_java_primitive = function
    | String s -> Java_libs.String s
    | _ -> Java_libs.Not_implemented
end

module Runtime_data_area = struct
  type t = {
    (* todo pc_register: Frame.t; *)
    stacks : Frame.t Stack.t;
    (* todo heap: ???; method_area: ???; *)
    cp : Cp_info.t array; (* todo native_method_stacks: ??? Stack.t; *)
  }

  let create cp = { stacks = Stack.create (); cp }
end

type t = { rda : Runtime_data_area.t; entry_class : Classfile.t }

(* loader *)
let create (class_file : Classfile.t) =
  {
    rda = Runtime_data_area.create class_file.constant_pool;
    entry_class = class_file;
  }

let entry_point machine = machine.entry_class.methods.(1).attributes.(0)

(* access constant_pool *)
let get_constant machine index = machine.rda.cp.(index - 1)
let get_constant_16 machine byte = get_constant machine @@ Uint16.to_int byte

let get_constant_8 machine byte1 byte2 =
  let byte1 = Uint16.of_uint8 byte1 and byte2 = Uint16.of_uint8 byte2 in
  get_constant_16 machine Uint16.(shift_left byte1 8 + byte2)

(* resolution *)
let field_resolution machine = function
  | Cp_info.Fieldref { class_index; name_and_type_index } ->
    let constant_pool = machine.rda.cp in
    let class_index = Uint16.to_int class_index - 1
    and name_and_type_index = Uint16.to_int name_and_type_index - 1 in
    let callee_class =
      match constant_pool.(class_index) with
      | Cp_info.Class v -> constant_pool.(Uint16.to_int v - 1)
      | info ->
        invalid_arg @@ "for resolution of field :"
        ^ Cp_info.to_debug_string info
    in
    let field, field_type =
      match constant_pool.(name_and_type_index) with
      | Cp_info.Name_and_type { name_index; descriptor_index } ->
        let name_index = Uint16.to_int name_index - 1
        and descriptor_index = Uint16.to_int descriptor_index - 1 in
        (constant_pool.(name_index), constant_pool.(descriptor_index))
      | info ->
        invalid_arg @@ "for resolution of field :"
        ^ Cp_info.to_debug_string info
    in
    Frame.Callable (callee_class, field, field_type)
  | info ->
    invalid_arg @@ "for resolution of field :" ^ Cp_info.to_debug_string info

let stack_push machine frame = Stack.push frame machine.rda.stacks
let stack_pop machine n = List.init n (fun _ -> Stack.pop machine.rda.stacks)
