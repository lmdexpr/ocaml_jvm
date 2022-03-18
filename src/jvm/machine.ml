open Stdint
open Classfile

module Frame = struct
  type t =
    | Callable of Cp_info.t * Cp_info.t * Cp_info.t
    | String of string

  let to_java_primitive = function
    | String s -> Java_libs.String s
    | _ -> Java_libs.Not_implemented
end

module Runtime_data_area = struct
  type t =
    { (* todo pc_register: Frame.t; *)
      stacks : Frame.t Stack.t
    ; (* todo heap: ???; method_area: ???; *)
      cp : Cp_info.t array (* todo native_method_stacks: ??? Stack.t; *)
    }

  let create cp = { stacks = Stack.create (); cp }
end

type t =
  { rda : Runtime_data_area.t
  ; entry_class : Classfile.t
  }

(* loader *)
let create (class_file : Classfile.t) =
  { rda = Runtime_data_area.create class_file.constant_pool
  ; entry_class = class_file
  }

let entry_point machine = machine.entry_class.methods.(1).attributes.(0)

(* access constant_pool *)
let get_constant machine index = machine.rda.cp.(index - 1)
let get_constant_16 machine byte = get_constant machine @@ Uint16.to_int byte

let get_constant_8 machine byte1 byte2 =
  let byte1 = Uint16.of_uint8 byte1 and byte2 = Uint16.of_uint8 byte2 in
  get_constant_16 machine Uint16.(shift_left byte1 8 + byte2)

(* resolution *)
let field_resolution machine c =
  let get_constant_16 = get_constant_16 machine in
  let open Utils.Try.Ops in
  let* fieldref = Cp_info.unwrap_fieldref c in
  let* class_index =
    get_constant_16 fieldref.class_index |> Cp_info.unwrap_class
  in
  let callee_class = get_constant_16 class_index in
  let* name_and_type =
    get_constant_16 fieldref.name_and_type_index |> Cp_info.unwrap_name_and_type
  in
  let field = get_constant_16 name_and_type.name_index
  and field_type = get_constant_16 name_and_type.descriptor_index in
  Result.ok @@ Frame.Callable (callee_class, field, field_type)

let stack_push machine frame = Stack.push frame machine.rda.stacks
let stack_pop machine n = List.init n (fun _ -> Stack.pop machine.rda.stacks)
