module Frame = struct
  type t = ()
end

module Runtime_data_area = struct 
  type t = {
    (* todo
    pc_register: Frame.t;
    *)
    stacks: frame Stack.t;
    (* todo
    heap: ???;
    method_area: ???;
    *)
    run_time_cp: Classfile.Cp_info.t array;
    (* todo
    native_method_stacks: ??? Stack.t;
    *)
  }

  let create class_file : Runtime_data_area.t =
    let stacks = Stack.create () in
    { stacks; class_file.constant_pool }
end

let run (entry_class: Classfile.t) class_files =
  let rda = Runtime_data_area.create entry_class in
  let rec step = function
    | 0xb2 :: op1 :: op2 :: tl -> Instruction.getstatic op1 op2; step tl
    | 0x12 :: op         :: tl -> Instruction.ldc op; step tl
    | 0xb6 :: op1 :: op2 :: tl -> Instruction.invokevirtual op1 op2; step tl
    | 0xb1 :: _                -> Instruction.return ()
    | [] -> print_endline "nothing to run."
    | _  -> print_endline "not implemented step."
    in
  match entry_class.methods.attributes.(0) with
  | Code ({ _max_stack; _max_locals; code; _exception_table }, _attributes) ->
      step (Array.to_list code |> List.map Uint8.to_int)
  | _ -> print_endline "illegal entry_point"
