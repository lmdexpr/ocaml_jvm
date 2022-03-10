module Frame = struct
  type t = { dummy : int }
end

module Runtime_data_area = struct
  type t = {
    (* todo
       pc_register: Frame.t;
    *)
    stacks : frame Stack.t;
    (* todo
       heap: ???;
       method_area: ???;
    *)
    run_time_cp : Classfile.Cp_info.t array;
        (* todo
           native_method_stacks: ??? Stack.t;
        *)
  }

  let create (class_file : Classfile.t) : Runtime_data_area.t =
    { stacks = Stack.create (); run_time_cp = class_file.constant_pool }
end

let run (entry_class : Classfile.t) =
  let ( >> ) a b = a |> fun _ -> b in
  let rda = Runtime_data_area.create entry_class in
  let rec step = function
    | 0x00 :: tl -> (* nop *) step tl
    | 0x01 :: tl -> Instruction.aconst_null
    | 0x02 :: tl -> Instruction.iconst_m1
    | 0x03 :: tl -> Instruction.iconst_ 0
    | 0x04 :: tl -> Instruction.iconst_ 1
    | 0x05 :: tl -> Instruction.iconst_ 2
    | 0x06 :: tl -> Instruction.iconst_ 3
    | 0x07 :: tl -> Instruction.iconst_ 4
    | 0x08 :: tl -> Instruction.iconst_ 5
    | 0x09 :: tl -> Instruction.lconst_ 0
    | 0x0a :: tl -> Instruction.lconst_ 1
    | 0x0b :: tl -> Instruction.fconst_ 0
    | 0x0c :: tl -> Instruction.fconst_ 1
    | 0x0d :: tl -> Instruction.fconst_ 2
    | 0x0e :: tl -> Instruction.dconst_ 0
    | 0x0f :: tl -> Instruction.dconst_ 1
    | 0x10 :: tl -> Instruction.bipush
    | 0x11 :: tl -> Instruction.sipush
    | 0x12 :: op :: tl -> Instruction.ldc op >> step tl
    | 0x13 :: tl -> Instruction.ldc_w
    | 0x14 :: tl -> Instruction.ldc2_w
    | 0x15 :: tl -> Instruction.iload
    | 0x16 :: tl -> Instruction.lload
    | 0x17 :: tl -> Instruction.fload
    | 0x18 :: tl -> Instruction.dload
    | 0x19 :: tl -> Instruction.aload
    | 0x1a :: tl -> Instruction.iload_ 0
    | 0x1b :: tl -> Instruction.iload_ 1
    | 0x1c :: tl -> Instruction.iload_ 2
    | 0x1d :: tl -> Instruction.iload_ 3
    | 0x1e :: tl -> Instruction.lload_ 0
    | 0x1f :: tl -> Instruction.lload_ 1
    | 0x20 :: tl -> Instruction.lload_ 2
    | 0x21 :: tl -> Instruction.lload_ 3
    | 0x22 :: tl -> Instruction.fload_ 0
    | 0x23 :: tl -> Instruction.fload_ 1
    | 0x24 :: tl -> Instruction.fload_ 2
    | 0x25 :: tl -> Instruction.fload_ 3
    | 0x26 :: tl -> Instruction.dload_ 0
    | 0x27 :: tl -> Instruction.dload_ 1
    | 0x28 :: tl -> Instruction.dload_ 2
    | 0x29 :: tl -> Instruction.dload_ 3
    | 0x2a :: tl -> Instruction.aload_ 0 >> step tl
    | 0x2b :: tl -> Instruction.aload_ 1 >> step tl
    | 0x2c :: tl -> Instruction.aload_ 2 >> step tl
    | 0x2d :: tl -> Instruction.aload_ 3 >> step tl
    | 0x2e :: tl -> Instruction.iaload
    | 0x2f :: tl -> Instruction.laload
    | 0x30 :: tl -> Instruction.faload
    | 0x31 :: tl -> Instruction.daload
    | 0x32 :: tl -> Instruction.aaload ()
    | 0x33 :: tl -> Instruction.baload
    | 0x34 :: tl -> Instruction.caload
    | 0x35 :: tl -> Instruction.saload
    | 0x36 :: tl -> Instruction.istore
    | 0x37 :: tl -> Instruction.lstore
    | 0x38 :: tl -> Instruction.fstore
    | 0x39 :: tl -> Instruction.dstore
    | 0x3a :: tl -> Instruction.astore
    | 0x3b :: tl -> Instruction.istore_ 0
    | 0x3c :: tl -> Instruction.istore_ 1
    | 0x3d :: tl -> Instruction.istore_ 2
    | 0x3e :: tl -> Instruction.istore_ 3
    | 0x3f :: tl -> Instruction.lstore_ 0
    | 0x40 :: tl -> Instruction.lstore_ 1
    | 0x41 :: tl -> Instruction.lstore_ 2
    | 0x42 :: tl -> Instruction.lstore_ 3
    | 0x43 :: tl -> Instruction.fstore_ 0
    | 0x44 :: tl -> Instruction.fstore_ 1
    | 0x45 :: tl -> Instruction.fstore_ 2
    | 0x46 :: tl -> Instruction.fstore_ 3
    | 0x47 :: tl -> Instruction.dstore_ 0
    | 0x48 :: tl -> Instruction.dstore_ 1
    | 0x49 :: tl -> Instruction.dstore_ 2
    | 0x4a :: tl -> Instruction.dstore_ 3
    | 0x4b :: tl -> Instruction.astore_ 0
    | 0x4c :: tl -> Instruction.astore_ 1
    | 0x4d :: tl -> Instruction.astore_ 2
    | 0x4e :: tl -> Instruction.astore_ 3
    | 0x4f :: tl -> Instruction.iastore
    | 0x50 :: tl -> Instruction.lastore
    | 0x51 :: tl -> Instruction.fastore
    | 0x52 :: tl -> Instruction.dastore
    | 0x53 :: tl -> Instruction.aastore
    | 0x54 :: tl -> Instruction.bastore
    | 0x55 :: tl -> Instruction.castore
    | 0x56 :: tl -> Instruction.sastore
    | 0x57 :: tl -> Instruction.pop
    | 0x58 :: tl -> Instruction.pop2
    | 0x59 :: tl -> Instruction.dup
    | 0x5a :: tl -> Instruction.dup_x1
    | 0x5b :: tl -> Instruction.dup_x2
    | 0x5c :: tl -> Instruction.dup2
    | 0x5d :: tl -> Instruction.dup2_x1
    | 0x5e :: tl -> Instruction.dup2_x2
    | 0x5f :: tl -> Instruction.swap
    | 0x60 :: tl -> Instruction.iadd
    | 0x61 :: tl -> Instruction.ladd
    | 0x62 :: tl -> Instruction.fadd
    | 0x63 :: tl -> Instruction.dadd
    | 0x64 :: tl -> Instruction.isub
    | 0x65 :: tl -> Instruction.lsub
    | 0x66 :: tl -> Instruction.fsub
    | 0x67 :: tl -> Instruction.dsub
    | 0x68 :: tl -> Instruction.imul
    | 0x69 :: tl -> Instruction.lmul
    | 0x6a :: tl -> Instruction.fmul
    | 0x6b :: tl -> Instruction.dmul
    | 0x6c :: tl -> Instruction.idiv
    | 0x6d :: tl -> Instruction.ldiv
    | 0x6e :: tl -> Instruction.fdiv
    | 0x6f :: tl -> Instruction.ddiv
    | 0x70 :: tl -> Instruction.irem
    | 0x71 :: tl -> Instruction.lrem
    | 0x72 :: tl -> Instruction.frem
    | 0x73 :: tl -> Instruction.drem
    | 0x74 :: tl -> Instruction.ineg
    | 0x75 :: tl -> Instruction.lneg
    | 0x76 :: tl -> Instruction.fneg
    | 0x77 :: tl -> Instruction.dneg
    | 0x78 :: tl -> Instruction.ishl
    | 0x79 :: tl -> Instruction.lshl
    | 0x7a :: tl -> Instruction.ishr
    | 0x7b :: tl -> Instruction.lshr
    | 0x7c :: tl -> Instruction.iushr
    | 0x7d :: tl -> Instruction.lushr
    | 0x7e :: tl -> Instruction.iand
    | 0x7f :: tl -> Instruction.land_
    | 0x80 :: tl -> Instruction.ior
    | 0x81 :: tl -> Instruction.lor_
    | 0x82 :: tl -> Instruction.ixor
    | 0x83 :: tl -> Instruction.lxor_
    | 0x84 :: tl -> Instruction.iinc
    | 0x85 :: tl -> Instruction.i2l
    | 0x86 :: tl -> Instruction.i2f
    | 0x87 :: tl -> Instruction.i2d
    | 0x88 :: tl -> Instruction.l2i
    | 0x89 :: tl -> Instruction.l2f
    | 0x8a :: tl -> Instruction.l2d
    | 0x8b :: tl -> Instruction.f2i
    | 0x8c :: tl -> Instruction.f2l
    | 0x8d :: tl -> Instruction.f2d
    | 0x8e :: tl -> Instruction.d2i
    | 0x8f :: tl -> Instruction.d2l
    | 0x90 :: tl -> Instruction.d2f
    | 0x91 :: tl -> Instruction.i2b
    | 0x92 :: tl -> Instruction.i2c
    | 0x93 :: tl -> Instruction.i2s
    | 0x94 :: tl -> Instruction.lcmp
    | 0x95 :: tl -> Instruction.fcmpl
    | 0x96 :: tl -> Instruction.fcmpg
    | 0x97 :: tl -> Instruction.dcmpl
    | 0x98 :: tl -> Instruction.dcmpg
    | 0x99 :: tl -> Instruction.ifeq
    | 0x9a :: tl -> Instruction.ifne
    | 0x9b :: tl -> Instruction.iflt
    | 0x9c :: tl -> Instruction.ifge
    | 0x9d :: tl -> Instruction.ifgt
    | 0x9e :: tl -> Instruction.ifle
    | 0x9f :: tl -> Instruction.if_icmpeq
    | 0xa0 :: tl -> Instruction.if_icmpne
    | 0xa1 :: tl -> Instruction.if_icmplt
    | 0xa2 :: tl -> Instruction.if_icmpge
    | 0xa3 :: tl -> Instruction.if_icmpgt
    | 0xa4 :: tl -> Instruction.if_icmple
    | 0xa5 :: tl -> Instruction.if_acmpeq
    | 0xa6 :: tl -> Instruction.if_acmpne
    | 0xa7 :: tl -> Instruction.goto
    | 0xa8 :: tl -> Instruction.jsr
    | 0xa9 :: tl -> Instruction.ret
    | 0xaa :: tl -> Instruction.tableswitch
    | 0xab :: tl -> Instruction.lookupswitch
    | 0xac :: tl -> Instruction.ireturn
    | 0xad :: tl -> Instruction.lreturn
    | 0xae :: tl -> Instruction.freturn
    | 0xaf :: tl -> Instruction.dreturn
    | 0xb0 :: tl -> Instruction.areturn
    | 0xb1 :: _ -> Instruction.return ()
    | 0xb2 :: op1 :: op2 :: tl -> Instruction.getstatic op1 op2 >> step tl
    | 0xb3 :: tl -> Instruction.putstatic
    | 0xb4 :: tl -> Instruction.getfield
    | 0xb5 :: tl -> Instruction.putfield
    | 0xb6 :: op1 :: op2 :: tl -> Instruction.invokevirtual op1 op2 >> step tl
    | 0xb7 :: tl -> Instruction.invokespecial >> step tl
    | 0xb8 :: tl -> Instruction.invokestatic
    | 0xb9 :: tl -> Instruction.invokeinterface
    | 0xba :: tl -> Instruction.invokedynamic
    | 0xbb :: tl -> Instruction.new_
    | 0xbc :: tl -> Instruction.newarray
    | 0xbd :: tl -> Instruction.anewarray
    | 0xbe :: tl -> Instruction.arraylength
    | 0xbf :: tl -> Instruction.athrow
    | 0xc0 :: tl -> Instruction.checkcast
    | 0xc1 :: tl -> Instruction.instanceof
    | 0xc2 :: tl -> Instruction.monitorenter
    | 0xc3 :: tl -> Instruction.monitorexit
    | 0xc4 :: tl -> Instruction.wide
    | 0xc5 :: tl -> Instruction.multianewarray
    | 0xc6 :: tl -> Instruction.ifnull
    | 0xc7 :: tl -> Instruction.ifnonnull
    | 0xc8 :: tl -> Instruction.goto_w
    | 0xc9 :: tl -> Instruction.jsr_w
    | 0xca :: _ -> print_endline "breakpoint"
    | 0xfe :: _ -> print_endline "impdep1"
    | 0xff :: _ -> print_endline "impdep2"
    | [] -> print_endline "nothing to run."
    | _ -> print_endline "empty opecode."
  in
  match entry_class.methods.attributes.(1) with
  (* todo load constructor *)
  | Code ({ _max_stack; _max_locals; code; _exception_table }, _attributes) ->
      step (Array.to_list code |> List.map Uint8.to_int)
  | _ -> print_endline "illegal entry_point"
