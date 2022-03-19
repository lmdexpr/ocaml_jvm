type t =
  { rda : Runtime_data_area.t
  ; entry_class : Classfile.t
  }

let make (entry_class : Classfile.t) =
  { rda = Runtime_data_area.make entry_class; entry_class }

let current_frame machine = Stack.pop machine.rda.stack

let rec invoke frame cp p code =
  let open Stdint in
  let open Utils.Try.Ops in
  let open Instruction in
  let op k = code.(p + k) in
  let cont k = invoke frame cp (p + k) code in
  let ( >> ) x k = x >>= fun _ -> cont k in
  match op 0 with
  | 0x00 -> (* nop *) Result.ok 1 >>= cont
  | 0x01 -> aconst_null >> 1
  | 0x02 -> iconst_ frame (-1) >> 1
  | 0x03 -> iconst_ frame 0 >> 1
  | 0x04 -> iconst_ frame 1 >> 1
  | 0x05 -> iconst_ frame 2 >> 1
  | 0x06 -> iconst_ frame 3 >> 1
  | 0x07 -> iconst_ frame 4 >> 1
  | 0x08 -> iconst_ frame 5 >> 1
  | 0x09 -> lconst_ (*0*) >> 1
  | 0x0a -> lconst_ (*1*) >> 1
  | 0x0b -> fconst_ (*0*) >> 1
  | 0x0c -> fconst_ (*1*) >> 1
  | 0x0d -> fconst_ (*2*) >> 1
  | 0x0e -> dconst_ (*0*) >> 1
  | 0x0f -> dconst_ (*1*) >> 1
  | 0x10 -> bipush frame (op 1) >> 2
  | 0x11 -> sipush >> 1
  | 0x12 -> ldc frame cp (op 1) >> 2
  | 0x13 -> ldc_w >> 1
  | 0x14 -> ldc2_w >> 1
  | 0x15 -> iload >> 1
  | 0x16 -> lload >> 1
  | 0x17 -> fload >> 1
  | 0x18 -> dload >> 1
  | 0x19 -> aload >> 1
  | 0x1a -> iload_ frame 0 >> 1
  | 0x1b -> iload_ frame 1 >> 1
  | 0x1c -> iload_ frame 2 >> 1
  | 0x1d -> iload_ frame 3 >> 1
  | 0x1e -> lload_ (*0*) >> 1
  | 0x1f -> lload_ (*1*) >> 1
  | 0x20 -> lload_ (*2*) >> 1
  | 0x21 -> lload_ (*3*) >> 1
  | 0x22 -> fload_ (*0*) >> 1
  | 0x23 -> fload_ (*1*) >> 1
  | 0x24 -> fload_ (*2*) >> 1
  | 0x25 -> fload_ (*3*) >> 1
  | 0x26 -> dload_ (*0*) >> 1
  | 0x27 -> dload_ (*1*) >> 1
  | 0x28 -> dload_ (*2*) >> 1
  | 0x29 -> dload_ (*3*) >> 1
  | 0x2a -> aload_ 0 >> 1
  | 0x2b -> aload_ 1 >> 1
  | 0x2c -> aload_ 2 >> 1
  | 0x2d -> aload_ 3 >> 1
  | 0x2e -> iaload >> 1
  | 0x2f -> laload >> 1
  | 0x30 -> faload >> 1
  | 0x31 -> daload >> 1
  | 0x32 -> aaload >> 1
  | 0x33 -> baload >> 1
  | 0x34 -> caload >> 1
  | 0x35 -> saload >> 1
  | 0x36 -> istore >> 1
  | 0x37 -> lstore >> 1
  | 0x38 -> fstore >> 1
  | 0x39 -> dstore >> 1
  | 0x3a -> astore >> 1
  | 0x3b -> istore_ frame 0 >> 1
  | 0x3c -> istore_ frame 1 >> 1
  | 0x3d -> istore_ frame 2 >> 1
  | 0x3e -> istore_ frame 3 >> 1
  | 0x3f -> lstore_ (*0*) >> 1
  | 0x40 -> lstore_ (*1*) >> 1
  | 0x41 -> lstore_ (*2*) >> 1
  | 0x42 -> lstore_ (*3*) >> 1
  | 0x43 -> fstore_ (*0*) >> 1
  | 0x44 -> fstore_ (*1*) >> 1
  | 0x45 -> fstore_ (*2*) >> 1
  | 0x46 -> fstore_ (*3*) >> 1
  | 0x47 -> dstore_ (*0*) >> 1
  | 0x48 -> dstore_ (*1*) >> 1
  | 0x49 -> dstore_ (*2*) >> 1
  | 0x4a -> dstore_ (*3*) >> 1
  | 0x4b -> astore_ (*0*) >> 1
  | 0x4c -> astore_ (*1*) >> 1
  | 0x4d -> astore_ (*2*) >> 1
  | 0x4e -> astore_ (*3*) >> 1
  | 0x4f -> iastore >> 1
  | 0x50 -> lastore >> 1
  | 0x51 -> fastore >> 1
  | 0x52 -> dastore >> 1
  | 0x53 -> aastore >> 1
  | 0x54 -> bastore >> 1
  | 0x55 -> castore >> 1
  | 0x56 -> sastore >> 1
  | 0x57 -> pop >> 1
  | 0x58 -> pop2 >> 1
  | 0x59 -> dup >> 1
  | 0x5a -> dup_x1 >> 1
  | 0x5b -> dup_x2 >> 1
  | 0x5c -> dup2 >> 1
  | 0x5d -> dup2_x1 >> 1
  | 0x5e -> dup2_x2 >> 1
  | 0x5f -> swap >> 1
  | 0x60 -> iadd >> 1
  | 0x61 -> ladd >> 1
  | 0x62 -> fadd >> 1
  | 0x63 -> dadd >> 1
  | 0x64 -> isub >> 1
  | 0x65 -> lsub >> 1
  | 0x66 -> fsub >> 1
  | 0x67 -> dsub >> 1
  | 0x68 -> imul >> 1
  | 0x69 -> lmul >> 1
  | 0x6a -> fmul >> 1
  | 0x6b -> dmul >> 1
  | 0x6c -> idiv >> 1
  | 0x6d -> ldiv >> 1
  | 0x6e -> fdiv >> 1
  | 0x6f -> ddiv >> 1
  | 0x70 -> irem frame >> 1
  | 0x71 -> lrem >> 1
  | 0x72 -> frem >> 1
  | 0x73 -> drem >> 1
  | 0x74 -> ineg >> 1
  | 0x75 -> lneg >> 1
  | 0x76 -> fneg >> 1
  | 0x77 -> dneg >> 1
  | 0x78 -> ishl >> 1
  | 0x79 -> lshl >> 1
  | 0x7a -> ishr >> 1
  | 0x7b -> lshr >> 1
  | 0x7c -> iushr >> 1
  | 0x7d -> lushr >> 1
  | 0x7e -> iand >> 1
  | 0x7f -> land_ >> 1
  | 0x80 -> ior >> 1
  | 0x81 -> lor_ >> 1
  | 0x82 -> ixor >> 1
  | 0x83 -> lxor_ >> 1
  | 0x84 -> iinc frame (op 1) (op 2) >> 3
  | 0x85 -> i2l >> 1
  | 0x86 -> i2f >> 1
  | 0x87 -> i2d >> 1
  | 0x88 -> l2i >> 1
  | 0x89 -> l2f >> 1
  | 0x8a -> l2d >> 1
  | 0x8b -> f2i >> 1
  | 0x8c -> f2l >> 1
  | 0x8d -> f2d >> 1
  | 0x8e -> d2i >> 1
  | 0x8f -> d2l >> 1
  | 0x90 -> d2f >> 1
  | 0x91 -> i2b >> 1
  | 0x92 -> i2c >> 1
  | 0x93 -> i2s >> 1
  | 0x94 -> lcmp >> 1
  | 0x95 -> fcmpl >> 1
  | 0x96 -> fcmpg >> 1
  | 0x97 -> dcmpl >> 1
  | 0x98 -> dcmpg >> 1
  | 0x99 -> if_ frame ( = ) (op 1) (op 2) >>= cont
  | 0x9a -> if_ frame ( <> ) (op 1) (op 2) >>= cont
  | 0x9b -> if_ frame ( < ) (op 1) (op 2) >>= cont
  | 0x9c -> if_ frame ( >= ) (op 1) (op 2) >>= cont
  | 0x9d -> if_ frame ( > ) (op 1) (op 2) >>= cont
  | 0x9e -> if_ frame ( <= ) (op 1) (op 2) >>= cont
  | 0x9f -> if_icmp frame ( = ) (op 1) (op 2) >>= cont
  | 0xa0 -> if_icmp frame ( <> ) (op 1) (op 2) >>= cont
  | 0xa1 -> if_icmp frame ( < ) (op 1) (op 2) >>= cont
  | 0xa2 -> if_icmp frame ( >= ) (op 1) (op 2) >>= cont
  | 0xa3 -> if_icmp frame ( > ) (op 1) (op 2) >>= cont
  | 0xa4 -> if_icmp frame ( <= ) (op 1) (op 2) >>= cont
  | 0xa5 -> if_acmpeq >> 1
  | 0xa6 -> if_acmpne >> 1
  | 0xa7 -> goto (op 1) (op 2) >>= cont
  | 0xa8 -> jsr >> 1
  | 0xa9 -> ret >> 1
  | 0xaa -> tableswitch >> 1
  | 0xab -> lookupswitch >> 1
  | 0xac -> ireturn
  | 0xad -> lreturn
  | 0xae -> freturn
  | 0xaf -> dreturn
  | 0xb0 -> areturn
  | 0xb1 -> return ()
  | 0xb2 ->
    let op1 = Uint8.of_int (op 1) and op2 = Uint8.of_int (op 2) in
    getstatic frame cp op1 op2 >> 3
  | 0xb3 -> putstatic >> 1
  | 0xb4 -> getfield >> 1
  | 0xb5 -> putfield >> 1
  | 0xb6 ->
    let op1 = Uint8.of_int (op 1) and op2 = Uint8.of_int (op 2) in
    invokevirtual frame cp op1 op2 >> 3
  | 0xb7 -> invokespecial (op 1) (op 2) >> 3
  | 0xb8 -> invokestatic >> 1
  | 0xb9 -> invokeinterface >> 1
  | 0xba -> invokedynamic >> 1
  | 0xbb -> new_ >> 1
  | 0xbc -> newarray >> 1
  | 0xbd -> anewarray >> 1
  | 0xbe -> arraylength >> 1
  | 0xbf -> athrow >> 1
  | 0xc0 -> checkcast >> 1
  | 0xc1 -> instanceof >> 1
  | 0xc2 -> monitorenter >> 1
  | 0xc3 -> monitorexit >> 1
  | 0xc4 -> wide >> 1
  | 0xc5 -> multianewarray >> 1
  | 0xc6 -> ifnull >> 1
  | 0xc7 -> ifnonnull >> 1
  | 0xc8 -> goto_w >> 1
  | 0xc9 -> jsr_w >> 1
  | 0xca ->
    print_endline "breakpoint";
    Result.error @@ Invalid_argument "breakpoint"
  | 0xfe ->
    print_endline "impdep1";
    Result.error @@ Invalid_argument "impdep1"
  | 0xff ->
    print_endline "impdep2";
    Result.error @@ Invalid_argument "impdep2"
  | _ -> Result.error @@ Invalid_argument "out of range"

let invoke (entry_point : Classfile.Method_info.t) machine =
  let cp = machine.rda.constant_pool in
  match entry_point.attributes.(0) with
  | Code (value, _attr) ->
    let frame = Frame.create value.max_locals in
    invoke frame cp 0 value.code
  | _ -> Result.error @@ Invalid_argument "illegal entry_point"
