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
  | 0x00 -> nop >>= cont
  | 0x01 -> aconst_null frame >> 1
  | 0x02 -> iconst_ (-1) frame >> 1
  | 0x03 -> iconst_ 0 frame >> 1
  | 0x04 -> iconst_ 1 frame >> 1
  | 0x05 -> iconst_ 2 frame >> 1
  | 0x06 -> iconst_ 3 frame >> 1
  | 0x07 -> iconst_ 4 frame >> 1
  | 0x08 -> iconst_ 5 frame >> 1
  | 0x09 -> lconst_ 0 frame >> 1
  | 0x0a -> lconst_ 1 frame >> 1
  | 0x0b -> fconst_ 0 frame >> 1
  | 0x0c -> fconst_ 1 frame >> 1
  | 0x0d -> fconst_ 2 frame >> 1
  | 0x0e -> dconst_ 0 frame >> 1
  | 0x0f -> dconst_ 1 frame >> 1
  | 0x10 -> bipush frame (op 1) >> 2
  | 0x11 -> sipush frame (op 1) (op 2) >> 3
  | 0x12 -> ldc frame cp (op 1) >> 2
  | 0x13 -> ldc_w frame cp (op 1) (op 2) >> 3
  | 0x14 -> ldc2_w frame cp (op 1) (op 2) >> 3
  | 0x15 -> iload frame (op 1) >> 2
  | 0x16 -> lload frame (op 1) >> 2
  | 0x17 -> fload frame (op 1) >> 2
  | 0x18 -> dload frame (op 1) >> 2
  | 0x19 -> aload frame (op 1) >> 2
  | 0x1a -> iload_ 0 frame >> 1
  | 0x1b -> iload_ 1 frame >> 1
  | 0x1c -> iload_ 2 frame >> 1
  | 0x1d -> iload_ 3 frame >> 1
  | 0x1e -> lload_ 0 frame >> 1
  | 0x1f -> lload_ 1 frame >> 1
  | 0x20 -> lload_ 2 frame >> 1
  | 0x21 -> lload_ 3 frame >> 1
  | 0x22 -> fload_ 0 frame >> 1
  | 0x23 -> fload_ 1 frame >> 1
  | 0x24 -> fload_ 2 frame >> 1
  | 0x25 -> fload_ 3 frame >> 1
  | 0x26 -> dload_ 0 frame >> 1
  | 0x27 -> dload_ 1 frame >> 1
  | 0x28 -> dload_ 2 frame >> 1
  | 0x29 -> dload_ 3 frame >> 1
  | 0x2a -> aload_ 0 frame >> 1
  | 0x2b -> aload_ 1 frame >> 1
  | 0x2c -> aload_ 2 frame >> 1
  | 0x2d -> aload_ 3 frame >> 1
  | 0x2e -> iaload frame >> 1
  | 0x2f -> laload frame >> 1
  | 0x30 -> faload frame >> 1
  | 0x31 -> daload frame >> 1
  | 0x32 -> aaload frame >> 1
  | 0x33 -> baload frame >> 1
  | 0x34 -> caload frame >> 1
  | 0x35 -> saload frame >> 1
  | 0x36 -> istore frame (op 1) >> 2
  | 0x37 -> lstore frame (op 1) >> 2
  | 0x38 -> fstore frame (op 1) >> 2
  | 0x39 -> dstore frame (op 1) >> 2
  | 0x3a -> astore frame (op 1) >> 2
  | 0x3b -> istore_ 0 frame >> 1
  | 0x3c -> istore_ 1 frame >> 1
  | 0x3d -> istore_ 2 frame >> 1
  | 0x3e -> istore_ 3 frame >> 1
  | 0x3f -> lstore_ 0 frame >> 1
  | 0x40 -> lstore_ 1 frame >> 1
  | 0x41 -> lstore_ 2 frame >> 1
  | 0x42 -> lstore_ 3 frame >> 1
  | 0x43 -> fstore_ 0 frame >> 1
  | 0x44 -> fstore_ 1 frame >> 1
  | 0x45 -> fstore_ 2 frame >> 1
  | 0x46 -> fstore_ 3 frame >> 1
  | 0x47 -> dstore_ 0 frame >> 1
  | 0x48 -> dstore_ 1 frame >> 1
  | 0x49 -> dstore_ 2 frame >> 1
  | 0x4a -> dstore_ 3 frame >> 1
  | 0x4b -> astore_ 0 frame >> 1
  | 0x4c -> astore_ 1 frame >> 1
  | 0x4d -> astore_ 2 frame >> 1
  | 0x4e -> astore_ 3 frame >> 1
  | 0x4f -> iastore frame >> 1
  | 0x50 -> lastore frame >> 1
  | 0x51 -> fastore frame >> 1
  | 0x52 -> dastore frame >> 1
  | 0x53 -> aastore frame >> 1
  | 0x54 -> bastore frame >> 1
  | 0x55 -> castore frame >> 1
  | 0x56 -> sastore frame >> 1
  | 0x57 -> pop frame >> 1
  | 0x58 -> pop2 frame >> 1
  | 0x59 -> dup frame >> 1
  | 0x5a -> dup_x1 frame >> 1
  | 0x5b -> dup_x2 frame >> 1
  | 0x5c -> dup2 frame >> 1
  | 0x5d -> dup2_x1 frame >> 1
  | 0x5e -> dup2_x2 frame >> 1
  | 0x5f -> swap frame >> 1
  | 0x60 -> iadd frame >> 1
  | 0x61 -> ladd frame >> 1
  | 0x62 -> fadd frame >> 1
  | 0x63 -> dadd frame >> 1
  | 0x64 -> isub frame >> 1
  | 0x65 -> lsub frame >> 1
  | 0x66 -> fsub frame >> 1
  | 0x67 -> dsub frame >> 1
  | 0x68 -> imul frame >> 1
  | 0x69 -> lmul frame >> 1
  | 0x6a -> fmul frame >> 1
  | 0x6b -> dmul frame >> 1
  | 0x6c -> idiv frame >> 1
  | 0x6d -> ldiv frame >> 1
  | 0x6e -> fdiv frame >> 1
  | 0x6f -> ddiv frame >> 1
  | 0x70 -> irem frame >> 1
  | 0x71 -> lrem frame >> 1
  | 0x72 -> frem frame >> 1
  | 0x73 -> drem frame >> 1
  | 0x74 -> ineg frame >> 1
  | 0x75 -> lneg frame >> 1
  | 0x76 -> fneg frame >> 1
  | 0x77 -> dneg frame >> 1
  | 0x78 -> ishl frame >> 1
  | 0x79 -> lshl frame >> 1
  | 0x7a -> ishr frame >> 1
  | 0x7b -> lshr frame >> 1
  | 0x7c -> iushr frame >> 1
  | 0x7d -> lushr frame >> 1
  | 0x7e -> iand frame >> 1
  | 0x7f -> land_ frame >> 1
  | 0x80 -> ior frame >> 1
  | 0x81 -> lor_ frame >> 1
  | 0x82 -> ixor frame >> 1
  | 0x83 -> lxor_ frame >> 1
  | 0x84 -> iinc frame (op 1) (op 2) >> 3
  | 0x85 -> i2l frame >> 1
  | 0x86 -> i2f frame >> 1
  | 0x87 -> i2d frame >> 1
  | 0x88 -> l2i frame >> 1
  | 0x89 -> l2f frame >> 1
  | 0x8a -> l2d frame >> 1
  | 0x8b -> f2i frame >> 1
  | 0x8c -> f2l frame >> 1
  | 0x8d -> f2d frame >> 1
  | 0x8e -> d2i frame >> 1
  | 0x8f -> d2l frame >> 1
  | 0x90 -> d2f frame >> 1
  | 0x91 -> i2b frame >> 1
  | 0x92 -> i2c frame >> 1
  | 0x93 -> i2s frame >> 1
  | 0x94 -> lcmp frame >> 1
  | 0x95 -> fcmp (*l*) false frame >> 1
  | 0x96 -> fcmp (*g*) true frame >> 1
  | 0x97 -> dcmp (*l*) false frame >> 1
  | 0x98 -> dcmp (*g*) true frame >> 1
  | 0x99 -> if_ ( = ) frame (op 1) (op 2) >>= cont
  | 0x9a -> if_ ( <> ) frame (op 1) (op 2) >>= cont
  | 0x9b -> if_ ( < ) frame (op 1) (op 2) >>= cont
  | 0x9c -> if_ ( >= ) frame (op 1) (op 2) >>= cont
  | 0x9d -> if_ ( > ) frame (op 1) (op 2) >>= cont
  | 0x9e -> if_ ( <= ) frame (op 1) (op 2) >>= cont
  | 0x9f -> if_icmp ( = ) frame (op 1) (op 2) >>= cont
  | 0xa0 -> if_icmp ( <> ) frame (op 1) (op 2) >>= cont
  | 0xa1 -> if_icmp ( < ) frame (op 1) (op 2) >>= cont
  | 0xa2 -> if_icmp ( >= ) frame (op 1) (op 2) >>= cont
  | 0xa3 -> if_icmp ( > ) frame (op 1) (op 2) >>= cont
  | 0xa4 -> if_icmp ( <= ) frame (op 1) (op 2) >>= cont
  | 0xa5 -> if_acmp ( = ) frame (op 1) (op 2) >> 3
  | 0xa6 -> if_acmp ( <> ) frame (op 1) (op 2) >> 3
  | 0xa7 -> goto (op 1) (op 2) >>= cont
  | 0xa8 -> jsr frame (op 1) (op 2) >> 3
  | 0xa9 -> ret frame (op 1) >> 2
  | 0xaa -> tableswitch frame >> 1
  | 0xab -> lookupswitch frame (*0-3 byte pad*) >> 1
  | 0xac -> ireturn frame
  | 0xad -> lreturn frame
  | 0xae -> freturn frame
  | 0xaf -> dreturn frame
  | 0xb0 -> areturn frame
  | 0xb1 -> return ()
  | 0xb2 ->
    let op1 = Uint8.of_int (op 1) and op2 = Uint8.of_int (op 2) in
    getstatic frame cp op1 op2 >> 3
  | 0xb3 -> putstatic frame (op 1) (op 2) >> 3
  | 0xb4 -> getfield frame (op 1) (op 2) >> 3
  | 0xb5 -> putfield frame (op 1) (op 2) >> 3
  | 0xb6 ->
    let op1 = Uint8.of_int (op 1) and op2 = Uint8.of_int (op 2) in
    invokevirtual frame cp op1 op2 >> 3
  | 0xb7 -> invokespecial frame (op 1) (op 2) >> 3
  | 0xb8 -> invokestatic frame (op 1) (op 2) >> 3
  | 0xb9 -> invokeinterface frame (op 1) (op 2) (op 3) (op 4) >> 5
  | 0xba -> invokedynamic frame (op 1) (op 2) (op 3) (op 4) >> 5
  | 0xbb -> new_ frame (op 1) (op 2) >> 3
  | 0xbc -> newarray frame (op 1) >> 2
  | 0xbd -> anewarray frame (op 1) (op 2) >> 3
  | 0xbe -> arraylength frame >> 1
  | 0xbf -> athrow frame >> 1
  | 0xc0 -> checkcast frame (op 1) (op 2) >> 3
  | 0xc1 -> instanceof frame (op 1) (op 2) >> 3
  | 0xc2 -> monitorenter frame >> 1
  | 0xc3 -> monitorexit frame >> 1
  | 0xc4 -> wide frame >> 1
  | 0xc5 -> multianewarray frame (op 1) (op 2) (op 3) >> 4
  | 0xc6 -> ifnull frame (op 1) (op 2) >> 3
  | 0xc7 -> ifnonnull frame (op 1) (op 2) >> 3
  | 0xc8 -> goto_w (op 1) (op 2) (op 3) (op 4) >> 5
  | 0xc9 -> jsr_w frame (op 1) (op 2) (op 3) (op 4) >> 5
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
