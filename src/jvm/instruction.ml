open Stdint
open Printf
open Machine

let aaload () = print_endline "aaload"
let aastore () = print_endline "aastore"
let aconst_null () = print_endline "aconst_null"
let aload index = printf "aload %x\n" (Uint8.to_int index)
let aload_ n = printf "aload_ %n\n" n
let anewarray = ()
let areturn = ()
let arraylength = ()
let astore = ()
let astore_ = ()
let athrow = ()
let baload = ()
let bastore = ()
let bipush = ()
let caload = ()
let castore = ()
let checkcast = ()
let d2f = ()
let d2i = ()
let d2l = ()
let dadd = ()
let daload = ()
let dastore = ()
let dcmpl = ()
let dcmpg = ()
let dconst_ = ()
let ddiv = ()
let dload = ()
let dload_ = ()
let dmul = ()
let dneg = ()
let drem = ()
let dreturn = ()
let dstore = ()
let dstore_ = ()
let dsub = ()
let dup = ()
let dup_x1 = ()
let dup_x2 = ()
let dup2 = ()
let dup2_x1 = ()
let dup2_x2 = ()
let f2d = ()
let f2i = ()
let f2l = ()
let fadd = ()
let faload = ()
let fastore = ()
let fcmpl = ()
let fcmpg = ()
let fconst_ = ()
let fdiv = ()
let fload = ()
let fload_ = ()
let fmul = ()
let fneg = ()
let frem = ()
let freturn = ()
let fstore = ()
let fstore_ = ()
let fsub = ()
let getfield = ()

let getstatic machine byte1 byte2 =
  get_constant_8 machine byte1 byte2
  |> field_resolution machine |> stack_push machine

let goto = ()
let goto_w = ()
let i2b = ()
let i2c = ()
let i2d = ()
let i2f = ()
let i2l = ()
let i2s = ()
let iadd = ()
let iaload = ()
let iand = ()
let iastore = ()
let iconst_m1 = ()
let iconst_ = ()
let idiv = ()
let if_acmpeq = ()
let if_acmpne = ()
let if_icmpeq = ()
let if_icmpne = ()
let if_icmplt = ()
let if_icmpge = ()
let if_icmpgt = ()
let if_icmple = ()
let ifeq = ()
let ifne = ()
let iflt = ()
let ifge = ()
let ifgt = ()
let ifle = ()
let ifnonnull = ()
let ifnull = ()
let iinc = ()
let iload = ()
let iload_ = ()
let imul = ()
let ineg = ()
let instanceof = ()
let invokedynamic = ()
let invokeinterface = ()

let invokespecial operand1 operand2 =
  Printf.printf "invokespecial %x %x\n" operand1 operand2

let invokestatic = ()

let invokevirtual machine op1 op2 =
  let get_constant_16 = get_constant_16 machine in
  match get_constant_8 machine op1 op2 with
  | Classfile.Cp_info.Methodref { class_index; name_and_type_index } ->
    let _ = class_index in
    let method_name, arguments =
      match get_constant_16 name_and_type_index with
      | Classfile.Cp_info.Name_and_type { name_index; descriptor_index } ->
        Classfile.Cp_info.
          ( get_constant_16 name_index |> utf8_to_string,
            get_constant_16 descriptor_index |> utf8_to_string )
      | _ -> invalid_arg "not implemented pattern of invokevirtual"
    in
    let arguments = Str.(split (regexp ";") arguments) in
    let arguments = List.length arguments - 1 |> Machine.stack_pop machine in
    Java_libs.call method_name @@ List.map Frame.to_java_primitive arguments
  | _ -> invalid_arg "not implemented pattern of invokevirtual"

let ior = ()
let irem = ()
let ireturn = ()
let ishl = ()
let ishr = ()
let istore = ()
let istore_ = ()
let isub = ()
let iushr = ()
let ixor = ()
let jsr = ()
let jsr_w = ()
let l2d = ()
let l2f = ()
let l2i = ()
let ladd = ()
let laload = ()
let land_ = ()
let lastore = ()
let lcmp = ()
let lconst_ = ()

let ldc machine operand =
  match get_constant machine operand with
  | Classfile.Cp_info.String v ->
    Frame.String (get_constant_16 machine v |> Classfile.Cp_info.utf8_to_string)
    |> Machine.stack_push machine
  | _ -> invalid_arg "not implemented"

let ldc_w = ()
let ldc2_w = ()
let ldiv = ()
let lload = ()
let lload_ = ()
let lmul = ()
let lneg = ()
let lookupswitch = ()
let lor_ = ()
let lrem = ()
let lreturn = ()
let lshl = ()
let lshr = ()
let lstore = ()
let lstore_ = ()
let lsub = ()
let lushr = ()
let lxor_ = ()
let monitorenter = ()
let monitorexit = ()
let multianewarray = ()
let new_ = ()
let newarray = ()
let nop = ()
let pop = ()
let pop2 = ()
let putfield = ()
let putstatic = ()
let ret = ()
let return () = ()
let saload = ()
let sastore = ()
let sipush = ()
let swap = ()
let tableswitch = ()
let wide = ()

let rec step (machine : t) = function
  | 0x00 :: tl -> (* nop *) step machine tl
  | 0x01 :: _tl -> aconst_null ()
  | 0x02 :: _tl -> iconst_m1
  | 0x03 :: _tl -> iconst_ (*0*)
  | 0x04 :: _tl -> iconst_ (*1*)
  | 0x05 :: _tl -> iconst_ (*2*)
  | 0x06 :: _tl -> iconst_ (*3*)
  | 0x07 :: _tl -> iconst_ (*4*)
  | 0x08 :: _tl -> iconst_ (*5*)
  | 0x09 :: _tl -> lconst_ (*0*)
  | 0x0a :: _tl -> lconst_ (*1*)
  | 0x0b :: _tl -> fconst_ (*0*)
  | 0x0c :: _tl -> fconst_ (*1*)
  | 0x0d :: _tl -> fconst_ (*2*)
  | 0x0e :: _tl -> dconst_ (*0*)
  | 0x0f :: _tl -> dconst_ (*1*)
  | 0x10 :: _tl -> bipush
  | 0x11 :: _tl -> sipush
  | 0x12 :: op :: tl ->
    ldc machine op;
    step machine tl
  | 0x13 :: _tl -> ldc_w
  | 0x14 :: _tl -> ldc2_w
  | 0x15 :: _tl -> iload
  | 0x16 :: _tl -> lload
  | 0x17 :: _tl -> fload
  | 0x18 :: _tl -> dload
  | 0x19 :: _tl -> aload @@ Uint8.of_int 0
  | 0x1a :: _tl -> iload_ (*0*)
  | 0x1b :: _tl -> iload_ (*1*)
  | 0x1c :: _tl -> iload_ (*2*)
  | 0x1d :: _tl -> iload_ (*3*)
  | 0x1e :: _tl -> lload_ (*0*)
  | 0x1f :: _tl -> lload_ (*1*)
  | 0x20 :: _tl -> lload_ (*2*)
  | 0x21 :: _tl -> lload_ (*3*)
  | 0x22 :: _tl -> fload_ (*0*)
  | 0x23 :: _tl -> fload_ (*1*)
  | 0x24 :: _tl -> fload_ (*2*)
  | 0x25 :: _tl -> fload_ (*3*)
  | 0x26 :: _tl -> dload_ (*0*)
  | 0x27 :: _tl -> dload_ (*1*)
  | 0x28 :: _tl -> dload_ (*2*)
  | 0x29 :: _tl -> dload_ (*3*)
  | 0x2a :: tl ->
    aload_ 0;
    step machine tl
  | 0x2b :: tl ->
    aload_ 1;
    step machine tl
  | 0x2c :: tl ->
    aload_ 2;
    step machine tl
  | 0x2d :: tl ->
    aload_ 3;
    step machine tl
  | 0x2e :: _tl -> iaload
  | 0x2f :: _tl -> laload
  | 0x30 :: _tl -> faload
  | 0x31 :: _tl -> daload
  | 0x32 :: _tl -> aaload ()
  | 0x33 :: _tl -> baload
  | 0x34 :: _tl -> caload
  | 0x35 :: _tl -> saload
  | 0x36 :: _tl -> istore
  | 0x37 :: _tl -> lstore
  | 0x38 :: _tl -> fstore
  | 0x39 :: _tl -> dstore
  | 0x3a :: _tl -> astore
  | 0x3b :: _tl -> istore_ (*0*)
  | 0x3c :: _tl -> istore_ (*1*)
  | 0x3d :: _tl -> istore_ (*2*)
  | 0x3e :: _tl -> istore_ (*3*)
  | 0x3f :: _tl -> lstore_ (*0*)
  | 0x40 :: _tl -> lstore_ (*1*)
  | 0x41 :: _tl -> lstore_ (*2*)
  | 0x42 :: _tl -> lstore_ (*3*)
  | 0x43 :: _tl -> fstore_ (*0*)
  | 0x44 :: _tl -> fstore_ (*1*)
  | 0x45 :: _tl -> fstore_ (*2*)
  | 0x46 :: _tl -> fstore_ (*3*)
  | 0x47 :: _tl -> dstore_ (*0*)
  | 0x48 :: _tl -> dstore_ (*1*)
  | 0x49 :: _tl -> dstore_ (*2*)
  | 0x4a :: _tl -> dstore_ (*3*)
  | 0x4b :: _tl -> astore_ (*0*)
  | 0x4c :: _tl -> astore_ (*1*)
  | 0x4d :: _tl -> astore_ (*2*)
  | 0x4e :: _tl -> astore_ (*3*)
  | 0x4f :: _tl -> iastore
  | 0x50 :: _tl -> lastore
  | 0x51 :: _tl -> fastore
  | 0x52 :: _tl -> dastore
  | 0x53 :: _tl -> aastore ()
  | 0x54 :: _tl -> bastore
  | 0x55 :: _tl -> castore
  | 0x56 :: _tl -> sastore
  | 0x57 :: _tl -> pop
  | 0x58 :: _tl -> pop2
  | 0x59 :: _tl -> dup
  | 0x5a :: _tl -> dup_x1
  | 0x5b :: _tl -> dup_x2
  | 0x5c :: _tl -> dup2
  | 0x5d :: _tl -> dup2_x1
  | 0x5e :: _tl -> dup2_x2
  | 0x5f :: _tl -> swap
  | 0x60 :: _tl -> iadd
  | 0x61 :: _tl -> ladd
  | 0x62 :: _tl -> fadd
  | 0x63 :: _tl -> dadd
  | 0x64 :: _tl -> isub
  | 0x65 :: _tl -> lsub
  | 0x66 :: _tl -> fsub
  | 0x67 :: _tl -> dsub
  | 0x68 :: _tl -> imul
  | 0x69 :: _tl -> lmul
  | 0x6a :: _tl -> fmul
  | 0x6b :: _tl -> dmul
  | 0x6c :: _tl -> idiv
  | 0x6d :: _tl -> ldiv
  | 0x6e :: _tl -> fdiv
  | 0x6f :: _tl -> ddiv
  | 0x70 :: _tl -> irem
  | 0x71 :: _tl -> lrem
  | 0x72 :: _tl -> frem
  | 0x73 :: _tl -> drem
  | 0x74 :: _tl -> ineg
  | 0x75 :: _tl -> lneg
  | 0x76 :: _tl -> fneg
  | 0x77 :: _tl -> dneg
  | 0x78 :: _tl -> ishl
  | 0x79 :: _tl -> lshl
  | 0x7a :: _tl -> ishr
  | 0x7b :: _tl -> lshr
  | 0x7c :: _tl -> iushr
  | 0x7d :: _tl -> lushr
  | 0x7e :: _tl -> iand
  | 0x7f :: _tl -> land_
  | 0x80 :: _tl -> ior
  | 0x81 :: _tl -> lor_
  | 0x82 :: _tl -> ixor
  | 0x83 :: _tl -> lxor_
  | 0x84 :: _tl -> iinc
  | 0x85 :: _tl -> i2l
  | 0x86 :: _tl -> i2f
  | 0x87 :: _tl -> i2d
  | 0x88 :: _tl -> l2i
  | 0x89 :: _tl -> l2f
  | 0x8a :: _tl -> l2d
  | 0x8b :: _tl -> f2i
  | 0x8c :: _tl -> f2l
  | 0x8d :: _tl -> f2d
  | 0x8e :: _tl -> d2i
  | 0x8f :: _tl -> d2l
  | 0x90 :: _tl -> d2f
  | 0x91 :: _tl -> i2b
  | 0x92 :: _tl -> i2c
  | 0x93 :: _tl -> i2s
  | 0x94 :: _tl -> lcmp
  | 0x95 :: _tl -> fcmpl
  | 0x96 :: _tl -> fcmpg
  | 0x97 :: _tl -> dcmpl
  | 0x98 :: _tl -> dcmpg
  | 0x99 :: _tl -> ifeq
  | 0x9a :: _tl -> ifne
  | 0x9b :: _tl -> iflt
  | 0x9c :: _tl -> ifge
  | 0x9d :: _tl -> ifgt
  | 0x9e :: _tl -> ifle
  | 0x9f :: _tl -> if_icmpeq
  | 0xa0 :: _tl -> if_icmpne
  | 0xa1 :: _tl -> if_icmplt
  | 0xa2 :: _tl -> if_icmpge
  | 0xa3 :: _tl -> if_icmpgt
  | 0xa4 :: _tl -> if_icmple
  | 0xa5 :: _tl -> if_acmpeq
  | 0xa6 :: _tl -> if_acmpne
  | 0xa7 :: _tl -> goto
  | 0xa8 :: _tl -> jsr
  | 0xa9 :: _tl -> ret
  | 0xaa :: _tl -> tableswitch
  | 0xab :: _tl -> lookupswitch
  | 0xac :: _tl -> ireturn
  | 0xad :: _tl -> lreturn
  | 0xae :: _tl -> freturn
  | 0xaf :: _tl -> dreturn
  | 0xb0 :: _tl -> areturn
  | 0xb1 :: _ -> return ()
  | 0xb2 :: op1 :: op2 :: tl ->
    let op1 = Uint8.of_int op1 and op2 = Uint8.of_int op2 in
    getstatic machine op1 op2;
    step machine tl
  | 0xb3 :: _tl -> putstatic
  | 0xb4 :: _tl -> getfield
  | 0xb5 :: _tl -> putfield
  | 0xb6 :: op1 :: op2 :: tl ->
    let op1 = Uint8.of_int op1 and op2 = Uint8.of_int op2 in
    invokevirtual machine op1 op2;
    step machine tl
  | 0xb7 :: op1 :: op2 :: tl ->
    invokespecial op1 op2;
    step machine tl
  | 0xb8 :: _tl -> invokestatic
  | 0xb9 :: _tl -> invokeinterface
  | 0xba :: _tl -> invokedynamic
  | 0xbb :: _tl -> new_
  | 0xbc :: _tl -> newarray
  | 0xbd :: _tl -> anewarray
  | 0xbe :: _tl -> arraylength
  | 0xbf :: _tl -> athrow
  | 0xc0 :: _tl -> checkcast
  | 0xc1 :: _tl -> instanceof
  | 0xc2 :: _tl -> monitorenter
  | 0xc3 :: _tl -> monitorexit
  | 0xc4 :: _tl -> wide
  | 0xc5 :: _tl -> multianewarray
  | 0xc6 :: _tl -> ifnull
  | 0xc7 :: _tl -> ifnonnull
  | 0xc8 :: _tl -> goto_w
  | 0xc9 :: _tl -> jsr_w
  | 0xca :: _tl -> print_endline "breakpoint"
  | 0xfe :: _tl -> print_endline "impdep1"
  | 0xff :: _tl -> print_endline "impdep2"
  | [] -> print_endline "nothing to run."
  | _ -> print_endline "empty opecode."
