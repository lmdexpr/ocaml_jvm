open Stdint
open Printf

module Util = struct
  let get_constant constant_pool byte1 byte2 =
    let byte1 = Uint16.of_uint8 byte1 
    and byte2 = Uint16.of_uint8 byte2 in
    constant_pool.( Uint16.((shift_left byte1 8) + byte2) |> Uint16.to_int )

  let field_resolution constant_pool = function 
    | Cp_info.Fieldref { class_index; name_and_type_index } ->
        let class_index         = Uint16.to_int class_index
        and name_and_type_index = Uint16.to_int name_and_type_index in
        let callee_class =
          match constant_pool.(class_index) with
          | Cp_info.Class v -> constant_pool.(Uint16.to_int v) |> Cp_info.utf8_to_string
          | _ -> raise @@ Invalid_argument "illegal argument for resolution of field"
        in
        let field, field_type =
          match constant_pool.(name_and_type_index) with
          | Cp_info.Name_and_type { name_index; descriptor_index } ->
              let name_index = Uint16.to_int name_index
              and descriptor_index = Uint16.to_int descriptor_index in
              (constant_pool.(name_index), constant_pool.(descriptor_index))
          | _ -> raise @@ Invalid_argument "illegal argument for resolution of field"
        in
          (callee_class, field, field_type)
        
    | _ -> raise @@ Invalid_argument "illegal argument for resolution of field"
end

let aaload () = print_endline "aaload"

let aastore () = print_endline "aastore" 

let aconst_null () = print_endline "aconst_null"

let aload index = printf "aload %x\n" (Uint8.to_int index)

let aload_ n = printf "aload_ %n\n" n

let anewarray = ()
let areturn = ()
let arraylength = ()
let astore = ()
let astore_ n = ()
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
let dconst_ d = ()
let ddiv = ()
let dload = ()
let dload_ n = ()
let dmul = ()
let dneg = ()
let drem = ()
let dreturn = ()
let dstore = ()
let dstore_ n = ()
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
let fconst_ f = ()
let fdiv = ()
let fload = ()
let fload_ n = ()
let fmul = ()
let fneg = ()
let frem = ()
let freturn = ()
let fstore = ()
let fstore_ n = ()
let fsub = ()
let getfield = ()

let getstatic rda byte1 byte2 =
  let cp = rda.run_time_cp in
  let symbol_name_index = Util.get_constant cp byte1 byte2 in
  let callee_class, field, method_return = Util.field_resolution cp symbol_name_index
  in
    Stack.push (callee_class, field, method_return) rda.stacks 

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
let iconst_ i = ()
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
let iload_ n = ()
let imul = ()
let ineg = ()
let instanceof = ()
let invokedynamic = ()
let invokeinterface = ()
let invokespecial operand1 operand2 = 
  Printf.printf "invokespecial %x %x\n" (Uint8.to_int operand1) (Uint8.to_int operand2)

let invokestatic = ()

let invokevirtual operand1 operand2 =
  Printf.printf "invokevirtual %x %x\n" (Uint8.to_int operand1) (Uint8.to_int operand2)

let ior = ()
let irem = ()
let ireturn = ()
let ishl = ()
let ishr = ()
let istore = ()
let istore_ n = ()
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
let lconst_ l = ()

let ldc operand = Printf.printf "ldc %x\n" (Uint8.to_int operand)

let ldc_w = ()
let ldc2_w = ()
let ldiv = ()
let lload = ()
let lload_ n = ()
let lmul = ()
let lneg = ()
let lookupswitch = ()
let lor_ = ()
let lrem = ()
let lreturn = ()
let lshl = ()
let lshr = ()
let lstore = ()
let lstore_ n = ()
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

let return () = print_endline "return"

let saload = ()
let sastore = ()
let sipush = ()
let swap = ()
let tableswitch = ()
let wide = ()
