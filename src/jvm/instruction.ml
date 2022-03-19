open Classfile
open Utils
open Try.Ops

(* mnemonics *)
let aaload = not_implemented ~name:"aaload"
let aastore = not_implemented ~name:"aastore"
let aconst_null = not_implemented ~name:"aconst_null"
let aload = not_implemented ~name:"aload"
let aload_ n = not_implemented ~name:("aload_" ^ string_of_int n)
let anewarray = not_implemented ~name:"anewarray"
let areturn = not_implemented ~name:"areturn"
let arraylength = not_implemented ~name:"arraylength"
let astore = not_implemented ~name:"astore"
let astore_ = not_implemented ~name:"astore_"
let athrow = not_implemented ~name:"athrow"
let baload = not_implemented ~name:"baload"
let bastore = not_implemented ~name:"bastore"
let bipush frame op = Frame.Int op |> Frame.stack_push frame |> Try.ok
let caload = not_implemented ~name:"caload"
let castore = not_implemented ~name:"castore"
let checkcast = not_implemented ~name:"checkcast"
let d2f = not_implemented ~name:"d2f"
let d2i = not_implemented ~name:"d2i"
let d2l = not_implemented ~name:"d2l"
let dadd = not_implemented ~name:"dadd"
let daload = not_implemented ~name:"daload"
let dastore = not_implemented ~name:"dastore"
let dcmpl = not_implemented ~name:"dcmpl"
let dcmpg = not_implemented ~name:"dcmpg"
let dconst_ = not_implemented ~name:"dconst_"
let ddiv = not_implemented ~name:"ddiv"
let dload = not_implemented ~name:"dload"
let dload_ = not_implemented ~name:"dload_"
let dmul = not_implemented ~name:"dmul"
let dneg = not_implemented ~name:"dneg"
let drem = not_implemented ~name:"drem"
let dreturn = not_implemented ~name:"dreturn"
let dstore = not_implemented ~name:"dstore"
let dstore_ = not_implemented ~name:"dstore_"
let dsub = not_implemented ~name:"dsub"
let dup = not_implemented ~name:"dup"
let dup_x1 = not_implemented ~name:"dup_x1"
let dup_x2 = not_implemented ~name:"dup_x2"
let dup2 = not_implemented ~name:"dup2"
let dup2_x1 = not_implemented ~name:"dup2_x1"
let dup2_x2 = not_implemented ~name:"dup2_x2"
let f2d = not_implemented ~name:"f2d"
let f2i = not_implemented ~name:"f2i"
let f2l = not_implemented ~name:"f2l"
let fadd = not_implemented ~name:"fadd"
let faload = not_implemented ~name:"faload"
let fastore = not_implemented ~name:"fastore"
let fcmpl = not_implemented ~name:"fcmpl"
let fcmpg = not_implemented ~name:"fcmpg"
let fconst_ = not_implemented ~name:"fconst_"
let fdiv = not_implemented ~name:"fdiv"
let fload = not_implemented ~name:"fload"
let fload_ = not_implemented ~name:"fload_"
let fmul = not_implemented ~name:"fmul"
let fneg = not_implemented ~name:"fneg"
let frem = not_implemented ~name:"frem"
let freturn = not_implemented ~name:"freturn"
let fstore = not_implemented ~name:"fstore"
let fstore_ = not_implemented ~name:"fstore_"
let fsub = not_implemented ~name:"fsub"
let getfield = not_implemented ~name:"getfield"

let getstatic frame cp byte1 byte2 =
  Runtime_data_area.get_constant_8 cp byte1 byte2
  |> Runtime_data_area.field_resolution cp
  |> Try.map @@ Frame.stack_push frame

let goto byte1 byte2 = to_signed byte1 byte2 |> Try.ok
let goto_w = not_implemented ~name:"goto_w"
let i2b = not_implemented ~name:"i2b"
let i2c = not_implemented ~name:"i2c"
let i2d = not_implemented ~name:"i2d"
let i2f = not_implemented ~name:"i2f"
let i2l = not_implemented ~name:"i2l"
let i2s = not_implemented ~name:"i2s"
let iadd = not_implemented ~name:"iadd"
let iaload = not_implemented ~name:"iaload"
let iand = not_implemented ~name:"iand"
let iastore = not_implemented ~name:"iastore"

let iconst_ frame n =
  let f () = Frame.Int n |> Frame.stack_push frame |> Try.ok in
  require_in_range ~name:"iconst_" ~lower:(-1) ~n ~upper:5 ~f

let idiv = not_implemented ~name:"idiv"
let if_acmpeq = not_implemented ~name:"if_acmpeq"
let if_acmpne = not_implemented ~name:"if_acmpne"

let if_icmp frame cmp byte1 byte2 =
  let open Frame in
  match stack_pops frame 2 with
  | [ Int v2; Int v1 ] ->
    Try.ok @@ if cmp v1 v2 then to_signed byte1 byte2 else 3
  | _ -> Try.error @@ Invalid_argument "by if_icmp"

let if_ frame cmp byte1 byte2 =
  let open Frame in
  match stack_pops frame 1 with
  | [ Int v ] -> Try.ok @@ if cmp v 0 then to_signed byte1 byte2 else 3
  | _ -> Try.error @@ Invalid_argument "by if_"

let ifnonnull = not_implemented ~name:"ifnonnull"
let ifnull = not_implemented ~name:"ifnull"

let iinc (frame : Frame.t) index const =
  match frame.locals.(index) with
  | Int i -> Try.ok @@ (frame.locals.(index) <- Int (i + const))
  | other ->
    Try.error @@ Invalid_argument ("iinc: " ^ Frame.local_to_string other)

let iload = not_implemented ~name:"iload"

let iload_ frame n =
  let open Frame in
  let f () =
    match frame.locals.(n) with
    | Int value -> Try.ok @@ stack_push frame (Int value)
    | other -> Try.error @@ Invalid_argument ("iload: " ^ local_to_string other)
  in
  require_in_range ~name:"iload_" ~lower:0 ~n ~upper:3 ~f

let imul = not_implemented ~name:"imul"
let ineg = not_implemented ~name:"ineg"
let instanceof = not_implemented ~name:"instanceof"
let invokedynamic = not_implemented ~name:"invokedynamic"
let invokeinterface = not_implemented ~name:"invokeinterface"
let invokespecial _operand1 _operand2 = not_implemented ~name:"invokespecial"
let invokestatic = not_implemented ~name:"invokestatic"

let invokevirtual frame cp op1 op2 =
  let open Runtime_data_area in
  let open Cp_info in
  let get_constant_16 = get_constant_16 cp in
  let* methodref = get_constant_8 cp op1 op2 |> unwrap_methodref in
  let* { name_index; descriptor_index } =
    get_constant_16 methodref.name_and_type_index |> unwrap_name_and_type
  in
  let* method_name = get_constant_16 name_index |> unwrap_utf8 in
  let method_name = utf8_to_string method_name in
  let* arguments = get_constant_16 descriptor_index |> unwrap_utf8 in
  let arguments =
    let open Str in
    match utf8_to_string arguments |> full_split (regexp "[()]") with
    | Delim "(" :: Text s :: Delim ")" :: _ -> split (regexp_string ";") s
    | _ -> invalid_arg @@ utf8_to_string arguments
  in
  List.length arguments |> Frame.stack_pops frame
  |> List.map Frame.to_java_primitive
  |> Java_libs.call method_name |> Try.ok

let ior = not_implemented ~name:"ior"

let irem frame =
  let open Frame in
  match stack_pops frame 2 with
  | [ Int v2; Int v1 ] ->
    Int (v1 - (v1 / v2 * v2)) |> stack_push frame |> Try.ok
  | _ -> Try.error @@ Invalid_argument "irem"

let ireturn = not_implemented ~name:"ireturn"
let ishl = not_implemented ~name:"ishl"
let ishr = not_implemented ~name:"ishr"
let istore = not_implemented ~name:"istore"

let istore_ frame n =
  let open Frame in
  let f () =
    match stack_pop frame with
    | Int value -> Try.ok @@ (frame.locals.(n) <- Int value)
    | other ->
      Try.error
      @@ Invalid_argument ("istore: top of stack is " ^ operand_to_string other)
  in
  require_in_range ~name:"istore_" ~lower:0 ~n ~upper:3 ~f

let isub = not_implemented ~name:"isub"
let iushr = not_implemented ~name:"iushr"
let ixor = not_implemented ~name:"ixor"
let jsr = not_implemented ~name:"jsr"
let jsr_w = not_implemented ~name:"jsr_w"
let l2d = not_implemented ~name:"l2d"
let l2f = not_implemented ~name:"l2f"
let l2i = not_implemented ~name:"l2i"
let ladd = not_implemented ~name:"ladd"
let laload = not_implemented ~name:"laload"
let land_ = not_implemented ~name:"land_"
let lastore = not_implemented ~name:"lastore"
let lcmp = not_implemented ~name:"lcmp"
let lconst_ = not_implemented ~name:"lconst_"

let ldc frame cp operand =
  let open Runtime_data_area in
  let open Cp_info in
  let* str = unwrap_string @@ get_constant cp operand in
  let* str = unwrap_utf8 @@ get_constant_16 cp str in
  Frame.String (utf8_to_string str) |> Frame.stack_push frame |> Try.ok

let ldc_w = not_implemented ~name:"ldc_w"
let ldc2_w = not_implemented ~name:"ldc2_w"
let ldiv = not_implemented ~name:"ldiv"
let lload = not_implemented ~name:"lload"
let lload_ = not_implemented ~name:"lload_"
let lmul = not_implemented ~name:"lmul"
let lneg = not_implemented ~name:"lneg"
let lookupswitch = not_implemented ~name:"lookupswitch"
let lor_ = not_implemented ~name:"lor_"
let lrem = not_implemented ~name:"lrem"
let lreturn = not_implemented ~name:"lreturn"
let lshl = not_implemented ~name:"lshl"
let lshr = not_implemented ~name:"lshr"
let lstore = not_implemented ~name:"lstore"
let lstore_ = not_implemented ~name:"lstore_"
let lsub = not_implemented ~name:"lsub"
let lushr = not_implemented ~name:"lushr"
let lxor_ = not_implemented ~name:"lxor_"
let monitorenter = not_implemented ~name:"monitorenter"
let monitorexit = not_implemented ~name:"monitorexit"
let multianewarray = not_implemented ~name:"multianewarray"
let new_ = not_implemented ~name:"new_"
let newarray = not_implemented ~name:"newarray"
let nop = not_implemented ~name:"nop"
let pop = not_implemented ~name:"pop"
let pop2 = not_implemented ~name:"pop2"
let putfield = not_implemented ~name:"putfield"
let putstatic = not_implemented ~name:"putstatic"
let ret = not_implemented ~name:"ret"
let return = Try.ok
let saload = not_implemented ~name:"saload"
let sastore = not_implemented ~name:"sastore"
let sipush = not_implemented ~name:"sipush"
let swap = not_implemented ~name:"swap"
let tableswitch = not_implemented ~name:"tableswitch"
let wide = not_implemented ~name:"wide"
