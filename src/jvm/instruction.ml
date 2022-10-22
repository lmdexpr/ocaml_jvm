open Classfile

(* utils *)
let ( let* ) = Result.bind
let not_implemented ~name = Result.error @@ Failure ("not_implemented " ^ name)

let to_signed byte1 byte2 =
  let value = (byte1 lsl 8) lor byte2 in
  if value land 0b1000_0000_0000_0000 = 0 then value else value - 0x10000

exception Requirements_failed of string

let require_in_range ~name ~lower ~n ~upper ~f =
  if lower <= n && n <= upper then f ()
  else
    let n = string_of_int n in
    let msg =
      name ^ " require " ^ string_of_int lower ^ " <= n <= "
      ^ string_of_int upper ^ ", actual: " ^ n
    in
    Result.error @@ Requirements_failed msg

(* mnemonics *)
let aaload _frame = not_implemented ~name:"aaload"
let aastore _frame = not_implemented ~name:"aastore"
let aconst_null _frame = not_implemented ~name:"aconst_null"
let aload _frame _index = not_implemented ~name:"aload"
let aload_ n _frame = not_implemented ~name:("aload_" ^ string_of_int n)
let anewarray _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"anewarray"
let areturn _frame = not_implemented ~name:"areturn"
let arraylength _frame = not_implemented ~name:"arraylength"
let astore _frame _index = not_implemented ~name:"astore"
let astore_ _n _frame = not_implemented ~name:"astore_"
let athrow _frame = not_implemented ~name:"athrow"
let baload _frame = not_implemented ~name:"baload"
let bastore _frame = not_implemented ~name:"bastore"
let bipush frame byte = Frame.Int byte |> Frame.stack_push frame |> Result.ok
let caload _frame = not_implemented ~name:"caload"
let castore _frame = not_implemented ~name:"castore"
let checkcast _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"checkcast"
let d2f _frame = not_implemented ~name:"d2f"
let d2i _frame = not_implemented ~name:"d2i"
let d2l _frame = not_implemented ~name:"d2l"
let dadd _frame = not_implemented ~name:"dadd"
let daload _frame = not_implemented ~name:"daload"
let dastore _frame = not_implemented ~name:"dastore"
let dcmp _is_g _frame = not_implemented ~name:"dcmp_"

let dconst_ n _frame =
  let f () = not_implemented ~name:"dconst_" in
  require_in_range ~name:"dconst_" ~lower:0 ~n ~upper:1 ~f

let ddiv _frame = not_implemented ~name:"ddiv"
let dload _frame _index = not_implemented ~name:"dload"

let dload_ n _frame =
  let f () = not_implemented ~name:"dload_" in
  require_in_range ~name:"dload_" ~lower:0 ~n ~upper:3 ~f

let dmul _frame = not_implemented ~name:"dmul"
let dneg _frame = not_implemented ~name:"dneg"
let drem _frame = not_implemented ~name:"drem"
let dreturn _frame = not_implemented ~name:"dreturn"
let dstore _frame _index = not_implemented ~name:"dstore"

let dstore_ n _frame =
  let f () = not_implemented ~name:"dstore_" in
  require_in_range ~name:"dload_" ~lower:0 ~n ~upper:3 ~f

let dsub _frame = not_implemented ~name:"dsub"
let dup _frame = not_implemented ~name:"dup"
let dup_x1 _frame = not_implemented ~name:"dup_x1"
let dup_x2 _frame = not_implemented ~name:"dup_x2"
let dup2 _frame = not_implemented ~name:"dup2"
let dup2_x1 _frame = not_implemented ~name:"dup2_x1"
let dup2_x2 _frame = not_implemented ~name:"dup2_x2"
let f2d _frame = not_implemented ~name:"f2d"
let f2i _frame = not_implemented ~name:"f2i"
let f2l _frame = not_implemented ~name:"f2l"
let fadd _frame = not_implemented ~name:"fadd"
let faload _frame = not_implemented ~name:"faload"
let fastore _frame = not_implemented ~name:"fastore"
let fcmp _is_g _frame = not_implemented ~name:"fcmp_"

let fconst_ n _frame =
  let f () = not_implemented ~name:"fconst_" in
  require_in_range ~name:"iconst_" ~lower:0 ~n ~upper:2 ~f

let fdiv _frame = not_implemented ~name:"fdiv"
let fload _frame _index = not_implemented ~name:"fload"

let fload_ n _frame =
  let f () = not_implemented ~name:"fload_" in
  require_in_range ~name:"fload_" ~lower:0 ~n ~upper:3 ~f

let fmul _frame = not_implemented ~name:"fmul"
let fneg _frame = not_implemented ~name:"fneg"
let frem _frame = not_implemented ~name:"frem"
let freturn _frame = not_implemented ~name:"freturn"
let fstore _frame _index = not_implemented ~name:"fstore"

let fstore_ n _frame =
  let f () = not_implemented ~name:"fstore_" in
  require_in_range ~name:"fstore_" ~lower:0 ~n ~upper:3 ~f

let fsub _frame = not_implemented ~name:"fsub"
let getfield _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"getfield"

let getstatic frame cp indexbyte1 indexbyte2 =
  Runtime_data_area.get_constant_8 cp indexbyte1 indexbyte2
  |> Runtime_data_area.field_resolution cp
  |> Result.map @@ Frame.stack_push frame

let goto branchbyte1 branchbyte2 =
  to_signed branchbyte1 branchbyte2 |> Result.ok

let goto_w _branchbyte1 _branchbyte2 _branchbyte3 _branchbyte4 =
  not_implemented ~name:"goto_w"

let i2b _frame = not_implemented ~name:"i2b"
let i2c _frame = not_implemented ~name:"i2c"
let i2d _frame = not_implemented ~name:"i2d"
let i2f _frame = not_implemented ~name:"i2f"
let i2l _frame = not_implemented ~name:"i2l"
let i2s _frame = not_implemented ~name:"i2s"
let iadd _frame = not_implemented ~name:"iadd"
let iaload _frame = not_implemented ~name:"iaload"
let iand _frame = not_implemented ~name:"iand"
let iastore _frame = not_implemented ~name:"iastore"

let iconst_ n frame =
  let f () = Frame.Int n |> Frame.stack_push frame |> Result.ok in
  require_in_range ~name:"iconst_" ~lower:(-1) ~n ~upper:5 ~f

let idiv _frame = not_implemented ~name:"idiv"

let if_acmp _cond _frame _branchbyte1 _branchbyte2 =
  not_implemented ~name:"if_acmp"

let if_icmp cond frame branchbyte1 branchbyte2 =
  let open Frame in
  match stack_pops frame 2 with
  | [ Int v2; Int v1 ] ->
    Result.ok @@ if cond v1 v2 then to_signed branchbyte1 branchbyte2 else 3
  | _ -> Result.error @@ Invalid_argument "by if_icmp"

let if_ cond frame branchbyte1 branchbyte2 =
  let open Frame in
  match stack_pops frame 1 with
  | [ Int v ] ->
    Result.ok @@ if cond v 0 then to_signed branchbyte1 branchbyte2 else 3
  | _ -> Result.error @@ Invalid_argument "by if_"

let ifnonnull _frame _branchbyte1 _branchbyte2 =
  not_implemented ~name:"ifnonnull"

let ifnull _frame _branchbyte1 _branchbyte2 = not_implemented ~name:"ifnull"

let iinc (frame : Frame.t) index const =
  match frame.locals.(index) with
  | Int i -> Result.ok @@ (frame.locals.(index) <- Int (i + const))
  | other ->
    Result.error @@ Invalid_argument ("iinc: " ^ Frame.local_to_string other)

let iload _frame _index = not_implemented ~name:"iload"

let iload_ n frame =
  let open Frame in
  let f () =
    match frame.locals.(n) with
    | Int value -> Result.ok @@ stack_push frame (Int value)
    | other ->
      Result.error @@ Invalid_argument ("iload: " ^ local_to_string other)
  in
  require_in_range ~name:"iload_" ~lower:0 ~n ~upper:3 ~f

let imul _frame = not_implemented ~name:"imul"
let ineg _frame = not_implemented ~name:"ineg"

let instanceof _frame _indexbyte1 _indexbyte2 =
  not_implemented ~name:"instanceof"

let invokedynamic _frame _indexbyte1 _indexbyte2 _ _ =
  not_implemented ~name:"invokedynamic"

let invokeinterface _frame _indexbyte1 _indexbyte2 _count _ =
  not_implemented ~name:"invokeinterface"

let invokespecial _frame _indexbyte1 _indexbyte2 =
  not_implemented ~name:"invokespecial"

let invokestatic _frame _indexbyte1 _indexbyte2 =
  not_implemented ~name:"invokestatic"

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
  |> Java_libs.call method_name |> Result.ok

let ior _frame = not_implemented ~name:"ior"

let irem frame =
  let open Frame in
  match stack_pops frame 2 with
  | [ Int v2; Int v1 ] ->
    Int (v1 - (v1 / v2 * v2)) |> stack_push frame |> Result.ok
  | _ -> Result.error @@ Invalid_argument "irem"

let ireturn _frame = not_implemented ~name:"ireturn"
let ishl _frame = not_implemented ~name:"ishl"
let ishr _frame = not_implemented ~name:"ishr"
let istore _frame _index = not_implemented ~name:"istore"

let istore_ n frame =
  let open Frame in
  let f () =
    match stack_pop frame with
    | Int value -> Result.ok @@ (frame.locals.(n) <- Int value)
    | other ->
      Result.error
      @@ Invalid_argument ("istore: top of stack is " ^ operand_to_string other)
  in
  require_in_range ~name:"istore_" ~lower:0 ~n ~upper:3 ~f

let isub _frame = not_implemented ~name:"isub"
let iushr _frame = not_implemented ~name:"iushr"
let ixor _frame = not_implemented ~name:"ixor"
let jsr _frame _branchbyte1 _branchbyte2 = not_implemented ~name:"jsr"

let jsr_w _frame _branchbyte1 _branchbyte2 _branchbyte3 _branchbyte4 =
  not_implemented ~name:"jsr_w"

let l2d _frame = not_implemented ~name:"l2d"
let l2f _frame = not_implemented ~name:"l2f"
let l2i _frame = not_implemented ~name:"l2i"
let ladd _frame = not_implemented ~name:"ladd"
let laload _frame = not_implemented ~name:"laload"
let land_ _frame = not_implemented ~name:"land_"
let lastore _frame = not_implemented ~name:"lastore"
let lcmp _frame = not_implemented ~name:"lcmp"

let lconst_ n _frame =
  let f () = not_implemented ~name:"lconst_" in
  require_in_range ~name:"lconst_" ~lower:0 ~n ~upper:1 ~f

let ldc frame cp index =
  let open Runtime_data_area in
  let open Cp_info in
  let* str = unwrap_string @@ get_constant cp index in
  let* str = unwrap_utf8 @@ get_constant_16 cp str in
  Frame.String (utf8_to_string str) |> Frame.stack_push frame |> Result.ok

let ldc_w _frame _cp _indexbyte1 _indexbyte2 = not_implemented ~name:"ldc_w"
let ldc2_w _frame _cp _indexbyte1 _indexbyte2 = not_implemented ~name:"ldc2_w"
let ldiv _frame = not_implemented ~name:"ldiv"
let lload _frame _index = not_implemented ~name:"lload"

let lload_ n _frame =
  let f () = not_implemented ~name:"lload_" in
  require_in_range ~name:"lload_" ~lower:0 ~n ~upper:3 ~f

let lmul _frame = not_implemented ~name:"lmul"
let lneg _frame = not_implemented ~name:"lneg"
let lookupswitch _frame (* todo *) = not_implemented ~name:"lookupswitch"
let lor_ _frame = not_implemented ~name:"lor_"
let lrem _frame = not_implemented ~name:"lrem"
let lreturn _frame = not_implemented ~name:"lreturn"
let lshl _frame = not_implemented ~name:"lshl"
let lshr _frame = not_implemented ~name:"lshr"
let lstore _frame _index = not_implemented ~name:"lstore"

let lstore_ n _frame =
  let f () = not_implemented ~name:"lstore_" in
  require_in_range ~name:"lstore_" ~lower:0 ~n ~upper:3 ~f

let lsub _frame = not_implemented ~name:"lsub"
let lushr _frame = not_implemented ~name:"lushr"
let lxor_ _frame = not_implemented ~name:"lxor_"
let monitorenter _frame = not_implemented ~name:"monitorenter"
let monitorexit _frame = not_implemented ~name:"monitorexit"

let multianewarray _frame _indexbyte1 _indexbyte2 _dimensions =
  not_implemented ~name:"multianewarray"

let new_ _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"new_"
let newarray _frame _atype = not_implemented ~name:"newarray"
let nop = Result.ok 1
let pop _frame = not_implemented ~name:"pop"
let pop2 _frame = not_implemented ~name:"pop2"
let putfield _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"putfield"
let putstatic _frame _indexbyte1 _indexbyte2 = not_implemented ~name:"putstatic"
let ret _frame _index = not_implemented ~name:"ret"
let return = Result.ok
let saload _frame = not_implemented ~name:"saload"
let sastore _frame = not_implemented ~name:"sastore"
let sipush _frame _byte1 _byte2 = not_implemented ~name:"sipush"
let swap _frame = not_implemented ~name:"swap"
let tableswitch _frame (* todo *) = not_implemented ~name:"tableswitch"
let wide _frame (* todo *) = not_implemented ~name:"wide"
