open Stdint
open Utils.Reader

type t_exception =
  { start_pc : uint16
  ; end_pc : uint16
  ; handler_pc : uint16
  ; catch_type : uint16
  }

type t_line_number =
  { start_pc : uint16
  ; line_number : uint16
  }

type t_code =
  { max_stack : int
  ; max_locals : int
  ; code : int array
  ; exception_table : t_exception array
  }

type t_verification_type_info =
  | Top_variable_info
  | Integer_variable_info
  | Float_variable_info
  | Long_variable_info
  | Double_variable_info
  | Null_variable_info
  | UninitializedThis_variable_info
  | Object_variable_info of uint16
  | Uninitialized_variable_info of uint16

type t_full_frame =
  { offset_delta : uint16
  ; locals : t_verification_type_info array
  ; stack : t_verification_type_info array
  }

type t_stack_map_frame =
  | Same_frame
  | Same_locals_1_stack_item_frame of t_verification_type_info
  | Same_locals_1_stack_item_frame_extended of uint16 * t_verification_type_info
  | Chop_frame of uint16
  | Same_frame_extended of uint16
  | Append_frame of uint16 * t_verification_type_info array
  | Full_frame of t_full_frame

type t =
  | Code of t_code * t array
  | Line_number_table of t_line_number array
  | Source_file of uint16
  | Stack_map_table of t_stack_map_frame array
  | Not_implemented

let read_attribute_name ic cp =
  let attribute_name_index = read_u2 ic in
  let _attribute_length = read_u4 ic in
  cp.(Uint16.to_int attribute_name_index - 1)
  |> Cp_info.unwrap_utf8
  |> Result.fold ~ok:Cp_info.utf8_to_string ~error:raise

let read_verification_type_info ic =
  match read_u1 ic |> Uint8.to_int with
  | 0 -> Top_variable_info
  | 1 -> Integer_variable_info
  | 2 -> Float_variable_info
  | 3 -> Long_variable_info
  | 4 -> Double_variable_info
  | 5 -> Null_variable_info
  | 6 -> UninitializedThis_variable_info
  | 7 -> Object_variable_info (read_u2 ic)
  | 8 -> Uninitialized_variable_info (read_u2 ic)
  | _ -> invalid_arg "read_verification_type_info: out of range"

let read_stack_map_frame ic =
  let frame_type = read_u1 ic |> Uint8.to_int in
  if 0 <= frame_type && frame_type <= 63 then Same_frame
  else if frame_type <= 127 then
    Same_locals_1_stack_item_frame (read_verification_type_info ic)
  else if frame_type <= 246 then
    failwith "read_stack_map_frame : reserved for future use"
  else if frame_type = 247 then
    Same_locals_1_stack_item_frame_extended
      (read_u2 ic, read_verification_type_info ic)
  else if frame_type <= 250 then Chop_frame (read_u2 ic)
  else if frame_type = 251 then Same_frame_extended (read_u2 ic)
  else if frame_type <= 254 then
    Append_frame
      ( read_u2 ic
      , Array.init (frame_type - 251) (fun _ -> read_verification_type_info ic)
      )
  else if frame_type = 255 then
    let offset_delta = read_u2 ic in
    let number_of_locals = read_u2 ic |> Uint16.to_int in
    let locals =
      Array.init number_of_locals (fun _ -> read_verification_type_info ic)
    in
    let number_of_stack_items = read_u2 ic |> Uint16.to_int in
    let stack =
      Array.init number_of_stack_items (fun _ -> read_verification_type_info ic)
    in
    Full_frame { offset_delta; locals; stack }
  else invalid_arg "read_stack_map_frame : out of range"

let rec read ic cp =
  match read_attribute_name ic cp with
  | "Code" ->
    let max_stack = read_u2 ic |> Uint16.to_int in
    let max_locals = read_u2 ic |> Uint16.to_int in
    let code_length = read_u4 ic in
    let n = Uint32.to_int code_length in
    let code = Array.init n (fun _ -> read_u1 ic |> Uint8.to_int) in
    let exception_table_length = read_u2 ic in
    let n = Uint16.to_int exception_table_length in
    let f _ =
      let start_pc = read_u2 ic in
      let end_pc = read_u2 ic in
      let handler_pc = read_u2 ic in
      let catch_type = read_u2 ic in
      { start_pc; end_pc; handler_pc; catch_type }
    in
    let exception_table = Array.init n f in
    let attributes_count = read_u2 ic in
    let attributes_count = Uint16.to_int attributes_count in
    let attributes = Array.init attributes_count (fun _ -> read ic cp) in
    let code = { max_stack; max_locals; code; exception_table } in
    Code (code, attributes)
  | "LineNumberTable" ->
    let line_number_table_length = read_u2 ic in
    let n = Uint16.to_int line_number_table_length in
    let f _ =
      let start_pc = read_u2 ic in
      let line_number = read_u2 ic in
      { start_pc; line_number }
    in
    Line_number_table (Array.init n f)
  | "SourceFile" -> Source_file (read_u2 ic)
  | "StackMapTable" ->
    let n = read_u2 ic |> Uint16.to_int in
    let entries = Array.init n (fun _ -> read_stack_map_frame ic) in
    Stack_map_table entries
  | s ->
    print_endline @@ "not implemented read_attribute " ^ s;
    Not_implemented

let verification_type_info_to_string = function
  | Top_variable_info -> "Top_variable_info"
  | Integer_variable_info -> "Integer_variable_info"
  | Float_variable_info -> "Float_variable_info"
  | Long_variable_info -> "Long_variable_info"
  | Double_variable_info -> "Double_variable_info"
  | Null_variable_info -> "Null_variable_info"
  | UninitializedThis_variable_info -> "UninitializedThis_variable_info"
  | Object_variable_info idx ->
    "Object_variable_info: { " ^ Uint16.to_string idx ^ " }"
  | Uninitialized_variable_info offset ->
    "Uninitialized_variable_info: { " ^ Uint16.to_string offset ^ " }"

let stack_map_frame_to_string ?(prefix = "") = function
  | Same_frame -> "Same_frame"
  | Same_locals_1_stack_item_frame i ->
    "Same_locals_1_stack_item_frame: { "
    ^ verification_type_info_to_string i
    ^ " }\n"
  | Same_locals_1_stack_item_frame_extended (d, i) ->
    "Same_locals_1_stack_item_frame_extended: { " ^ Uint16.to_string d ^ "; "
    ^ verification_type_info_to_string i
    ^ " }\n"
  | Chop_frame d -> "Chop_frame: { " ^ Uint16.to_string d ^ " }\n"
  | Same_frame_extended d ->
    "Same_frame_extended: { " ^ Uint16.to_string d ^ " }\n"
  | Append_frame (offset_delta, locals) ->
    let s = "Full_frame : {\n" in
    let s =
      s ^ prefix ^ "  offset_delta: " ^ Uint16.to_string offset_delta ^ ";\n"
    in
    let prefix = prefix ^ "  " in
    let s =
      s ^ prefix ^ "  locals: "
      ^ Utils.array_to_string ~prefix locals verification_type_info_to_string
      ^ ";\n"
    in
    s
  | Full_frame { offset_delta; locals; stack } ->
    let s = prefix ^ "Full_frame : {\n" in
    let s =
      s ^ prefix ^ "  offset_delta: " ^ Uint16.to_string offset_delta ^ ";\n"
    in
    let more_nest = prefix ^ "  " in
    let s =
      s ^ prefix ^ "  locals: "
      ^ Utils.array_to_string ~prefix:more_nest locals
          verification_type_info_to_string
      ^ ";\n"
    in
    let s =
      s ^ prefix ^ "  stack: "
      ^ Utils.array_to_string ~prefix:more_nest stack
          verification_type_info_to_string
      ^ ";\n"
    in
    s ^ prefix ^ "}"

let rec to_debug_string ?(prefix = "") = function
  | Code (v, attributes) ->
    let s = "Code : {\n" in
    let s = s ^ prefix ^ "  max_stack: " ^ string_of_int v.max_stack ^ ";\n" in
    let s =
      s ^ prefix ^ "  max_locals: " ^ string_of_int v.max_locals ^ ";\n"
    in
    let more_nest = prefix ^ "  " in
    let s =
      s ^ prefix ^ "  code: "
      ^ Utils.array_to_string ~prefix:more_nest v.code string_of_int
      ^ ";\n"
    in
    let e_to_s (e : t_exception) =
      "{ start_pc: "
      ^ Uint16.to_string e.start_pc
      ^ "; end_pc: " ^ Uint16.to_string e.end_pc ^ "; handler_pc: "
      ^ Uint16.to_string e.handler_pc
      ^ "; catch_type: "
      ^ Uint16.to_string e.catch_type
      ^ ";}"
    in
    let s =
      s ^ prefix ^ "  exception_table: "
      ^ Utils.array_to_string ~prefix:more_nest v.exception_table e_to_s
      ^ ";\n"
    in
    let s =
      s ^ prefix ^ "  attributes: "
      ^ Utils.array_to_string ~prefix:more_nest attributes
          (to_debug_string ~prefix:(more_nest ^ "  "))
      ^ ";\n"
    in
    s ^ prefix ^ "}"
  | Line_number_table v ->
    "LineNumberTable : "
    ^ Utils.array_to_string ~prefix v (fun e ->
          "{ start_pc: "
          ^ Uint16.to_string e.start_pc
          ^ "; line_number: "
          ^ Uint16.to_string e.line_number
          ^ " }")
  | Source_file v -> "SourceFile : { " ^ Uint16.to_string v ^ " }"
  | Stack_map_table v ->
    "StackMapTable: "
    ^ Utils.array_to_string ~prefix v
        (stack_map_frame_to_string ~prefix:(prefix ^ "  "))
  | Not_implemented -> "NOT IMPLEMENTED"
