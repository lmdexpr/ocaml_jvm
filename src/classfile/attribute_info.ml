open Ubytes.Reader

type t_exception =
  { start_pc : U16.t
  ; end_pc : U16.t
  ; handler_pc : U16.t
  ; catch_type : U16.t
  }

type t_line_number =
  { start_pc : U16.t
  ; line_number : U16.t
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
  | Object_variable_info of U16.t
  | Uninitialized_variable_info of U16.t

let verification_type_info_to_string = function
  | Top_variable_info -> "Top_variable_info"
  | Integer_variable_info -> "Integer_variable_info"
  | Float_variable_info -> "Float_variable_info"
  | Long_variable_info -> "Long_variable_info"
  | Double_variable_info -> "Double_variable_info"
  | Null_variable_info -> "Null_variable_info"
  | UninitializedThis_variable_info -> "UninitializedThis_variable_info"
  | Object_variable_info idx ->
    "Object_variable_info: { " ^ U16.to_string idx ^ " }"
  | Uninitialized_variable_info offset ->
    "Uninitialized_variable_info: { " ^ U16.to_string offset ^ " }"

type t_full_frame =
  { offset_delta : U16.t
  ; locals : t_verification_type_info array
  ; stack : t_verification_type_info array
  }

type t_stack_map_frame =
  | Same_frame
  | Same_locals_1_stack_item_frame of t_verification_type_info
  | Same_locals_1_stack_item_frame_extended of U16.t * t_verification_type_info
  | Chop_frame of U16.t
  | Same_frame_extended of U16.t
  | Append_frame of U16.t * t_verification_type_info array
  | Full_frame of t_full_frame

type t =
  | Code of t_code * t array
  | Line_number_table of t_line_number array
  | Source_file of U16.t
  | Stack_map_table of t_stack_map_frame array
  | Not_implemented

let read_attribute_name ic cp =
  let attribute_name_index = U16.read ic in
  let _attribute_length = U32.read ic in
  cp.(U16.to_int attribute_name_index - 1)
  |> Cp_info.unwrap_utf8
  |> Result.fold ~ok:Cp_info.utf8_to_string ~error:raise

let read_verification_type_info ic =
  match U8.read ic |> U8.to_int with
  | 0 -> Top_variable_info
  | 1 -> Integer_variable_info
  | 2 -> Float_variable_info
  | 3 -> Long_variable_info
  | 4 -> Double_variable_info
  | 5 -> Null_variable_info
  | 6 -> UninitializedThis_variable_info
  | 7 -> Object_variable_info (U16.read ic)
  | 8 -> Uninitialized_variable_info (U16.read ic)
  | _ -> invalid_arg "read_verification_type_info: out of range"

let read_stack_map_frame ic =
  let frame_type = U8.read ic |> U8.to_int in
  if 0 <= frame_type && frame_type <= 63 then Same_frame
  else if frame_type <= 127 then
    Same_locals_1_stack_item_frame (read_verification_type_info ic)
  else if frame_type <= 246 then
    failwith "read_stack_map_frame : reserved for future use"
  else if frame_type = 247 then
    Same_locals_1_stack_item_frame_extended
      (U16.read ic, read_verification_type_info ic)
  else if frame_type <= 250 then Chop_frame (U16.read ic)
  else if frame_type = 251 then Same_frame_extended (U16.read ic)
  else if frame_type <= 254 then
    Append_frame
      ( U16.read ic
      , Array.init (frame_type - 251) (fun _ -> read_verification_type_info ic)
      )
  else if frame_type = 255 then
    let offset_delta = U16.read ic in
    let number_of_locals = U16.read ic |> U16.to_int in
    let locals =
      Array.init number_of_locals (fun _ -> read_verification_type_info ic)
    in
    let number_of_stack_items = U16.read ic |> U16.to_int in
    let stack =
      Array.init number_of_stack_items (fun _ -> read_verification_type_info ic)
    in
    Full_frame { offset_delta; locals; stack }
  else invalid_arg "read_stack_map_frame : out of range"

let rec read ic cp =
  match read_attribute_name ic cp with
  | "Code" ->
    let max_stack = U16.read ic |> U16.to_int in
    let max_locals = U16.read ic |> U16.to_int in
    let code_length = U32.read ic in
    let n = U32.to_int code_length in
    let code = Array.init n (fun _ -> U8.read ic |> U8.to_int) in
    let exception_table_length = U16.read ic in
    let n = U16.to_int exception_table_length in
    let f _ =
      let start_pc = U16.read ic in
      let end_pc = U16.read ic in
      let handler_pc = U16.read ic in
      let catch_type = U16.read ic in
      { start_pc; end_pc; handler_pc; catch_type }
    in
    let exception_table = Array.init n f in
    let attributes_count = U16.read ic in
    let attributes_count = U16.to_int attributes_count in
    let attributes = Array.init attributes_count (fun _ -> read ic cp) in
    let code = { max_stack; max_locals; code; exception_table } in
    Code (code, attributes)
  | "LineNumberTable" ->
    let line_number_table_length = U16.read ic in
    let n = U16.to_int line_number_table_length in
    let f _ =
      let start_pc = U16.read ic in
      let line_number = U16.read ic in
      { start_pc; line_number }
    in
    Line_number_table (Array.init n f)
  | "SourceFile" -> Source_file (U16.read ic)
  | "StackMapTable" ->
    let n = U16.read ic |> U16.to_int in
    let entries = Array.init n (fun _ -> read_stack_map_frame ic) in
    Stack_map_table entries
  | s ->
    print_endline @@ "not implemented read_attribute " ^ s;
    Not_implemented
