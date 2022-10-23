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
  let attribute_name_index = U16.read ic |> U16.to_int in
  let _attribute_length = U32.read ic in
  Cp_info.unwrap_utf8 cp.(attribute_name_index - 1)
  |> Result.map Cp_info.utf8_to_string

let read_verification_type_info ic =
  try
    Result.ok
    @@
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
  with e -> Result.error e

let read_stack_map_frame ic =
  let open Result_ext.Ops in
  let frame_type = U8.read ic |> U8.to_int in
  match frame_type with
  | n when 0 <= n && n <= 63 -> Result.ok Same_frame
  | n when n <= 127 ->
    Result.map (fun v -> Same_locals_1_stack_item_frame v)
    @@ read_verification_type_info ic
  | n when n <= 246 ->
    Result.error @@ invalid_arg "read_stack_map_frame : reserved for future use"
  | 247 ->
    let u16 = U16.read ic in
    let* verification_type_info = read_verification_type_info ic in
    Result.ok
    @@ Same_locals_1_stack_item_frame_extended (u16, verification_type_info)
  | n when n <= 250 -> Result.ok @@ Chop_frame (U16.read ic)
  | 251 -> Result.ok @@ Same_frame_extended (U16.read ic)
  | n when n <= 254 ->
    let u16 = U16.read ic in
    let* arr =
      Result_ext.n_bind (frame_type - 251) (fun _ ->
          read_verification_type_info ic)
    in
    Result.ok @@ Append_frame (u16, arr)
  | 255 ->
    let offset_delta = U16.read ic in
    let number_of_locals = U16.read ic |> U16.to_int in
    let* locals =
      Result_ext.n_bind number_of_locals (fun _ ->
          read_verification_type_info ic)
    in
    let number_of_stack_items = U16.read ic |> U16.to_int in
    let* stack =
      Result_ext.n_bind number_of_stack_items (fun _ ->
          read_verification_type_info ic)
    in
    Result.ok @@ Full_frame { offset_delta; locals; stack }
  | _ -> Result.error @@ invalid_arg "read_stack_map_frame : out of range"

let rec read ic cp =
  let open Result_ext.Ops in
  let* attribute_name = read_attribute_name ic cp in
  match attribute_name with
  | "Code" ->
    let max_stack = U16.read ic |> U16.to_int
    and max_locals = U16.read ic |> U16.to_int
    and code_length = U32.read ic |> U32.to_int in
    let code = Array.init code_length (fun _ -> U8.read ic |> U8.to_int) in
    let exception_table_length = U16.read ic |> U16.to_int in
    let f _ =
      let start_pc = U16.read ic in
      let end_pc = U16.read ic in
      let handler_pc = U16.read ic in
      let catch_type = U16.read ic in
      { start_pc; end_pc; handler_pc; catch_type }
    in
    let exception_table = Array.init exception_table_length f in
    let attributes_count = U16.read ic |> U16.to_int in
    let* attributes = Result_ext.n_bind attributes_count (fun _ -> read ic cp) in
    let code = { max_stack; max_locals; code; exception_table } in
    Result.ok @@ Code (code, attributes)
  | "LineNumberTable" ->
    let line_number_table_length = U16.read ic |> U16.to_int in
    let f _ =
      let start_pc = U16.read ic in
      let line_number = U16.read ic in
      { start_pc; line_number }
    in
    Result.ok @@ Line_number_table (Array.init line_number_table_length f)
  | "SourceFile" -> Result.ok @@ Source_file (U16.read ic)
  | "StackMapTable" ->
    let n = U16.read ic |> U16.to_int in
    let* entries = Result_ext.n_bind n (fun _ -> read_stack_map_frame ic) in
    Result.ok @@ Stack_map_table entries
  | s ->
    print_endline @@ "not implemented read_attribute " ^ s;
    Result.ok Not_implemented
