open Stdint
open Utils.Reader

type t_exception = {
  start_pc : uint16;
  end_pc : uint16;
  handler_pc : uint16;
  catch_type : uint16;
}

type t_line_number = { start_pc : uint16; line_number : uint16 }

type t_code = {
  max_stack : uint16;
  max_locals : uint16;
  code : uint8 array;
  exception_table : t_exception array;
}

type t =
  | Code of t_code * t array
  | Line_number_table of t_line_number array
  | Source_file of uint16
  | Not_implemented

let read_attribute_name ic cp =
  let attribute_name_index = read_u2 ic in
  let _attribute_length = read_u4 ic in
  cp.(Uint16.to_int attribute_name_index - 1) |> Cp_info.utf8_to_string

let rec read ic cp =
  match read_attribute_name ic cp with
  | "Code" ->
      let max_stack = read_u2 ic in
      let max_locals = read_u2 ic in
      let code_length = read_u4 ic in
      let n = Uint32.to_int code_length in
      let code = Array.init n (fun _ -> read_u1 ic) in
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
  | _ -> Not_implemented

let rec to_debug_string ?(prefix = "") = function
  | Code (v, attributes) ->
      let s = "Code : {\n" in
      let s =
        s ^ prefix ^ "  max_stack: " ^ Uint16.to_string v.max_stack ^ ";\n"
      in
      let s =
        s ^ prefix ^ "  max_locals: " ^ Uint16.to_string v.max_locals ^ ";\n"
      in
      let more_nest = prefix ^ "  " in
      let s =
        s ^ prefix ^ "  code: "
        ^ Utils.array_to_string ~prefix:more_nest v.code Uint8.to_string
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
  | Not_implemented -> "NOT IMPLEMENTED"
