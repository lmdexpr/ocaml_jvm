open Uint

let array_to_debug_string ?(prefix = "") arr f =
  "[\n"
  ^ Array.fold_left (fun acc e -> acc ^ prefix ^ "  " ^ f e ^ ";\n") "" arr
  ^ prefix ^ "]"

module Attribute_info = struct
  open Attribute_info

  let stack_map_frame_to_string ?(prefix = "") = function
    | Same_frame -> "Same_frame"
    | Same_locals_1_stack_item_frame i ->
      "Same_locals_1_stack_item_frame: { "
      ^ verification_type_info_to_string i
      ^ " }\n"
    | Same_locals_1_stack_item_frame_extended (d, i) ->
      "Same_locals_1_stack_item_frame_extended: { " ^ U16.to_string d ^ "; "
      ^ verification_type_info_to_string i
      ^ " }\n"
    | Chop_frame d -> "Chop_frame: { " ^ U16.to_string d ^ " }\n"
    | Same_frame_extended d ->
      "Same_frame_extended: { " ^ U16.to_string d ^ " }\n"
    | Append_frame (offset_delta, locals) ->
      let s = "Full_frame : {\n" in
      let s =
        s ^ prefix ^ "  offset_delta: " ^ U16.to_string offset_delta ^ ";\n"
      in
      let prefix = prefix ^ "  " in
      let s =
        s ^ prefix ^ "  locals: "
        ^ array_to_debug_string ~prefix locals verification_type_info_to_string
        ^ ";\n"
      in
      s
    | Full_frame { offset_delta; locals; stack } ->
      let s = prefix ^ "Full_frame : {\n" in
      let s =
        s ^ prefix ^ "  offset_delta: " ^ U16.to_string offset_delta ^ ";\n"
      in
      let more_nest = prefix ^ "  " in
      let s =
        s ^ prefix ^ "  locals: "
        ^ array_to_debug_string ~prefix:more_nest locals
            verification_type_info_to_string
        ^ ";\n"
      in
      let s =
        s ^ prefix ^ "  stack: "
        ^ array_to_debug_string ~prefix:more_nest stack
            verification_type_info_to_string
        ^ ";\n"
      in
      s ^ prefix ^ "}"

  let rec to_string ?(prefix = "") = function
    | Code (v, attributes) ->
      let s = "Code : {\n" in
      let s =
        s ^ prefix ^ "  max_stack: " ^ string_of_int v.max_stack ^ ";\n"
      in
      let s =
        s ^ prefix ^ "  max_locals: " ^ string_of_int v.max_locals ^ ";\n"
      in
      let more_nest = prefix ^ "  " in
      let s =
        s ^ prefix ^ "  code: "
        ^ array_to_debug_string ~prefix:more_nest v.code string_of_int
        ^ ";\n"
      in
      let e_to_s (e : t_exception) =
        "{ start_pc: " ^ U16.to_string e.start_pc ^ "; end_pc: "
        ^ U16.to_string e.end_pc ^ "; handler_pc: " ^ U16.to_string e.handler_pc
        ^ "; catch_type: " ^ U16.to_string e.catch_type ^ ";}"
      in
      let s =
        s ^ prefix ^ "  exception_table: "
        ^ array_to_debug_string ~prefix:more_nest v.exception_table e_to_s
        ^ ";\n"
      in
      let s =
        s ^ prefix ^ "  attributes: "
        ^ array_to_debug_string ~prefix:more_nest attributes
            (to_string ~prefix:(more_nest ^ "  "))
        ^ ";\n"
      in
      s ^ prefix ^ "}"
    | Line_number_table v ->
      "LineNumberTable : "
      ^ array_to_debug_string ~prefix v (fun e ->
            "{ start_pc: " ^ U16.to_string e.start_pc ^ "; line_number: "
            ^ U16.to_string e.line_number
            ^ " }")
    | Source_file v -> "SourceFile : { " ^ U16.to_string v ^ " }"
    | Stack_map_table v ->
      "StackMapTable: "
      ^ array_to_debug_string ~prefix v
          (stack_map_frame_to_string ~prefix:(prefix ^ "  "))
    | Not_implemented -> "NOT IMPLEMENTED"
end

module Method_info = struct
  open Method_info

  let to_string ?(prefix = "") mi =
    let s = "{\n" in
    let s =
      s ^ prefix ^ "  access_flags: " ^ U16.to_string mi.access_flags ^ ";\n"
    in
    let s = s ^ prefix ^ "  name_index: " ^ mi.name_index ^ ";\n" in
    let s = s ^ prefix ^ "  descriptor_index: " ^ mi.descriptor_index ^ ";\n" in
    let s =
      s ^ prefix ^ "  attributes: "
      ^ array_to_debug_string ~prefix:(prefix ^ "  ") mi.attributes
          (Attribute_info.to_string ~prefix:(prefix ^ "    "))
      ^ ";\n"
    in
    s ^ prefix ^ "}"
end
