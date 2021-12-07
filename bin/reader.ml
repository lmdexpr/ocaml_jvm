open Stdint
open Structures

let read_byte ic = try Some (input_char ic) with End_of_file -> None

let read_bytes ic length =
  let rec loop count acc = 
    if count > 0 then
      match read_byte ic with
      | Some b -> loop (count - 1) (Seq.cons b acc)
      | None -> acc
    else
      acc
  in
  loop length Seq.empty |> Bytes.of_seq

let read_u1 ic = read_bytes ic 1 |> fun bs ->  Uint8.of_bytes_little_endian bs 0
let read_u2 ic = read_bytes ic 2 |> fun bs -> Uint16.of_bytes_little_endian bs 0
let read_u4 ic = read_bytes ic 4 |> fun bs -> Uint32.of_bytes_little_endian bs 0

let times ~n ~f ~init =
  let result = Array.make n init in
  let rec loop i =
    if i < n then
      begin
        result.(i) <- f ();
        loop (i + 1)
      end
    else
      result
  in
    loop 0

let read_fieldref ic : c_fieldref_v =
  { class_index = read_u2 ic; name_and_type_index = read_u2 ic }
let read_methodref ic : c_methodref_v =
  { class_index = read_u2 ic; name_and_type_index = read_u2 ic }
let read_interface_methodref ic : c_interface_methodref_v =
  { class_index = read_u2 ic; name_and_type_index = read_u2 ic }

let read_long   ic : c_long_v   = { high_bytes = read_u4 ic; low_bytes = read_u4 ic }
let read_double ic : c_double_v = { high_bytes = read_u4 ic; low_bytes = read_u4 ic }

let read_name_and_type ic = { name_index = read_u2 ic; descriptor_index = read_u2 ic }

let read_utf8 ic = 
  let len = read_u2 ic in
  let n   = Uint16.to_int len in
  { length = len; byte_array = times ~n ~init:Uint8.zero ~f:(fun _ -> read_u1 ic) }

let read_method_handle ic = { reference_kind = read_u1 ic ; reference_index = read_u2 ic }
let read_method_type   ic = { descriptor_index = read_u2 ic }

let read_dynamic ic : c_dynamic_v =
  { bootstrap_method_attr_index = read_u2 ic; name_and_type_index = read_u2 ic }
let read_invoke_dynamic ic : c_invoke_dynamic_v =
  { bootstrap_method_attr_index = read_u2 ic; name_and_type_index = read_u2 ic }

exception Illegal_constant_pool
let read_cp_info ic n : cp_info array =
  let f () =
    match read_byte ic |> Option.get |> int_of_char with
    |  7 -> C_class               (read_u2 ic)
    |  9 -> C_fieldref            (read_fieldref ic)
    | 10 -> C_methodref           (read_methodref ic)
    | 11 -> C_interface_methodref (read_interface_methodref ic)
    |  8 -> C_string              (read_u2 ic)
    |  3 -> C_integer             (read_u4 ic)
    |  4 -> C_float               (read_u4 ic)
    |  5 -> C_long                (read_long ic)
    |  6 -> C_double              (read_double ic)
    | 12 -> C_name_and_type       (read_name_and_type ic)
    |  1 -> C_utf8                (read_utf8 ic)
    | 15 -> C_method_handle       (read_method_handle ic)
    | 16 -> C_method_type         (read_method_type ic)
    | 17 -> C_dynamic             (read_dynamic ic)
    | 18 -> C_invoke_dynamic      (read_invoke_dynamic ic)
    | 19 -> C_module              (read_u2 ic)
    | 20 -> C_package             (read_u2 ic)
    | _  -> raise Illegal_constant_pool
  in
    times ~n ~init:C_dummy ~f

let read_class_file ic =
  let magic               = read_u4 ic in
  let minor_version       = read_u2 ic in
  let major_version       = read_u2 ic in
  let constant_pool_count = read_u2 ic in
  let constant_pool       = Uint16.to_int constant_pool_count - 1 |> read_cp_info ic in
  begin 
    Uint32.to_string_hex magic |> print_endline;
    Uint16.to_string_hex minor_version |> print_endline;
    Uint16.to_string_hex major_version |> print_endline;
    Uint16.to_string_hex constant_pool_count |> print_endline;
    print_endline "constant_pool {";
    constant_pool |> Array.iter (function
      | C_dummy                 -> print_endline "  C_dummy"
      | C_class               v -> Printf.printf "  C_class %s\n" (Uint16.to_string v) 
      | C_fieldref            v -> Printf.printf "  C_fieldref %s %s\n" (Uint16.to_string v.class_index) (Uint16.to_string v.name_and_type_index)
      | C_methodref           v -> Printf.printf "  C_methodref %s %s\n" (Uint16.to_string v.class_index) (Uint16.to_string v.name_and_type_index)
      | C_interface_methodref _ -> print_endline "  C_interface_methodref"
      | C_string              v -> Printf.printf "  C_string %s\n" (Uint16.to_string v)
      | C_integer             _ -> print_endline "  C_integer"
      | C_float               _ -> print_endline "  C_float"
      | C_long                _ -> print_endline "  C_long"
      | C_double              _ -> print_endline "  C_double"
      | C_name_and_type       v -> Printf.printf "  C_name_and_type %s %s\n" (Uint16.to_string v.name_index) (Uint16.to_string v.descriptor_index)
      | C_utf8                v -> Printf.printf "  C_utf8 %d %s\n" (Uint16.to_int v.length) (Array.fold_left (fun s b -> s ^ (Uint8.to_int b |> Char.chr |> Char.escaped)) "" v.byte_array)
      | C_method_handle       _ -> print_endline "  C_method_handle"
      | C_method_type         _ -> print_endline "  C_method_type"
      | C_dynamic             _ -> print_endline "  C_dynamic"
      | C_invoke_dynamic      _ -> print_endline "  C_invoke_dynamic"
      | C_module              _ -> print_endline "  C_module"
      | C_package             _ -> print_endline "  C_package"
    );
    print_endline "}"
  end
