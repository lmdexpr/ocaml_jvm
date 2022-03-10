let array_to_string ?(prefix = "") arr f =
  "[\n"
  ^ Array.fold_left (fun acc e -> acc ^ prefix ^ "  " ^ f e ^ ";\n") "" arr
  ^ prefix ^ "]"

module Reader = Reader
