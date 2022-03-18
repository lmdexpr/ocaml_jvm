module Import = struct
  module Try = Try
end

module Reader = Reader
module Try = Import.Try

let array_to_string ?(prefix = "") arr f =
  "[\n"
  ^ Array.fold_left (fun acc e -> acc ^ prefix ^ "  " ^ f e ^ ";\n") "" arr
  ^ prefix ^ "]"

let unwind ~(protect : 'a -> unit) f x =
  try
    let y = f x in
    protect x;
    y
  with e ->
    protect x;
    raise e
