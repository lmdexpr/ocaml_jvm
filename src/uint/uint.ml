module type Basics = sig
  type t

  val bits : int
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
  val to_string_hex : t -> string
  val logor : t -> t -> t
  val shift_left : t -> int -> t
end

module Make (B : Basics) = struct
  type t = B.t

  let of_bytes_little_endian buffer =
    let int_of_pos buffer offset = Char.code (Bytes.get buffer offset) in
    let rec loop buffer i n =
      if i = 0 then n
      else
        let b = B.of_int (int_of_pos buffer (i - 1)) in
        let n = B.logor (B.shift_left n 8) b in
        loop buffer (i - 1) n
    in
    loop buffer (B.bits / 8) B.zero
end

module U8 = struct
  module Basics : sig
    include Basics
  end = struct
    type t = int

    let bits = 8
    let zero = 0
    let of_int i = i
    let to_int i = i
    let to_string = Printf.sprintf "%02d"
    let to_string_hex = Printf.sprintf "%02x"
    let logor x y = x lor y
    let shift_left i offset = (i lsl offset) land 0xff
  end

  include Basics
  include Make (Basics)
end

module U16 = struct
  module Basics : sig
    include Basics
  end = struct
    type t = int

    let bits = 16
    let zero = 0
    let of_int i = i
    let to_int i = i
    let to_string = Printf.sprintf "%04d"
    let to_string_hex = Printf.sprintf "%04x"
    let logor x y = x lor y
    let shift_left i offset = (i lsl offset) land 0xffff
  end

  include Basics
  include Make (Basics)

  let of_u8 u8 = U8.to_int u8 |> of_int
  let ( + ) x y = of_int @@ (to_int x + to_int y)
end

module U32 = struct
  module Basics : sig
    include Basics
  end = struct
    type t = int32

    open Int32

    let bits = 32
    let zero = zero
    let of_int = of_int
    let to_int = to_int
    let to_string = Printf.sprintf "%08ld"
    let to_string_hex = Printf.sprintf "%08lx"
    let logor = logor
    let shift_left i offset = shift_left i offset |> logand @@ of_int 0xffffffff
  end

  include Basics
  include Make (Basics)
end
