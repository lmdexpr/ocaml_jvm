include Result

type 'a t = ('a, exn) result

module Ops = struct
  let ( >>= ) = bind
  let ( let* ) x f = x >>= f
end
