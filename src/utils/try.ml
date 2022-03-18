include Result

type 'a t = ('a, exn) result

let not_implemented _ = error @@ Failure "not_implemented"

module Ops = struct
  let ( >>= ) = bind
  let ( let* ) x f = x >>= f
  let return = ok
end
