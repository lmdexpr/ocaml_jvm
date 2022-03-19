include Result

type 'a t = ('a, exn) result

module Ops = struct
  let ( >>= ) = bind
  let ( let* ) x f = x >>= f
end

let get = function
  | Ok v -> v
  | Error e -> raise e
