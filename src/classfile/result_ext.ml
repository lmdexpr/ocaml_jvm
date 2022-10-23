module Ops = struct
  let ( let* ) = Result.bind
end

let rec n_bind n f acc =
  if n = 0 then Result.ok acc
  else
    match f () with
    | Ok v -> n_bind (n - 1) f @@ (v :: acc)
    | Error e -> Result.error e

let n_bind n f = Result.map Array.of_list @@ n_bind n f []
