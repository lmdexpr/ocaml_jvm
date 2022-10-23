module Ops = struct
  let ( let* ) = Result.bind
  let ( and* ) = Result.bind
end

let rec n_bind n f acc =
  if n = 0 then Result.ok acc
  else
    match f () with
    | Ok v -> n_bind (n - 1) f @@ (v :: acc)
    | Error e -> Result.error e

let n_bind n f = Result.map Array.of_list @@ n_bind n f []
let try_with f = try Result.ok @@ f () with e -> Result.error e
