module Ops = struct
  let ( let* ) = Result.bind
  let ( and* ) = Result.bind
end

open Ops

let rec n_bind f acc = function
  | 0 -> Result.ok acc
  | n ->
    let* v = f () in
    n_bind f (v :: acc) (n - 1)

let n_bind ~n ~f = Result.map Array.of_list @@ n_bind f [] n
let try_with ~f = try Result.ok @@ f () with e -> Result.error e
