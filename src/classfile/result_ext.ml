module Ops = struct
  let ( let* ) = Result.bind
  let ( and* ) = Result.bind
  let ( let+ ) x k = Result.map k x
  let ( and+ ) x k = Result.map k x
end

open Ops

let rec n_bind f acc = function
  | 0 -> Result.ok acc
  | n ->
    let* v = f () in
    n_bind f (v :: acc) (n - 1)

let n_bind ~n ~f =
  let* xs = n_bind f [] n in
  let arr = List.rev xs |> Array.of_list in
  Result.ok arr

let try_with ~f = try Result.ok @@ f () with e -> Result.error e
