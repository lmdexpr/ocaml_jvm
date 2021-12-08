let array_fill ~n ~f ~init =
  let result = Array.make n init in
  let rec loop i =
    if i < n then
      begin
        result.(i) <- f ();
        loop (i + 1)
      end
    else
      result
  in
    loop 0

