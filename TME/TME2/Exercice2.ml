let rec u (n : int) : int =
  match n with
    | 0 -> 42
    | x -> (3 * (u (x - 1)) + 4)

let () = assert ((u 4) = 3562)

let u_bis (n : int) : int =
  let rec u_aux (x : int) =
    if (x = 0) then 42
    else (3 * (u (x - 1)) + 4)
  in
    if (n < 0) then raise (Invalid_argument "n is lower than 0")
    else (u_aux n)

let () = assert ((u_bis 4) = 3562)

let rec u_iter n f a =
  if (n > 0) then (u_iter (n - 1) f (f a))
  else a

let () = assert ((u_iter 2 (fun x -> (3 * x + 4)) 42) = 394)