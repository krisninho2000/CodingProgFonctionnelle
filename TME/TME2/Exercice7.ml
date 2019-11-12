let f x a =
  (1. /. 2.) *. (x +. (a /. x))

let rec sqrt_n n a (x : float) =
  match n with
    | 0 -> x
    | r -> (sqrt_n (n - 1) a (f x a))

let eq_eps eps x y =
  if (abs_float(x -. y) < eps) then true
  else false

let rec sqrt_x eps a x =
  if ((eq_eps eps x (f x a)) = true) then x
  else (sqrt_x eps a (f x a))

