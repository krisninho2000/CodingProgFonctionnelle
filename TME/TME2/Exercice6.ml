let rec suite_p n =
  match n with
    | 1 -> 1.
    | x -> 1. /. (float_of_int x) +. suite_p (n - 1)

let suite_p_terminale n =
  let rec p_aux acc x =
    match x with
      | 1 -> 1.
      | r -> 1. /. (float_of_int r) +. (p_aux acc (r - 1))
  in
  p_aux 1 n