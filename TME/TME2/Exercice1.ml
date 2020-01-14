let rec fact (n : int) : int =
  match n with
    | 0 -> 1
    | x -> x * (fact (x - 1))

let () = assert ((fact 3) = 6)

let fact_bis (n : int) : int =
  let rec fact_aux (n : int) (acc : int) =
    if (n = 0) then acc
    else (fact_aux (n - 1) (acc * n))
  in
    if (n < 0) then raise (Invalid_argument "n is lower than 0")
    else (fact_aux n 1)

let () = assert ((fact_bis 3) = 6)