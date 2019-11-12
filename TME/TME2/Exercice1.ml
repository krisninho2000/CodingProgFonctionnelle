let rec fact (n : int) : int =
  match n with
    | 0 -> 1
    | x -> x * (fact (x - 1))

let () = assert ((fact 3) = 6)

let fact_bis (n : int) : int =
  let rec fact_aux (acc : int) =
    if (acc = 0) then 1
    else acc * (fact_aux (acc - 1))
  in
    if (n < 0) then raise (Invalid_argument "n is lower than 0")
    else (fact_aux n)

let () = assert ((fact_bis 3) = 6)