let rec sum_inter (i : int) (j : int) : int =
  if (i > j) then raise (Invalid_argument "i shouldn't be greater than j")
  else
    if (i = j) then i
    else i + (sum_inter (i + 1) j)

let () = assert ((sum_inter 3 5) = 12)

let rec sumf_inter f i j =
  if (i > j) then raise (Invalid_argument "i shouldn't be greater than j")
  else
  if (i = j) then (f i)
  else (f i) + (sumf_inter f (i + 1) j)

let () = assert ((sumf_inter (fun x -> 10 * x) 3 5) = 120)