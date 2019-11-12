let rec less_divider (n : int) (v : int) : int =
  if (n > v) then raise (Invalid_argument "n is greater than v")
  else
    if (n = v) then 0
    else
      if (v mod n = 0) then n
      else less_divider (n + 1) v

let () = assert ((less_divider 3 10) = 5)

let prime n =
  if (less_divider 2 n = 0) then true
  else false

let rec next_prime n =
  if (prime (n+1)) then n+1
  else (next_prime (n+1))

let nth_prime n =
  let rec nth_paux x r acc =
    if (x = r) then acc
    else nth_paux x (r + 1) (next_prime acc)
  in
  nth_paux n 0 2