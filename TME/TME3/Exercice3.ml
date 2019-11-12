let rec drop (n : int) (l1 : 'a list) : 'a list =
  if (List.length l1 < n) then []
  else if (n <= 0) then l1
  else
    match l1 with
      | [] -> []
      | x::xs -> (drop (n - 1) xs)

let () = assert ((drop 2 [1;2;3;4;5;6]) = [3;4;5;6])

let rec take (n : int) (l1 : 'a list) : 'a list =
  if (List.length l1 < n) then l1
  else if (n <= 0) then []
  else 
    match l1 with
      | [] -> []
      | x::xs -> x::(take (n - 1) xs)

let () = assert ((take 2 [1;2;3;4;5;6]) = [1;2])

let sub (i : int) (len : int) (l1 : 'a list) : 'a list =
  if (len + i > List.length l1) then []
  else (take len (drop i l1))

let () = assert ((sub 2 2 [1;2;3;4;5;6]) = [3;4])