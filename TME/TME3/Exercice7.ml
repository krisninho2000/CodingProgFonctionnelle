let rec merge l1 l2 =
  match l1, l2 with
    | [], [] -> []
    | [], h::t -> l2
    | h::t, [] -> l1
    | x::xs, h::t -> if (x < h) then x::(merge xs l2)
                     else h::(merge l1 t)

let merge_new l1 l2 =
  let rec aux l1 l2 =
    match l1, l2 with
    | [], [] -> []
    | [], h::t -> l2
    | h::t, [] -> l1
    | x::xs, h::t -> if (x < h) then x::(aux xs l2)
                     else h::(aux l1 t)
  in
  aux l1 l2

let rec split l1 l2 l =
  match l with
    | [] -> (l1, l2)
    | x::xs -> if (List.length l1 = List.length l2) then (aux (l1@[x]) l2 xs)
               else (aux l1 (l2@[x]) xs)

let split_new liste =
  let rec aux l1 l2 l =
    match l with
      | [] -> (l1, l2)
      | x::xs -> if (List.length l1 = List.length l2) then (aux (l1@[x]) l2 xs)
                 else (aux l1 (l2@[x]) xs)
  in
  aux [] [] liste

let rec padding l1 l2 x =
  if (List.length l1 > List.length l2) then (padding l1 (l2@[x]) x)
  else if (List.length l1 < List.length l2) then (padding (l1@[x]) l2 x)
  else (l1,l2)