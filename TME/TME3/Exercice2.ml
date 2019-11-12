let rec begaie (l1 : 'a list) : 'a list =
  match l1 with
    | [] -> []
    | x::xs -> x::x::(begaie xs)

let () = assert ((begaie [1;2;3]) = [1;1;2;2;3;3])

let rec somme (l1 : int list) : int =
  match l1 with
    | [] -> 0
    | x::xs -> x + (somme xs)

let somme_terminale liste =
  let rec aux liste v =
    match liste with
      | [] -> v
      | x::xs -> aux xs (x + v)
  in
  aux liste 0

let () = assert ((somme [1;2;3]) = 6)

let rec flatten (l1 : ('a list) list) : 'a list =
  match l1 with
    | [] -> []
    | x::xs -> x@(flatten xs)

let () = assert ((flatten [[1;2];[3;4];[5;6]] = [1;2;3;4;5;6]))

