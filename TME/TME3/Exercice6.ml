let liste_int = [1; 2; 3; 4]

let rec somme_list (l1 : int list) : int =
  match l1 with
    | [] -> 0
    | h::t -> h + somme_list(t)

let () = assert ((somme_list [1;2;3]) = 6)

let () = assert ((List.fold_left (fun x y -> (x + y)) 0 liste_int) = 10)

let () = assert ((List.fold_left (fun x y -> (x * y)) 1 liste_int) = 24)

let () = assert ((List.fold_right (fun x y -> (x * y)) liste_int 2) = 48)

let () = assert ((List.fold_right (fun x y -> if x > y then x else y) liste_int 0) = 4)
