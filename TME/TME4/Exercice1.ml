type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree

let rec taille (t : 'a btree) : (int) =
  match t with
    | Empty -> 0
    | Node(treeg, x, treed) -> 1 + (taille treeg) + (taille treed)

let rec hauteur (t : 'a btree) : (int) =
  match t with
    | Empty -> 0
    | Node(treeg, a, treed) -> 1 + (max (hauteur treeg) (hauteur treed))

let rec list_by_depth (t : 'a btree) (n : int) : ('a list) =
  match t with
    | Empty -> []
    | Node(treeg, a, treed) -> if (n = 0) then
                                [a]
                               else
                                (list_by_depth treeg (n - 1))@(list_by_depth treed (n - 1))

let rec to_list (t : 'a btree) : ('a list) =
  match t with
    | Empty -> []
    | Node(treeg, x, treed) -> [x]@(to_list treeg)@(to_list treed)