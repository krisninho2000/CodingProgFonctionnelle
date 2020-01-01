type 'a ubtree =
  | Empty2
  | Leaf of 'a
  | Node2 of 'a ubtree * 'a * 'a ubtree

let rec hauteur (t : 'a ubtree) : int =
  match t with
    | Empty2 -> 0
    | Leaf _ -> 1
    | Node2(treeg, x, treed) -> 1 + (max (hauteur treed) (hauteur treed))
    | x -> 1
