type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree

type 'a ubtree =
  | Empty2
  | Leaf of 'a
  | Node2 of 'a ubtree * 'a * 'a ubtree

let rec hauteur (t : 'a ubtree) : int =
  match t with
    | Empty2 -> 0
    | Leaf(x) -> 1
    | Node2(treeg, x, treed) -> 1 + (max (hauteur treed) (hauteur treed))

let rec leaves ubtree =
  match ubtree with
    | Empty2 -> []
    | Leaf(x) -> [x]
    | Node2(treeg,x,treed) -> (leaves treeg)@(leaves treed)

let rec bt_to_ubt btree =
  match btree with
    | Empty -> Empty2
    | Node(Empty,x,Empty) -> Leaf(x)
    | Node(treeg,x,Empty) -> Node2((bt_to_ubt treeg),x,Empty2)
    | Node(Empty,x,treed) -> Node2(Empty2,x,(bt_to_ubt treed))
    | Node(treeg,x,treed) -> Node2((bt_to_ubt treeg),x,(bt_to_ubt treed))
