type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree

let rec lt_btree btree x =
  match btree with
    | Empty -> true
    | Node(Empty,n,Empty) -> if (n < x) then true
                             else false
    | Node(treeg,n,treed) -> if (n < x) then ((lt_btree treeg x) && (lt_btree treed x))
                             else false

let rec ge_btree btree x =
  match btree with
    | Empty -> true
    | Node(Empty,n,Empty) -> if (n >= x) then true
                             else false
    | Node(treeg,n,treed) -> if (n >= x) then ((ge_btree treeg x) && (ge_btree treed x))
                             else false

let rec is_abr btree =
  match btree with
    | Empty -> true
    | Node(treeg,x,treed) -> ((lt_btree treeg x) && (ge_btree treed x))

let rec mem btree x =
  match btree with
    | Empty -> false
    | Node(treeg,n,treed) -> if (n == x) then true
                             else ((mem treeg x) || (mem treed x))