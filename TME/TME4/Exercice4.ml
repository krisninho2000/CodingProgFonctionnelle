type 'a btree =
  | Empty
  | Node of 'a btree * 'a * 'a btree

