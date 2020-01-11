type 'a ltree =
  | Values of 'a list
  | Node of 'a ltree * 'a ltree

let rec list_of bt =
  match bt with
    | Values(a) -> [a]
    | Node(t1,t2) -> (list_of t1)@(list_of t2)

let rec add x bt =
  match bt with
    | Values(a) -> a = [x]@a
    | Node(t1,t2) -> (add x t1) (add x t2)