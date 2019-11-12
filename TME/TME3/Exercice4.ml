let dico = [ (1, "foo"); (23, "baz"); (6, "bar")];;

let numbers = [ (1, "un"); (1, "one"); (2, "deux"); (2, "two")]

let rec list_assoc (key : 'a) (l1 : ('a * 'b) list) : 'b =
  match l1 with
    | [] -> failwith "Empty List or Key not found"
    | (m, n)::xs -> if (m = key) then n
      else (list_assoc key xs)

let new_list_assoc key liste =
  List.assoc key liste

let () = assert ((list_assoc 2 numbers) = "deux")

let rec list_assocs (key : 'a) (l1 : ('a * 'b) list) : 'b list =
  match l1 with 
    | [] -> []
    | (m,n)::xs -> if (m = key) then n::(list_assocs key xs)
                   else (list_assocs key xs)

let () = assert ((list_assocs 1 numbers) = ["un"; "one"])

let rec list_combine (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match l1,l2 with
    | [],[] -> []
    | [], x::xs -> []
    | x::xs, [] -> []
    | h1::t1, h2::t2 -> let elem = (h1, h2) in
      elem::(list_combine t1 t2)

let new_list_combine l1 l2 =
  List.map2 (fun a b -> (a,b)) l1 l2

let () = assert ((list_combine [1;2;3] ["un";"deux";"trois"]) = [(1, "un"); (2, "deux"); (3, "trois")])

let mem (l1 : 'a list) (key : 'a) : bool =
  let looking_for x = key in
  List.exists looking_for l1