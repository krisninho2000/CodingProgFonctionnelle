let rec prefix1 xs ys =
  match (xs,ys) with
    | ([],[]) -> true
    | ([],h::t) -> true
    | (h::t,[]) -> false
    | (h1::t1, h2::t2) -> if (h1 = h2) then prefix1 t1 t2
                          else false

type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

type dossier = {
  ident : string;
  discipline : string;
  semestres : (string * float) list list
}

let rec val_semestre coefs notes =
  match coefs with
    | [] -> 0.0
    | (m,n)::t -> (n *. (List.assoc m notes)) +. (val_semestre t notes)

let rec somme_semestres coefs notes =
  match notes with
    | [] -> 0.0
    | h::t -> (val_semestre coefs h) +. (somme_semestres coefs t)

let val_dossier param d =
  somme_semestres (List.assoc d.discipline param) d.semestres

let insert n d ds =
  let rec aux n d ds dlst =
    match ds with
      | [] -> (dlst)@[(n,d)]
      | (v,e)::t -> if (n >= v) then (dlst)@[(n,d)]@(ds)
                    else aux n d t ((dlst)@[(v,e)])
  in
  aux n d ds []