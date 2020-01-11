type table = {
  names : string list;
  mutable records : (string array) list
}

let tbl = {
  names = ["Nom"; "Prenom"; "Tél"; "Email"; "Année"];
  records = [[|"P"; "C"; "C"; "C"; "C"|];[|"P1"; "C1"; "C1"; "C1"; "C1"|]]
};;

let index z xs =
  let rec aux elem lst ind =
    match lst with
      | h::t -> if (h = elem) then ind
                else aux elem t (ind + 1)
      | [] -> failwith "Element Not Found"
  in
  aux z xs 0

let rec indexes zs xs =
  match zs with
    | [] -> []
    | h::t -> (index h xs)::(indexes t xs)

let rec proj is r =
  match is with
    | [] -> []
    | h::t -> (Array.get r h)::(proj t r)

let select (tbl : table) (ns : string list) : (string list) list =
  let index_list = indexes ns tbl.names in
  let rec aux ind_l tab_list lst =
    match tab_list with
      | h::t -> aux ind_l t ([(proj ind_l h)]@lst)
      | [] -> (List.rev lst)
  in
  aux index_list (tbl.records) []
