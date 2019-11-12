let desserts =
  [
    ( "gateau chocolat" ,
      [ "chocolat"; "oeuf"; "farine"; "sucre"; "beurre" ] );
    ( "gateau yaourt" ,
      [ "yaourt"; "oeuf"; "farine"; "sucre" ] );
    ( "crepes" ,
      [ "oeuf"; "farine"; "lait" ] );
    ( "quatre-quarts" ,
      [ "oeuf"; "farine"; "beurre"; "sucre" ] );
    ( "kouign amann" ,
      [ "farine"; "beurre"; "sucre" ] )
 ]

type liste_desserts = (string * string list) list

let ingredients rs rname =
  let x::xs = (List.filter (fun (a,b) -> if (a = rname) then true else false) rs) in
  let (x,y) = x in
  y

let nb_ingredients rs rname =
  List.length (ingredients rs rname)

let neo_nb_ingredients rs rname =
  List.length (List.assoc rname rs)

let min_ingredients rs =
  let x::xs = rs in
  let (a,b) = x in
  let rec aux rct rname value =
    match rct with
     | [] -> rname
     | h::t -> let (x,y) = h in
               if (List.length y < value) then aux t x (List.length y)
               else aux t rname value
  in
  aux rs a (List.length b)

let recette_avec rs iname =
  let rec aux rs iname =
    match rs with
      | [] -> []
      | x::xs -> let (a,b) = x in
                 if (List.exists (fun e -> if (e = iname) then true else false) b) then a::(aux xs iname)
                 else (aux xs iname)
  in
  aux rs iname

let add z xs =
  if (List.exists (fun e -> if e = z then true else false) xs) = true then xs
  else xs@[z]

let rec adds zs xs =
  match zs with
    | [] -> xs
    | h::t -> adds t (add h xs)

let rec tous_ingredients rs =
  match rs with
    | [] -> []
    | x::xs -> let (a,b) = x in
               adds b (tous_ingredients xs)

let tous_ingredients_terminale rs =
  let rec aux rs ing =
    match rs with
      | [] -> ing
      | x::xs -> let (a,b) = x in
                 aux xs (adds b ing)
  in
  aux rs []

let table_ingredients rs =
  let ings = tous_ingredients rs in
  List.map (fun a -> (a, recette_avec rs a)) ings