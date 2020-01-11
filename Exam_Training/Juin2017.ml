type bureau = {
  inscrits : int;
  votes : (string * int) list
}

let rec somme_votes votes =
  match votes with
    | [] -> 0
    | (n,v)::t -> v + (somme_votes t)

let rec somme_inscrits (bs : bureau list) : int =
  match bs with
    | [] -> 0
    | h::t -> (h.inscrits) + (somme_inscrits t)

let add_voix_candidat (nom : string) (voix : int) (votes : (string * int) list) : (string * int) list =
  List.append [(nom, voix)] votes

let rec add_voix_bureau votes_bureau votes_circo =
  match votes_bureau with
    | [] -> votes_circo
    | (n,v)::t -> if ((List.filter (fun (a,b) -> (a = n)) votes_circo) = []) then add_voix_bureau t (List.append [(n,v)] votes_circo)
               else add_voix_bureau t (List.map (fun (a,b) -> if (a = n) then (a, b + v) else (a,b)) votes_circo)

let votes_circo bs =
  let rec aux bs lst =
    match bs with
      | [] -> lst
      | h::t -> aux t (add_voix_bureau h.votes lst)
  in
  aux bs []

let premier_tour nb_voix nb_votes =
  if (nb_voix > nb_votes) then failwith "Data error"
  else (float_of_int (nb_voix) >= float_of_int (nb_votes) *. 0.5)

let rec elu_premier_tour votes nb_votes =
  match votes with
    | [] -> "None"
    | (n,v)::t -> if (premier_tour v nb_votes) then (String.concat "" ["Candidat Elu - ";n])
                  else elu_premier_tour t nb_votes

let second_tour nb_voix nb_inscrits =
  if (nb_voix > nb_inscrits) then failwith "Data error"
  else (float_of_int (nb_voix) >= float_of_int (nb_inscrits) *. 0.125)

let rec candidats_second_tour votes nb_inscrits =
  match votes with
    | [] -> []
    | (n,v)::t -> if (second_tour v nb_inscrits) then [n]@(candidats_second_tour t nb_inscrits)
                  else candidats_second_tour t nb_inscrits

type resultat =
  | Elu_premier_tour of string
  | Candidats_second_tour of string list

let resultat_premier_tour bs =
  let rec aux v_lst lst =
    match v_lst with
      | [] -> Candidats_second_tour(lst)
      | (n,v)::t -> if (premier_tour v (somme_inscrits bs)) then Elu_premier_tour(n)
                    else if (second_tour v (somme_inscrits bs)) then aux t ([n]@lst)
                    else aux t lst
  in
  aux (votes_circo bs) []