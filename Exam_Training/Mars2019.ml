let nations = [("wa",54);("it",12);("ir",54);("fr",30)] 

let add_to_value k dv kvs =
  if (List.exists (fun (a,b) -> if a = k then true else false) kvs = false) then [(k,dv)]@kvs
  else List.map (fun (a,b) -> if a = k then (a,b+dv) else (a,b)) kvs

let max_key_list kvs =
  let x::xs = kvs in
  let (a,b) = x in
  let rec aux nation max kvs =
    match kvs with
      | [] -> nation
      | (a,b)::t -> if (b > max) then aux [a] b t
                    else if (b = max) then aux ([a]@nation) max t
                    else aux nation max t
  in
  aux [a] b xs