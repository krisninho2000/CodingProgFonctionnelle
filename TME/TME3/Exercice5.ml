let films = [ ("Pulp Fiction", ("Tarantino","Quentin"), 1994, [("Travolta","John") ; ("Thurman","Uma")]);
              ("Psychose" , ("Hitchcock","Alfred") , 1960, [("Perkins","Anthony"); ("Leigh","Janet")]);
              ("Shining" , ("Kubrick","Stanley") , 1980, [("Nicholson","Jack") ; ("Duvall","Shelley")]);
              ("Barry Lyndon", ("Kubrick","Stanley") , 1975, [("Dullea","Keir") ; ("Lockwood","Gary")]);
              ("Grease" , ("Randal","Kleiser") , 1978, [("Travolta","John") ; ("Olivia","Newton-John")]); ]

type film_t = (string * (string * string) * int * ((string * string) list))

let titres (l1 : film_t list) : string list =
  List.map (fun (a, b, c, d) -> a) l1

let () = assert ((titres films) = ["Pulp Fiction"; "Psychose"; "Shining"; "Barry Lyndon"; "Grease"])

let films_1980 (l1 : film_t list) : string list =
  titres (List.filter (fun (a, b, c, d) -> if c = 1980 then true else false) l1)

let () = assert ((films_1980 films) = ["Shining"])

let film_by_actor (l1 : film_t list) (firstname : string) (lastname : string) : string list =
  titres (List.filter (fun (a, b, c, [i; j]) -> if ((i = (lastname, firstname)) || (j = (lastname, firstname))) then true else false) l1)

let () = assert ((film_by_actor films "Uma" "Thurman") = ["Pulp Fiction"])