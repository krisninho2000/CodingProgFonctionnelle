type triplei = (int * int * int)
type triplef = (float * float * float)

let float_of_3int (i3 : triplei) : triplef =
  let (a,b,c) = i3 in
  (float_of_int a, float_of_int b, float_of_int c)

let valeur_poly i3 x =
  let (a,b,c) = float_of_3int i3 in
  (a *. x *. x) +. (b *. x) +. c 

let discriminant i3 =
  let (a,b,c) = i3 in
  b * b - 4 * a * c

let nb_solutions d =
  if (d > 0) then 2
  else if (d = 0) then 1
  else 0

let solutions i3 =
  let (a, b, c) = i3 in
  let delta = discriminant (a, b, c) in
  let nb_s = nb_solutions delta in
  let (af, bf, _) = float_of_3int i3 in
  let df = float_of_int (discriminant i3) in
  
  if (nb_s = 2) then (((-. bf) +. sqrt(df)) /. (2. *. af)), (((-. bf) -. sqrt(df)) /. (2. *. af))
  else if (nb_s = 1) then ( ((-. bf) /. (2. *. af)), ((-. bf) /. (2. *. af)))
  else (0.0, 0.0)

