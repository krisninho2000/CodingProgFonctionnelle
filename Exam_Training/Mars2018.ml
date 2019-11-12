type move = UP | DOWN | LEFT | RIGHT

let movement = [UP;UP;RIGHT;RIGHT]

let positions = [(2,2);(4,4);(2,5)]

let step m pos =
  let (a,b) = pos in
  if (m = UP) then (a, b+1)
  else if (m = DOWN) then (a, b-1)
  else if (m = RIGHT) then (a+1, b)
  else if (m = LEFT) then (a-1, b)
  else failwith ("Move isn't correct")

let walk pos ms =
  let rec aux pos ms =
    match ms with
      | [] -> []
      | x::xs -> let n = (step x pos) in
                 n::(aux n xs)
  in
  pos::(aux pos ms)

let rec repeat x n =
  match n with
    | 0 -> []
    | v -> x::(repeat x (n-1))

let path_x x1 x2 =
  if (x1 > x2) then repeat LEFT (x1 - x2)
  else if (x1 < x2) then repeat RIGHT (x2 - x1)
  else []

  let path_y y1 y2 =
    if (y1 > y2) then repeat DOWN (y1 - y2)
    else if (y1 < y2) then repeat UP (y2 - y1)
    else []

let path_to pos1 pos2 =
  let (x1,y1) = pos1 in
  let (x2,y2) = pos2 in
  (path_x x1 x2)@(path_y y1 y2)

let rec run posi poslist =
  match poslist with
    | [] -> []
    | x::xs -> let go = path_to posi x in
               (walk posi go)@(run x xs)

let dist pos1 pos2 =
  let (x1,y1) = pos1 in
  let (x2,y2) = pos2 in
  abs(x1-x2)+abs(y1-y2)

let closest pos poslist =
  if (poslist = []) then failwith ("Not_found")
  else
  let x::xs = poslist in
  let rec aux pos poslist close =
    match poslist with
      | [] -> close
      | x::xs -> if (dist pos close > dist pos x) then aux pos xs x
                 else aux pos xs close
  in
  aux pos xs x
  
let rec remove (x : 'a) (xs : 'a list) : 'a list =
  match xs with
    | [] -> []
    | h::t -> if (h = x) then t
              else h::(remove x t)

let neo_run posi poslist =
  let rec aux posi poslist =
    match poslist with
      | [] -> []
      | x::xs -> let go = path_to posi x in
                 (remove x (walk posi go))@(run x xs)
  in
  (aux posi poslist)