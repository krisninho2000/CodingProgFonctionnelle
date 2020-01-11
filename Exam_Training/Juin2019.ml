let rec decreasing ns =
  if (List.length ns < 2) then true
  else match ns with
    | h::(m::t) -> if (m > h) then false
                   else decreasing (m::t)

let rec diff_list ns =
  if (List.length ns < 2) then []
  else match ns with
    | h::(m::t) -> [h - m]@(diff_list (m::t))

let shrinking ns =
  if (List.length ns <= List.length (diff_list ns)) then false
  else true

type resistance = Res of float
type circuit =
  | Ground
  | Serial of resistance * circuit
  | Parallel of circuit * circuit

let rec court_circuit c =
  match c with
    | Serial(Res(a), c1) -> true
    | Parallel(c1, c2) -> ((court_circuit c1) || (court_circuit c2))
    | Ground -> false

let rec resistance_circuit c =
  match c with
    | Serial(Res(a), c1) -> a +. (resistance_circuit c1)
    | Parallel(c1, c2) -> ((resistance_circuit c1) *. (resistance_circuit c2)) /. ((resistance_circuit c1) +. (resistance_circuit c2))
    | Ground -> 0.

type indom =
  | Host of string
  | Dom of string * indom list

let rec exists_host name it =
  if (it = []) then false
  else
    match it with
      | Host(a) -> if (List.hd name = a) then true else false
      | Dom(s,l) -> if 
