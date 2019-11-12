let rec repeat (n : int) (element : 'a) : 'a list =
  if (n <= 0) then []
  else element :: (repeat (n - 1) element)

let () = assert ((repeat 3 "salut") = ["salut";"salut";"salut"])

let rec range (i : int) (j : int) : int list =
  if (i > j) then []
  else i :: (range (i + 1) j)

let () = assert ((range 2 4) = [2;3;4])

let rec range_bis (x : int) (n : int) : int list =
  if (n <= 0) then []
  else x :: (range_bis (x + 1) (n - 1))

let () = assert ((range_bis 2 5) = [2;3;4;5;6])

