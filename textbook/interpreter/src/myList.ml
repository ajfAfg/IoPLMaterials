let rec remove elem = function
  | [] -> []
  | x :: xs when x = elem -> xs
  | x :: xs -> x :: remove elem xs

let subtract xs ys = List.fold_left (fun acc y -> remove y acc) xs ys
