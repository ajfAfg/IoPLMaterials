(* Let polymorphism *)
let rec f = fun x -> x in
if f true then f 2 else 3
