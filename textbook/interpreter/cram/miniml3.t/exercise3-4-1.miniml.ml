(* Functional abstraction and application *)
let f =
  let x = 2 in
  let addx y = x + y in
  addx
in
f 4
;;
