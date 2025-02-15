(* Factorial Calculation without `rec` *)
(* NOTE: In OCaml, this expression cannot be typed. *)
let makemult maker x = if x < 1 then 0 else 4 + maker maker (x + -1) in
let times4 x = makemult makemult x in
times4 3
;;

(* NOTE: In OCaml, this expression cannot be typed. *)
let makefact maker x = if x < 1 then 1 else x * maker maker (x + -1) in
let fact x = makefact makefact x in
fact 4
