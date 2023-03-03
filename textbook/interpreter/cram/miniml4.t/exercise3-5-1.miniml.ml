(* `let rec` declaration / expression *)
let rec fact = fun n -> if n = 0 then 1 else n * (fact (n + (-1))) in
fact 5

let rec fib = fun n -> if n = 1 || n = 2 then 1 else fib (n + -1) + fib (n + -2);;

fib 10
