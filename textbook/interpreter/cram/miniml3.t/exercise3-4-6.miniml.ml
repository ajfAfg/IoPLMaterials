(* Factorial Calculation using functional abstraction that perform dynamic binding *)
let fact = fun n -> n + 1 in
let fact = fun n -> if n < 1 then 1 else n * fact (n + -1) in
fact 5
;;

let fact = dfun n -> n + 1 in
let fact = fun n -> if n < 1 then 1 else n * fact (n + -1) in
fact 5
;;

let fact = fun n -> n + 1 in
let fact = dfun n -> if n < 1 then 1 else n * fact (n + -1) in
fact 5
;;

let fact = dfun n -> n + 1 in
let fact = dfun n -> if n < 1 then 1 else n * fact (n + -1) in
fact 5
