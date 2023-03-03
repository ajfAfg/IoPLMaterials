(* Multiple Variable Declarations *)
let rec even = fun n -> if n = 0 then true else odd (n + -1)
and odd = fun n -> if n = 0 then false else even (n + -1)
;;

even 2;;
odd 2
