(* `( op )` notation *)
let threetimes f x = f (f x x) (f x x) in
threetimes ( + ) 5
;;
