type results = (string * Eval.exval) list

val eval_program :
  Eval.exval Environment.t ->
  Syntax.item list ->
  (string * Eval.exval) list * Eval.exval Environment.t

val string_of_results : (string * Eval.exval) list -> string
