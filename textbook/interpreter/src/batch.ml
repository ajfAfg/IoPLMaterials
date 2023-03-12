(* TODO:
   When type inference of variable definitions becomes possible,
   there is no need to declare types explicitly. *)
let initial_tyenv =
  let open Syntax in
  List.fold_left
    (fun tyenv (id, ty) -> Environment.extend id ty tyenv)
    Environment.empty
    [
      ("+", TyFun (TyInt, TyFun (TyInt, TyInt)));
      ("*", TyFun (TyInt, TyFun (TyInt, TyInt)));
      ("=", TyFun (TyInt, TyFun (TyInt, TyBool)));
      ("<", TyFun (TyInt, TyFun (TyInt, TyBool)));
      ("&&", TyFun (TyBool, TyFun (TyBool, TyBool)));
      ("||", TyFun (TyBool, TyFun (TyBool, TyBool)));
    ]

(* NOTE: No exception handling to perform the same behavior as the original *)
let read_eval_print filename =
  let fst = function x, _, _ -> x in
  let env = Eval.eval_program Environment.empty MyStdlib.program in
  filename |> MyFile.read_whole |> Lexing.from_string
  |> Parser.unit_implementation Lexer.main
  |> Run.run_program env initial_tyenv
  |> fst |> Run.string_of_run_results |> print_endline
