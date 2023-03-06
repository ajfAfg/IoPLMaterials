(* NOTE: No exception handling to perform the same behavior as the original *)
let read_eval_print filename =
  filename |> MyFile.read_whole |> Lexing.from_string
  |> Parser.unit_implementation Lexer.main
  |> PrettyEval.eval_program
       (Eval.eval_program Environment.empty MyStdlib.program)
  |> fst |> PrettyEval.string_of_results |> print_endline
