(* NOTE: No exception handling to perform the same behavior as the original *)
let read_eval_print filename =
  let fst = function x, _, _ -> x in
  let _, env, tyenv =
    Run.run_program Environment.empty Environment.empty MyStdlib.program
  in
  filename |> MyFile.read_whole |> Lexing.from_string
  |> Parser.unit_implementation Lexer.main
  |> Run.run_program env tyenv |> fst |> Run.string_of_run_results
  |> print_endline
