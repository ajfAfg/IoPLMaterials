let rec read_eval_print' env =
  print_string "# ";
  flush stdout;
  try
    let program =
      Parser.toplevel_input Lexer.main (Lexing.from_channel stdin)
    in
    let results, newenv = PrettyEval.eval_program env program in
    print_endline @@ PrettyEval.string_of_results results;
    read_eval_print' newenv
  with exn ->
    Printf.printf "Fatal error: exception %s\n" (Printexc.to_string exn);
    read_eval_print' env

let initial_program =
  let open Syntax in
  [
    Def [ ("x", ILit 10) ];
    Def [ ("v", ILit 5) ];
    Def [ ("i", ILit 1) ];
    Def [ ("ii", ILit 2) ];
    Def [ ("iii", ILit 3) ];
    Def [ ("iv", ILit 4) ];
  ]

let read_eval_print () =
  MyStdlib.program @ initial_program
  |> Eval.eval_program Environment.empty
  |> read_eval_print'
