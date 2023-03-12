let rec read_eval_print' env tyenv =
  print_string "# ";
  flush stdout;
  try
    let program =
      Parser.toplevel_input Lexer.main (Lexing.from_channel stdin)
    in
    let results, env', tyenv' = Run.run_program env tyenv program in
    print_endline @@ Run.string_of_run_results results;
    read_eval_print' env' tyenv'
  with exn ->
    Printf.printf "Fatal error: exception %s\n" (Printexc.to_string exn);
    read_eval_print' env tyenv

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
  let _, env, tyenv =
    MyStdlib.program @ initial_program
    |> Run.run_program Environment.empty Environment.empty
  in
  read_eval_print' env tyenv
