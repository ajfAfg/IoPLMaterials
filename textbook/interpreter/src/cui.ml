open Eval

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let _ =
    print_string "parsing done\n";
    flush stdout
  in
  let id, newenv, v = eval_decl env decl in
  Printf.printf "val %s = " id;
  pp_val v;
  print_newline ();
  read_eval_print newenv

let initial_env =
  List.fold_left
    (fun env (id, exval) -> Environment.extend id exval env)
    Environment.empty
    [
      ("x", IntV 10);
      ("v", IntV 5);
      ("i", IntV 1);
      ("ii", IntV 2);
      ("iii", IntV 3);
      ("iv", IntV 4);
    ]
