let _ =
  let args =
    (* NOTE: `Sys.argv` must contain a command name at the first. *)
    match Array.to_list Sys.argv with
    | [] -> failwith "Shouldn't reach here"
    | _cmd :: args -> args
  in
  match args with
  | [] -> Miniml.Cui.read_eval_print ()
  | filename :: _ -> Miniml.Batch.read_eval_print filename
