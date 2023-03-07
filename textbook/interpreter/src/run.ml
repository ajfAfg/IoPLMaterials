let extract_variable_names_to_be_defined = function
  | Syntax.Exp _exp -> []
  | Def bindings -> bindings |> List.map (fun (id, _v) -> id)
  | RecDef bindings -> bindings |> List.map (fun (id, _param, _v) -> id)

let run_program env tyenv program =
  List.fold_left
    (fun (results, env, tyenv) item ->
      let ty, tyenv' =
        (* TODO: Unimplemented expression types are represented by `Dummy`. *)
        try Typing.ty_item tyenv item with _ -> (Some Dummy, tyenv)
      in
      let v, env' = Eval.eval_item env item in
      let ids = extract_variable_names_to_be_defined item in
      let results' =
        match (v, ty) with
        | Some v', Some ty' -> [ ("-", v', ty') ]
        | _ ->
            List.map
              (fun id ->
                ( id,
                  Environment.lookup id env',
                  (* TODO: Unimplemented expression types are represented by `Dummy`. *)
                  try Environment.lookup id tyenv' with _ -> Dummy ))
              ids
      in
      (List.append results results', env', tyenv'))
    ([], env, tyenv) program

let string_of_run_results results =
  results
  |> List.map (fun (id, exval, ty) ->
         match ty with
         (* TODO: The type `Dummy` is not printed. *)
         | Syntax.Dummy ->
             Printf.sprintf "val %s = %s" id (Eval.string_of_exval exval)
         | _ ->
             Printf.sprintf "val %s : %s = %s" id (Typing.string_of_ty ty)
               (Eval.string_of_exval exval))
  |> String.concat "\n"
