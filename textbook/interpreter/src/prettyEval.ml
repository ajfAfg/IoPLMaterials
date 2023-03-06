type results = (Syntax.id * Eval.exval) list

let extract_variable_names_to_be_defined = function
  | Syntax.Exp _exp -> []
  | Def bindings -> bindings |> List.map (fun (id, _v) -> id)
  | RecDef bindings -> bindings |> List.map (fun (id, _param, _v) -> id)

let eval_program env program =
  List.fold_left
    (fun (results, env) item ->
      let v, newenv = Eval.eval_item env item in
      let ids = extract_variable_names_to_be_defined item in
      let results' =
        match v with
        | Some v' -> [ ("-", v') ]
        | None -> List.map (fun id -> (id, Environment.lookup id newenv)) ids
      in
      (List.append results results', newenv))
    ([], env) program

let string_of_exval = function
  | Eval.IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV _ -> "<fun>"
  | DProcV _ -> "<dfun>"

let string_of_results results =
  results
  |> List.map (fun (id, exval) ->
         Printf.sprintf "val %s = %s" id @@ string_of_exval exval)
  |> String.concat "\n"
