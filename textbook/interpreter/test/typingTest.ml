open Miniml

let ty = Syntax.show_ty |> Fmt.of_to_string |> Alcotest.of_pp

let environment =
  Environment.show Syntax.pp_ty |> Fmt.of_to_string |> Alcotest.of_pp

let init_env variable_and_type =
  List.fold_left
    (fun env (id, exval) -> Environment.extend id exval env)
    Environment.empty variable_and_type

let () =
  let open Alcotest in
  run "Typing"
    [
      ( "ty_prim",
        let check = check ty "" in
        [
          test_case "Plus: int -> int -> int" `Quick (fun () ->
              check TyInt @@ Typing.ty_prim Plus TyInt TyInt);
          test_case "Mult: int -> int -> int" `Quick (fun () ->
              check TyInt @@ Typing.ty_prim Mult TyInt TyInt);
          test_case "Eq: int -> int -> int" `Quick (fun () ->
              check TyInt @@ Typing.ty_prim Eq TyInt TyInt);
          test_case "Lt: int -> int -> int" `Quick (fun () ->
              check TyInt @@ Typing.ty_prim Lt TyInt TyInt);
          test_case "And: bool -> bool -> bool" `Quick (fun () ->
              check TyBool @@ Typing.ty_prim And TyBool TyBool);
          test_case "Or: bool -> bool -> bool" `Quick (fun () ->
              check TyBool @@ Typing.ty_prim Or TyBool TyBool);
        ] );
      ( "ty_exp",
        let check = check ty "" in
        [
          test_case "Bound variables can be referenced" `Quick (fun () ->
              let tyenv =
                init_env [ ("y", Syntax.TyInt); ("y", TyBool); ("x", TyInt) ]
              in
              check TyInt @@ Typing.ty_exp tyenv (Var "x");
              check TyBool @@ Typing.ty_exp tyenv (Var "y"));
          test_case "Referencing an unbounded variable raises an exception"
            `Quick (fun () ->
              try
                ignore @@ Typing.ty_exp Environment.empty (Var "x");
                fail "No exception"
              with
              | Typing.Error _ -> ignore pass
              | _ -> fail "Unexpected exception");
          test_case "Literals are mapped to the corresponding types" `Quick
            (fun () ->
              check TyInt @@ Typing.ty_exp Environment.empty (ILit 1);
              check TyBool @@ Typing.ty_exp Environment.empty (BLit true));
          test_case "Inference result is the same of `ty_prim`" `Quick
            (fun () ->
              check (Typing.ty_prim Plus TyInt TyInt)
              @@ Typing.ty_exp Environment.empty (BinOp (Plus, ILit 0, ILit 1));
              check (Typing.ty_prim Mult TyInt TyInt)
              @@ Typing.ty_exp Environment.empty (BinOp (Mult, ILit 0, ILit 1))
              (* NOTE: Omit for other operators *));
          test_case
            "The type of if expression is the same as then and else clauses"
            `Quick (fun () ->
              check TyInt
              @@ Typing.ty_exp Environment.empty
                   (IfExp (BLit true, ILit 1, ILit 0));
              check TyBool
              @@ Typing.ty_exp Environment.empty
                   (IfExp (BLit true, BLit true, BLit false)));
          test_case "The types of then and else clauses must be the same" `Quick
            (fun () ->
              try
                ignore
                @@ Typing.ty_exp Environment.empty
                     (IfExp (BLit true, ILit 1, BLit true));
                fail "No exception"
              with
              | Typing.Error _ -> ignore pass
              | _ -> fail "Unexpected exception");
          test_case "Variables can be referenced after variable binding" `Quick
            (fun () ->
              check TyInt
              @@ Typing.ty_exp Environment.empty
                   (LetExp ([ ("x", ILit 1) ], Var "x")));
        ] );
    ]
