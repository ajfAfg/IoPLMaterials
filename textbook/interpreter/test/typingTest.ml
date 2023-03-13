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
        let check = check (pair (list (pair ty ty)) ty) "" in
        [
          test_case "Returns constraints and a return type" `Quick (fun () ->
              let open Syntax in
              check ([ (TyInt, TyInt); (TyInt, TyInt) ], TyInt)
              @@ Typing.ty_prim Plus TyInt TyInt;
              check ([ (TyInt, TyInt); (TyInt, TyInt) ], TyInt)
              @@ Typing.ty_prim Mult TyInt TyInt;
              check ([ (TyInt, TyInt); (TyInt, TyInt) ], TyBool)
              @@ Typing.ty_prim Eq TyInt TyInt;
              check ([ (TyInt, TyInt); (TyInt, TyInt) ], TyBool)
              @@ Typing.ty_prim Lt TyInt TyInt;
              check ([ (TyBool, TyBool); (TyBool, TyBool) ], TyBool)
              @@ Typing.ty_prim And TyBool TyBool;
              check ([ (TyBool, TyBool); (TyBool, TyBool) ], TyBool)
              @@ Typing.ty_prim Or TyBool TyBool);
        ] );
      ( "ty_exp",
        let check = check ty "" in
        [
          test_case "Bound variables can be referenced" `Quick (fun () ->
              let tyenv =
                init_env [ ("y", Syntax.TyInt); ("y", TyBool); ("x", TyInt) ]
              in
              check TyInt @@ snd (Typing.ty_exp tyenv (Var "x"));
              check TyBool @@ snd (Typing.ty_exp tyenv (Var "y")));
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
              check TyInt @@ snd (Typing.ty_exp Environment.empty (ILit 1));
              check TyBool @@ snd (Typing.ty_exp Environment.empty (BLit true)));
          test_case "Inference result is the same of `ty_prim`" `Quick
            (fun () ->
              let open Syntax in
              check (snd (Typing.ty_prim Plus TyInt TyInt))
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (BinOp (Plus, ILit 0, ILit 1)));
              check (snd (Typing.ty_prim Mult TyInt TyInt))
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (BinOp (Mult, ILit 0, ILit 1)))
              (* NOTE: Omit for other operators *));
          test_case
            "The type of if expression is the same as then and else clauses"
            `Quick (fun () ->
              check TyInt
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (IfExp (BLit true, ILit 1, ILit 0)));
              check TyBool
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (IfExp (BLit true, BLit true, BLit false))));
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
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (LetExp ([ ("x", ILit 1) ], Var "x"))));
          test_case "Monomorphic type can be inferred" `Quick (fun () ->
              check (TyFun (TyInt, TyInt))
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (FunExp ("x", BinOp (Plus, Var "x", ILit 1))));
              check (TyFun (TyInt, TyFun (TyInt, TyInt)))
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y"))))));
          test_case "Return type can be inferred" `Quick (fun () ->
              check TyInt
              @@ snd
                   (Typing.ty_exp Environment.empty
                      (AppExp
                         (FunExp ("x", BinOp (Plus, Var "x", ILit 1)), ILit 2))));
        ] );
      ( "ty_item",
        let check = check ty "" in
        [
          test_case "Types of recursive definitions can be inferred" `Quick
            (fun () ->
              let _, tyenv =
                Typing.ty_item Environment.empty
                @@ RecDef
                     [
                       ( "even",
                         "n",
                         IfExp
                           ( BinOp (Eq, Var "n", ILit 0),
                             BLit true,
                             AppExp (Var "odd", BinOp (Plus, Var "n", ILit ~-1))
                           ) );
                       ( "odd",
                         "n",
                         IfExp
                           ( BinOp (Eq, Var "n", ILit 0),
                             BLit false,
                             AppExp (Var "even", BinOp (Plus, Var "n", ILit ~-1))
                           ) );
                     ]
              in
              check (TyFun (TyInt, TyBool)) @@ Environment.lookup "even" tyenv;
              check (TyFun (TyInt, TyBool)) @@ Environment.lookup "odd" tyenv);
        ] );
      ( "fresh_tyvar",
        let check = check int "" in
        [
          test_case "Generated `tyvar`s are not duplicated" `Quick (fun () ->
              let len = 100 in
              check len
              @@ (List.init len (fun _ -> Typing.fresh_tyvar ())
                 |> List.sort_uniq compare |> List.length));
        ] );
      ( "subst_type",
        let check = check ty "" in
        [
          test_case
            "A type is substituted to a type variable based on a type \
             substitution (c.f. Exercise 4.3.2)"
            `Quick (fun () ->
              let alpha = Typing.fresh_tyvar () in
              let beta = Typing.fresh_tyvar () in
              let gamma = Typing.fresh_tyvar () in
              check (TyFun (TyInt, TyBool))
              @@ Typing.subst_type
                   [ (alpha, TyInt) ]
                   (TyFun (TyVar alpha, TyBool));
              check (TyFun (TyBool, TyInt))
              @@ Typing.subst_type
                   [ (gamma, TyFun (TyVar beta, TyInt)); (beta, TyBool) ]
                   (TyVar gamma));
          test_case
            "Type substitution is performed in the order of the elements of \
             `subst` (c.f. Exercise 4.3.2)"
            `Quick (fun () ->
              let beta = Typing.fresh_tyvar () in
              let gamma = Typing.fresh_tyvar () in
              check (TyFun (TyVar beta, TyInt))
              @@ Typing.subst_type
                   [ (beta, TyBool); (gamma, TyFun (TyVar beta, TyInt)) ]
                   (TyVar gamma));
        ] );
      (* NOTE:
         The constraint that "variables with the same name cannot be defined at the same time"
         is a cross-cutting concern for `ty_item` and `ty_exp`,
         so they are tested in the same test suite. *)
      ( "Multiply_bound_variable",
        let test_if_the_expected_exception_is_raised title f =
          test_case
            (title
           ^ ": Variables cannot be bound several times in the same matching")
            `Quick (fun () ->
              try
                ignore @@ f ();
                fail "No exception"
              with
              | Typing.Error (Multiply_bound_variable _) -> ignore pass
              | _ -> fail "Unexpected exception")
        in
        [
          ( test_if_the_expected_exception_is_raised "Def" @@ fun () ->
            Typing.ty_item Environment.empty
              (Def [ ("x", ILit 1); ("x", ILit 2) ]) );
          ( test_if_the_expected_exception_is_raised "RecDef" @@ fun () ->
            Typing.ty_item Environment.empty
              (RecDef [ ("x", "y", ILit 1); ("x", "y", ILit 2) ]) );
          ( test_if_the_expected_exception_is_raised "LetExp" @@ fun () ->
            Typing.ty_exp Environment.empty
              (LetExp ([ ("x", ILit 1); ("x", ILit 2) ], ILit 1)) );
          ( test_if_the_expected_exception_is_raised "LetRecExp" @@ fun () ->
            Typing.ty_exp Environment.empty
              (LetRecExp ([ ("x", "y", ILit 1); ("x", "y", ILit 2) ], ILit 1))
          );
        ] );
    ]
