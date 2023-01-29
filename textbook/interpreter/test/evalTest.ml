open Miniml

let exval = Eval.show_exval |> Fmt.of_to_string |> Alcotest.of_pp

let environment =
  Environment.show Eval.pp_exval |> Fmt.of_to_string |> Alcotest.of_pp

let init_env variable_and_value =
  List.fold_left
    (fun env (id, exval) -> Environment.extend id exval env)
    Environment.empty variable_and_value

let () =
  let open Alcotest in
  run "Eval"
    [
      ( "apply_prim",
        let check = check exval "" in
        [
          test_case "Integer addition is possible" `Quick (fun () ->
              check (IntV 1) @@ Eval.apply_prim Plus (IntV 0) (IntV 1);
              check (IntV (-1)) @@ Eval.apply_prim Plus (IntV 0) (IntV (-1)));
          test_case "Integer multiplication is possible" `Quick (fun () ->
              check (IntV 0) @@ Eval.apply_prim Mult (IntV 0) (IntV 1);
              check (IntV 20) @@ Eval.apply_prim Mult (IntV (-2)) (IntV (-10)));
          test_case "*equal to* is possible for integer" `Quick (fun () ->
              check (BoolV true) @@ Eval.apply_prim Eq (IntV 0) (IntV 0);
              check (BoolV false) @@ Eval.apply_prim Eq (IntV 0) (IntV 1));
          test_case "*less than* is possible for integer" `Quick (fun () ->
              check (BoolV true) @@ Eval.apply_prim Lt (IntV 0) (IntV 1);
              check (BoolV false) @@ Eval.apply_prim Lt (IntV 1) (IntV 0));
          test_case "Logical conjunction is possible" `Quick (fun () ->
              check (BoolV true)
              @@ Eval.apply_prim And (BoolV true) (BoolV true);
              check (BoolV false)
              @@ Eval.apply_prim And (BoolV true) (BoolV false);
              check (BoolV false)
              @@ Eval.apply_prim And (BoolV false) (BoolV true);
              check (BoolV false)
              @@ Eval.apply_prim And (BoolV false) (BoolV false));
          test_case "Logical disjunction is possible" `Quick (fun () ->
              check (BoolV true) @@ Eval.apply_prim Or (BoolV true) (BoolV true);
              check (BoolV true)
              @@ Eval.apply_prim Or (BoolV true) (BoolV false);
              check (BoolV true)
              @@ Eval.apply_prim Or (BoolV false) (BoolV true);
              check (BoolV false)
              @@ Eval.apply_prim Or (BoolV false) (BoolV false));
        ] );
      ( "eval_exp",
        let check = check exval "" in
        [
          test_case "Bound variables can be referenced" `Quick (fun () ->
              let env =
                init_env
                  [
                    ("y", Eval.IntV 10);
                    ("y", Eval.BoolV true);
                    ("x", Eval.IntV 1);
                  ]
              in
              check (IntV 1) @@ Eval.eval_exp env (Var "x");
              check (BoolV true) @@ Eval.eval_exp env (Var "y"));
          test_case "Referencing an unbounded variable raises an exception"
            `Quick (fun () ->
              try
                ignore @@ Eval.eval_exp Environment.empty (Var "x");
                fail "No exception"
              with
              | Eval.Error _ -> ignore pass
              | _ -> fail "Unexpected exception");
          test_case
            "Literals are mapped to values in the internal representation"
            `Quick (fun () ->
              check (IntV 1) @@ Eval.eval_exp Environment.empty (ILit 1);
              check (BoolV true) @@ Eval.eval_exp Environment.empty (BLit true));
          test_case "Evaluation result is the same of `apply_prim`" `Quick
            (fun () ->
              check (Eval.apply_prim Plus (IntV 0) (IntV 1))
              @@ Eval.eval_exp Environment.empty (BinOp (Plus, ILit 0, ILit 1));
              check (Eval.apply_prim Mult (IntV 0) (IntV 1))
              @@ Eval.eval_exp Environment.empty (BinOp (Mult, ILit 0, ILit 1))
              (* NOTE: Omit for other operators *));
          test_case
            "If the condition is true, then the then clause is evaluated; \
             otherwise, the else clause is evaluated"
            `Quick (fun () ->
              check (IntV 1)
              @@ Eval.eval_exp Environment.empty
                   (IfExp (BLit true, ILit 1, ILit 0));
              check (IntV 0)
              @@ Eval.eval_exp Environment.empty
                   (IfExp (BLit false, ILit 1, ILit 0)));
          test_case "Variables can be referenced after variable binding" `Quick
            (fun () ->
              check (IntV 1)
              @@ Eval.eval_exp Environment.empty
                   (LetExp ([ ("x", ILit 1) ], Var "x")));
          test_case
            "Definitions of the same variable name are shadowed (c.f. Chapter \
             3.4 #変数宣言と有効範囲)"
            `Quick (fun () ->
              let exp =
                Syntax.LetExp
                  ( [ ("x", ILit 2) ],
                    LetExp
                      ( [ ("y", ILit 3) ],
                        LetExp
                          ( [ ("x", BinOp (Plus, Var "x", Var "y")) ],
                            BinOp (Mult, Var "x", Var "y") ) ) )
              in
              check (IntV 15) @@ Eval.eval_exp Environment.empty exp);
          test_case
            "Two or more variables can be defined at the same time (c.f. \
             Exercise 3.3.4)"
            `Quick (fun () ->
              let env =
                Environment.extend "x" (Eval.IntV 10) Environment.empty
              in
              check (IntV 110)
              @@ Eval.eval_exp env
                   (LetExp
                      ( [ ("x", ILit 100); ("y", Var "x") ],
                        BinOp (Plus, Var "x", Var "y") )));
          test_case
            "The value representing a function is a function closure (c.f. \
             Chapter3.5 #評価器の拡張: 関数値の表現の仕方)"
            `Quick (fun () ->
              check (Eval.IntV 6)
              @@ Eval.eval_exp
                   (init_env [ ("x", Eval.IntV 10) ])
                   (LetExp
                      ( [
                          ( "f",
                            LetExp
                              ( [ ("x", ILit 2) ],
                                LetExp
                                  ( [
                                      ( "addx",
                                        FunExp
                                          ("y", BinOp (Plus, Var "x", Var "y"))
                                      );
                                    ],
                                    Var "addx" ) ) );
                        ],
                        AppExp (Var "f", ILit 4) )));
          test_case
            "Support functions that perform dynamic binding (c.f. Exercise \
             3.4.5)"
            `Quick (fun () ->
              check (Eval.IntV 35)
              @@ Eval.eval_exp Environment.empty
                   (LetExp
                      ( [ ("a", ILit 3) ],
                        LetExp
                          ( [
                              ( "p",
                                DFunExp ("x", BinOp (Plus, Var "x", Var "a")) );
                            ],
                            LetExp
                              ( [ ("a", ILit 5) ],
                                BinOp (Mult, Var "a", AppExp (Var "p", ILit 2))
                              ) ) )));
          test_case
            "Support recursive function expression (c.f. Exercise 3.5.1)" `Quick
            (fun () ->
              check (Eval.IntV 120)
              @@ Eval.eval_exp Environment.empty
                   (LetRecExp
                      ( [
                          ( "fact",
                            "n",
                            IfExp
                              ( BinOp (Eq, Var "n", ILit 0),
                                ILit 1,
                                BinOp
                                  ( Mult,
                                    Var "n",
                                    AppExp
                                      ( Var "fact",
                                        BinOp (Plus, Var "n", ILit (-1)) ) ) )
                          );
                        ],
                        AppExp (Var "fact", ILit 5) )));
          test_case
            "Two or more recursive functions can be defined at the same time \
             in expression (c.f. Exercise 3.5.2)"
            `Quick (fun () ->
              check (Eval.BoolV true)
              @@ Eval.eval_exp Environment.empty
                   (LetRecExp
                      ( [
                          ( "even",
                            "n",
                            IfExp
                              ( BinOp (Eq, Var "n", ILit 0),
                                BLit true,
                                AppExp
                                  (Var "odd", BinOp (Plus, Var "n", ILit (-1)))
                              ) );
                          ( "odd",
                            "n",
                            IfExp
                              ( BinOp (Eq, Var "n", ILit 0),
                                BLit false,
                                AppExp
                                  (Var "even", BinOp (Plus, Var "n", ILit (-1)))
                              ) );
                        ],
                        AppExp (Var "even", ILit 2) )));
        ] );
      ( "eval_program",
        let check_environment = check environment "" in
        let check_exval = check exval "" in
        [
          test_case
            "When evaluating an equation, the environment remains the same"
            `Quick (fun () ->
              check_environment Environment.empty
              @@ snd
              @@ Eval.eval_program Environment.empty (Syntax.Exp (ILit 1));
              check_environment Environment.empty
              @@ snd
              @@ Eval.eval_program Environment.empty
                   (Syntax.Exp (LetExp ([ ("x", ILit 1) ], Var "x"))));
          test_case
            "Variables can be declared in sequence (c.f. Exercise 3.3.2)" `Quick
            (fun () ->
              let expected = init_env [ ("x", Eval.IntV 1); ("y", IntV 2) ] in
              let _, actual =
                Eval.eval_program Environment.empty
                @@ Syntax.Decls
                     [
                       [ ("x", ILit 1) ];
                       [ ("y", BinOp (Plus, Var "x", ILit 1)) ];
                     ]
              in
              check_environment expected actual);
          test_case
            "Two or more variables can be declared at the same time (c.f. \
             Exercise 3.3.4)"
            `Quick (fun () ->
              let expected =
                init_env
                  [
                    ("x", Eval.IntV 10);
                    ("x", IntV 1);
                    ("y", IntV 10);
                    ("z", IntV 10);
                  ]
              in
              let _, actual =
                Eval.eval_program (init_env [ ("x", Eval.IntV 10) ])
                @@ Syntax.Decls
                     [ [ ("x", ILit 1); ("y", Var "x") ]; [ ("z", Var "y") ] ]
              in
              check_environment expected actual);
          test_case
            "Support recursive function declaration (c.f. Exercise 3.5.1)"
            `Quick (fun () ->
              (* NOTE:
                 Since printing a recursive function does not stop
                 (because it contains itself in the environment),
                 test with the result of the recursive function computation. *)
              let _, newenv =
                Eval.eval_program Environment.empty
                @@ Syntax.RecDecl
                     [
                       ( "fact",
                         "n",
                         IfExp
                           ( BinOp (Eq, Var "n", ILit 0),
                             ILit 1,
                             BinOp
                               ( Mult,
                                 Var "n",
                                 AppExp
                                   (Var "fact", BinOp (Plus, Var "n", ILit (-1)))
                               ) ) );
                     ]
              in
              check_exval (Eval.IntV 120)
              @@ Eval.eval_exp newenv (AppExp (Var "fact", ILit 5)));
          test_case
            "Two or more recursive functions can be declared at the same time \
             (c.f. Exercise 3.5.2)"
            `Quick (fun () ->
              (* NOTE:
                 Since printing a recursive function does not stop
                 (because it contains itself in the environment),
                 test with the result of the recursive function computation. *)
              let _, newenv =
                Eval.eval_program Environment.empty
                @@ Syntax.RecDecl
                     [
                       ( "even",
                         "n",
                         IfExp
                           ( BinOp (Eq, Var "n", ILit 0),
                             BLit true,
                             AppExp (Var "odd", BinOp (Plus, Var "n", ILit (-1)))
                           ) );
                       ( "odd",
                         "n",
                         IfExp
                           ( BinOp (Eq, Var "n", ILit 0),
                             BLit false,
                             AppExp
                               (Var "even", BinOp (Plus, Var "n", ILit (-1))) )
                       );
                     ]
              in
              check_exval (Eval.BoolV true)
              @@ Eval.eval_exp newenv (AppExp (Var "even", ILit 2)));
        ] );
    ]
