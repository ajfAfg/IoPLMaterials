open Miniml

let program = Syntax.show_program |> Fmt.of_to_string |> Alcotest.of_pp
let program_of_string str = Parser.toplevel Lexer.main (Lexing.from_string str)

let () =
  let open Alcotest in
  run "Parser"
    [
      ( "toplevel",
        let check = check program "" in
        [
          test_case "Precedence and associativity are kept" `Quick (fun () ->
              (* c.f. https://v2.ocaml.org/manual/expr.html#ss:precedence-and-associativity *)
              check
                (Syntax.Exp (BinOp (Plus, BinOp (Mult, ILit 1, ILit 1), ILit 2)))
              @@ program_of_string "1 * 1 + 2;;";
              check
                (Syntax.Exp
                   (BinOp
                      ( Or,
                        BinOp (And, BinOp (Lt, ILit 1, ILit 2), BLit false),
                        BLit true )))
              @@ program_of_string "1 < 2 && false || true;;";
              (* c.f. Chapter 3.5 #関数式と適用式の構文 *)
              check
                (Syntax.Exp
                   (BinOp (Plus, AppExp (FunExp ("x", Var "x"), ILit 1), ILit 1)))
              @@ program_of_string "(fun x -> x) 1 + 1;;");
          test_case
            "The order in which variables are declared and the order of \
             elements in the list are the same"
            `Quick (fun () ->
              check (Syntax.Decls [ [ ("x", ILit 1) ]; [ ("y", ILit 1) ] ])
              @@ program_of_string "let x = 1 let y = 1;;";
              check
                (Syntax.Decls
                   [ [ ("x", ILit 1); ("y", ILit 1) ]; [ ("z", ILit 1) ] ])
              @@ program_of_string "let x = 1 and y = 1 let z = 1;;");
          test_case
            "Operators enclosed in round brackets are parsed as variable names \
             (c.f. Exercise 3.4.2)"
            `Quick (fun () ->
              check (Syntax.Exp (AppExp (AppExp (Var "+", ILit 1), ILit 1)))
              @@ program_of_string "( + ) 1 1;;";
              check (Syntax.Decls [ [ ("+", ILit 1) ] ])
              @@ program_of_string "let ( + ) = 1;;");
        ] );
    ]
