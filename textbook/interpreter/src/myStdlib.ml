open Syntax

let program =
  [
    Def [ ("+", FunExp ("x", FunExp ("y", BinOp (Plus, Var "x", Var "y")))) ];
    Def [ ("*", FunExp ("x", FunExp ("y", BinOp (Mult, Var "x", Var "y")))) ];
    Def [ ("=", FunExp ("x", FunExp ("y", BinOp (Eq, Var "x", Var "y")))) ];
    Def [ ("<", FunExp ("x", FunExp ("y", BinOp (Lt, Var "x", Var "y")))) ];
    Def [ ("&&", FunExp ("x", FunExp ("y", BinOp (And, Var "x", Var "y")))) ];
    Def [ ("||", FunExp ("x", FunExp ("y", BinOp (Or, Var "x", Var "y")))) ];
  ]
