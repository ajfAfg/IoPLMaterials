open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

let string_of_binop = function
  | Plus -> "+"
  | Mult -> "*"
  | Eq -> "="
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"

let string_of_ty = function TyInt -> "int" | TyBool -> "bool"

let ty_prim op ty1 ty2 =
  match op with
  | Plus | Mult | Eq | Lt -> (
      match (ty1, ty2) with
      | TyInt, TyInt -> TyInt
      | _ -> err ("Argument must be of integer: " ^ string_of_binop op))
  | And | Or -> (
      match (ty1, ty2) with
      | TyBool, TyBool -> TyBool
      | _ -> err ("Argument must be of boolean: " ^ string_of_binop op))

let rec ty_exp tyenv = function
  | Var x -> (
      try Environment.lookup x tyenv
      with Environment.Not_bound -> err ("variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) -> (
      match ty_exp tyenv exp1 with
      | TyBool -> (
          match (ty_exp tyenv exp2, ty_exp tyenv exp3) with
          | ty1, ty2 when ty1 = ty2 -> ty1
          | ty1, _ ->
              err ("Else expression must be " ^ string_of_ty ty1 ^ ": if"))
      | _ -> err "Test expression must be boolean: if")
  | LetExp (bindings, exp2) ->
      let tyenv' =
        bindings
        |> List.map (fun (id, exp1) -> (id, ty_exp tyenv exp1))
        |> List.fold_left
             (fun tyenv' (id, ty) -> Environment.extend id ty tyenv')
             tyenv
      in
      ty_exp tyenv' exp2
  | _ -> err "Not Implemented!"

let ty_item tyenv = function
  | Exp e ->
      let ty = ty_exp tyenv e in
      (Some ty, tyenv)
  | _ -> err "Not Implemented!"
