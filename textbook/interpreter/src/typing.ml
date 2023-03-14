open Syntax

type error =
  | Expr_type_clash
  | Multiply_bound_variable of id
  | Unbound_value of id
  | Not_implemented

exception Error of error

let err error = raise (Error error)

type tyenv = tysc Environment.t
type subst = (tyvar * ty) list

let string_of_error = function
  | Expr_type_clash -> "Cannot infer a type"
  | Multiply_bound_variable id ->
      Printf.sprintf "Variable %s is bound several times in this matching" id
  | Unbound_value id -> "Unbound value " ^ id
  | Not_implemented -> "Not implemented!"

let string_of_binop = function
  | Plus -> "+"
  | Mult -> "*"
  | Eq -> "="
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"

let string_of_tyvar tyvar =
  Printf.sprintf "'%c" @@ Char.chr (Char.code 'a' + tyvar)

let rec string_of_ty = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVar tyvar -> string_of_tyvar tyvar
  | TyFun ((TyFun _ as ty1), ty2) ->
      Printf.sprintf "(%s) -> %s" (string_of_ty ty1) (string_of_ty ty2)
  | TyFun (ty1, ty2) ->
      Printf.sprintf "%s -> %s" (string_of_ty ty1) (string_of_ty ty2)
  | _ -> err Not_implemented

let tysc_of_ty ty = TyScheme ([], ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
  in
  body

let freevar_ty ty =
  let rec freevar_ty' ty set =
    match ty with
    | TyVar tyvar -> MySet.insert tyvar set
    | TyFun (ty1, ty2) -> set |> freevar_ty' ty1 |> freevar_ty' ty2
    | _ -> set
  in
  freevar_ty' ty MySet.empty

let freevar_tysc = function
  | TyScheme (tyvars, ty) -> MySet.diff (freevar_ty ty) (MySet.from_list tyvars)

let rec freevar_tyenv tyenv =
  Environment.fold_right
    (fun tysc fv_tyenv -> MySet.union fv_tyenv @@ freevar_tysc tysc)
    tyenv MySet.empty

(* NOTE:
    The elements of `subst` are assumed to be topologically sorted with respect to `tyvar`.
    That is, ∀ (tyvar₁, ty₁), (tyvar₂, ty₂) ∈ subst, ∃ i, j ∈ int,
    (tyvar₁, ty₁) = List.nth subst i ∧ (tyvar₂, ty₂) = List.nth subst j ∧ i ≤ j → tyvar₁ ∉ FTV(ty₂),
    where `FTV(ty)` is the set of type variables that appear in `ty`. *)
let rec subst_type subst = function
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyVar tyvar -> (
      match List.assoc_opt tyvar subst with
      | Some b -> subst_type (List.tl subst) b
      | None -> TyVar tyvar)
  | TyFun (ty1, ty2) -> TyFun (subst_type subst ty1, subst_type subst ty2)
  | _ -> err Not_implemented

let subst_tyenv subst tyenv =
  Environment.map
    (fun (TyScheme (tyvars, ty)) -> TyScheme (tyvars, subst_type subst ty))
    tyenv

let eqs_of_subst subst = List.map (fun (tyvar, ty) -> (TyVar tyvar, ty)) subst
let eqs_of_substs substs = substs |> List.map eqs_of_subst |> List.flatten

let subst_eqs subst eqs =
  List.map (fun (ty1, ty2) -> (subst_type subst ty1, subst_type subst ty2)) eqs

let occur_check tyvar ty =
  if MySet.member tyvar @@ freevar_ty ty then err Expr_type_clash

let rec unify = function
  | [] -> []
  | eq :: eqs -> (
      match eq with
      | ty1, ty2 when ty1 = ty2 -> unify eqs
      | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
          unify @@ [ (ty11, ty21); (ty12, ty22) ] @ eqs
      | TyVar tyvar, ty ->
          occur_check tyvar ty;
          (unify @@ subst_eqs [ (tyvar, ty) ] eqs) @ [ (tyvar, ty) ]
      | ty, TyVar tyvar ->
          occur_check tyvar ty;
          (unify @@ subst_eqs [ (tyvar, ty) ] eqs) @ [ (tyvar, ty) ]
      | _ -> err Expr_type_clash)

let closure ty tyenv subst =
  let fv_tyenv =
    freevar_tyenv tyenv
    |> MySet.map (fun tyvar -> freevar_ty (subst_type subst (TyVar tyvar)))
    |> MySet.bigunion
  in
  let tyvars = MySet.diff (freevar_ty ty) fv_tyenv |> MySet.to_list in
  TyScheme (tyvars, ty)

let raise_if_id_duplicates ids =
  let ids' = List.sort_uniq compare ids in
  match MyList.subtract ids ids' with
  | [] -> ()
  | id :: _ -> err @@ Multiply_bound_variable id

let ty_prim op ty1 ty2 =
  match op with
  | Plus | Mult -> ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
  | Eq | Lt -> ([ (ty1, TyInt); (ty2, TyInt) ], TyBool)
  | And | Or -> ([ (ty1, TyBool); (ty2, TyBool) ], TyBool)

let rec ty_exp tyenv = function
  | Var x -> (
      try
        let (TyScheme (tyvars, ty)) = Environment.lookup x tyenv in
        let subst =
          List.map (fun tyvar -> (tyvar, TyVar (fresh_tyvar ()))) tyvars
        in
        ([], subst_type subst ty)
      with Environment.Not_bound -> err @@ Unbound_value x)
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let subst1, ty1 = ty_exp tyenv exp1 in
      let subst2, ty2 = ty_exp tyenv exp2 in
      let eqs, ty = ty_prim op ty1 ty2 in
      let subst = unify @@ eqs_of_subst subst1 @ eqs_of_subst subst2 @ eqs in
      (subst, subst_type subst ty)
  | IfExp (exp1, exp2, exp3) ->
      let subst1, ty1 = ty_exp tyenv exp1 in
      let subst2, ty2 = ty_exp tyenv exp2 in
      let subst3, ty3 = ty_exp tyenv exp3 in
      let subst =
        unify
        @@ eqs_of_substs [ subst1; subst2; subst3 ]
        @ [ (ty1, TyBool); (ty2, ty3) ]
      in
      (subst, subst_type subst ty2)
  | LetExp (bindings, exp2) ->
      let tyenv', subst1 = ty_let tyenv bindings in
      let subst2, ty = ty_exp tyenv' exp2 in
      let subst = unify @@ eqs_of_substs [ subst1; subst2 ] in
      (subst, subst_type subst ty)
  | LetRecExp (bindings, exp2) ->
      let tyenv', subst1 = ty_let_rec tyenv bindings in
      let subst2, ty = ty_exp tyenv' exp2 in
      let subst = unify @@ eqs_of_substs [ subst1; subst2 ] in
      (subst, subst_type subst ty)
  | FunExp (id, exp) ->
      let ty1 = TyVar (fresh_tyvar ()) in
      let subst, ty2 =
        ty_exp (Environment.extend id (tysc_of_ty ty1) tyenv) exp
      in
      (subst, TyFun (subst_type subst ty1, ty2))
  | AppExp (exp1, exp2) ->
      let ty2 = TyVar (fresh_tyvar ()) in
      let subst1, ty12 = ty_exp tyenv exp1 in
      let subst2, ty1 = ty_exp tyenv exp2 in
      let subst =
        unify @@ eqs_of_substs [ subst1; subst2 ] @ [ (ty12, TyFun (ty1, ty2)) ]
      in
      (subst, subst_type subst ty2)
  | _ -> err Not_implemented

and ty_let tyenv bindings =
  bindings |> List.map (fun (id, _) -> id) |> raise_if_id_duplicates;
  let bound_tyscs, subst =
    bindings
    |> List.map (fun (id, e) -> (id, ty_exp tyenv e))
    |> List.fold_left
         (fun (bound_types, subst) (id, (subst', ty)) ->
           let tysc = closure (subst_type subst' ty) tyenv subst' in
           ((id, tysc) :: bound_types, subst @ subst'))
         ([], [])
  in
  let tyenv' =
    List.fold_left
      (fun tyenv' (id, tysc) -> Environment.extend id tysc tyenv')
      tyenv bound_tyscs
  in
  (tyenv', subst)

and ty_let_rec tyenv bindings =
  bindings |> List.map (fun (id, _, _) -> id) |> raise_if_id_duplicates;
  let bindings_with_ty1_ty2 =
    List.map
      (fun (id, para, exp) ->
        let ty1 = TyVar (fresh_tyvar ()) in
        let ty2 = TyVar (fresh_tyvar ()) in
        (id, para, exp, ty1, ty2))
      bindings
  in
  let tyenv' =
    bindings_with_ty1_ty2
    |> List.map (fun (id, _para, _exp, ty1, ty2) ->
           (id, tysc_of_ty @@ TyFun (ty1, ty2)))
    |> List.fold_left
         (fun tyenv' (id, tysc) -> Environment.extend id tysc tyenv')
         tyenv
  in
  let subst =
    bindings_with_ty1_ty2
    |> List.map (fun (_id, para, exp, ty1, ty2) ->
           (ty_exp (Environment.extend para (tysc_of_ty ty1) tyenv') exp, ty2))
    |> List.fold_left
         (fun subst ((subst', ty2'), ty2) ->
           unify [ (ty2, ty2') ] @ subst @ subst')
         []
  in
  let tyenv' =
    bindings_with_ty1_ty2
    |> List.map (fun (id, _, _, ty1, ty2) ->
           (id, closure (subst_type subst (TyFun (ty1, ty2))) tyenv subst))
    |> List.fold_left
         (fun tyenv' (id, tysc) -> Environment.extend id tysc tyenv')
         tyenv
  in
  (tyenv', subst)

let ty_item tyenv = function
  | Exp e ->
      let subst, ty = ty_exp tyenv e in
      (Some ty, subst_tyenv subst tyenv)
  | Def bindings ->
      let tyenv', subst = ty_let tyenv bindings in
      (None, subst_tyenv subst tyenv')
  | RecDef bindings ->
      let tyenv', subst = ty_let_rec tyenv bindings in
      (None, subst_tyenv subst tyenv')
