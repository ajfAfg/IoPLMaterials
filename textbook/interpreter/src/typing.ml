open Syntax

type error =
  | Expr_type_clash
  | Multiply_bound_variable of id
  | Unbound_value of id
  | Not_implemented

exception Error of error

let err error = raise (Error error)

type tyenv = ty Environment.t
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
      try ([], Environment.lookup x tyenv)
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
      bindings |> List.map (fun (id, _) -> id) |> raise_if_id_duplicates;
      let id_subst_ty_list =
        List.map
          (fun (id, exp) ->
            let subst, ty = ty_exp tyenv exp in
            (id, subst, ty))
          bindings
      in
      let tyenv' =
        List.fold_left
          (fun tyenv (id, _, ty) -> Environment.extend id ty tyenv)
          tyenv id_subst_ty_list
      in
      let subst1 =
        List.fold_left
          (fun subst (_, subst', _) -> subst' @ subst)
          [] id_subst_ty_list
      in
      let subst2, ty2 = ty_exp tyenv' exp2 in
      let subst = unify @@ eqs_of_substs [ subst1; subst2 ] in
      (subst, subst_type subst ty2)
  | LetRecExp (bindings, exp2) ->
      let tyenv', eqs = ty_let_rec tyenv bindings in
      let subst1, ty = ty_exp tyenv' exp2 in
      let subst = unify @@ eqs_of_subst subst1 @ eqs in
      (subst, subst_type subst ty)
  | FunExp (id, exp) ->
      let ty1 = TyVar (fresh_tyvar ()) in
      let subst, ty2 = ty_exp (Environment.extend id ty1 tyenv) exp in
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

and ty_let_rec tyenv bindings =
  bindings |> List.map (fun (id, _, _) -> id) |> raise_if_id_duplicates;
  let bindings_with_ty1_ty2 =
    bindings
    |> List.map (fun (id, para, exp) ->
           let ty1 = TyVar (fresh_tyvar ()) in
           let ty2 = TyVar (fresh_tyvar ()) in
           (id, para, exp, ty1, ty2))
  in
  let tyenv' =
    bindings_with_ty1_ty2
    |> List.map (fun (id, _para, _exp, ty1, ty2) -> (id, TyFun (ty1, ty2)))
    |> List.fold_left
         (fun tyenv' (id, ty) -> Environment.extend id ty tyenv')
         tyenv
  in
  let subst, eqs =
    bindings_with_ty1_ty2
    |> List.map (fun (_id, para, exp, ty1, ty2) ->
           (ty_exp (Environment.extend para ty1 tyenv') exp, ty2))
    |> List.fold_left
         (fun (subst, eqs) ((subst', ty2'), ty2) ->
           (subst @ subst', (ty2, ty2') :: eqs))
         ([], [])
  in
  (tyenv', eqs_of_subst subst @ eqs)

let ty_item tyenv = function
  | Exp e ->
      let subst, ty = ty_exp tyenv e in
      (Some ty, Environment.map (subst_type subst) tyenv)
  | Def bindings ->
      bindings |> List.map (fun (id, _) -> id) |> raise_if_id_duplicates;
      let bound_types, subst =
        bindings
        |> List.map (fun (id, e) -> (id, ty_exp tyenv e))
        |> List.fold_left
             (fun (bound_types, subst) (id, (subst', ty)) ->
               ((id, ty) :: bound_types, subst @ subst'))
             ([], [])
      in
      let tyenv' =
        List.fold_left
          (fun tyenv' (id, ty) -> Environment.extend id ty tyenv')
          tyenv bound_types
      in
      (None, Environment.map (subst_type subst) tyenv')
  | RecDef bindings ->
      let tyenv', eqs = ty_let_rec tyenv bindings in
      let subst = unify eqs in
      (None, Environment.map (subst_type subst) tyenv')
