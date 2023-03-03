(* ML interpreter / type reconstruction *)
type id = string [@@deriving show]
type binOp = Plus | Mult | Eq | Lt | And | Or [@@deriving show]

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of (id * exp) list * exp
  | LetRecExp of (id * id * exp) list * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
[@@deriving show]

type item =
  | Exp of exp
  | Def of (id * exp) list
  | RecDef of (id * id * exp) list
[@@deriving show]

type program = item list [@@deriving show]
type tyvar = int [@@deriving show]

type ty = TyInt | TyBool | TyVar of tyvar | TyFun of ty * ty | TyList of ty
[@@deriving show]
