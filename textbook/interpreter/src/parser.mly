%{
open Syntax

let curry parameters expression =
  List.fold_right (fun x acc -> FunExp (x, acc)) parameters expression

(*
NOTE:
Since I don't know how to convert the token back to the original string,
I'm writing it directly, even though it doesn't seem very good.
*)
let string_of_binop = function
  | Plus -> "+"
  | Mult -> "*"
  | Eq -> "="
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT LAND LOR
%token IF THEN ELSE TRUE FALSE
%token LET REC IN EQ AND
%token RARROW FUN DFUN
%token APP
%token EOF

%token <int> INTV
%token <Syntax.id> ID

// NOTE: References about precedence and associativity
// - https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly
// - https://ptival.github.io/2017/05/16/parser-generators-and-function-application/
%nonassoc IN
%nonassoc LET FUN DFUN RARROW
%nonassoc THEN
%nonassoc ELSE
%nonassoc IF
%right LOR
%right LAND
%left EQ LT
%left PLUS
%left MULT
%nonassoc INTV ID TRUE FALSE LPAREN RPAREN SEMISEMI
%nonassoc APP

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
  | e=Expr SEMISEMI { Exp e }
  | ds=Decls SEMISEMI { Decls ds }
  | LET REC bs=LetRecBindings SEMISEMI { RecDecl bs }
  | { failwith "Syntax error" }

Expr :
  | e=Expr_ { e }
  | e1=Expr e2=Expr_ %prec APP { AppExp (e1, e2) }
  | e1=Expr op=BinOp e2=Expr { BinOp (op, e1, e2) }
  | IF e1=Expr THEN e2=Expr ELSE e3=Expr { IfExp (e1, e2, e3) }
  | FUN params=nonempty_list(ID) RARROW e=Expr { curry params e }
  | DFUN params=nonempty_list(ID) RARROW e=Expr { List.fold_right (fun x acc -> DFunExp (x, acc)) params e }
  | LET bs=LetBindings IN e=Expr { LetExp (bs, e) }
  | LET REC bs=LetRecBindings IN e2=Expr { LetRecExp (bs, e2) }

Expr_ :
  | n=ValueName { Var n }
  | c=Constant { c }
  | LPAREN e=Expr RPAREN { e }

%inline ValueName :
  | i=ID { i }
  | LPAREN binOp=BinOp RPAREN { string_of_binop binOp }

%inline Constant :
  | i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }

%inline BinOp :
  | PLUS { Plus }
  | MULT { Mult }
  | EQ { Eq }
  | LT { Lt }
  | LAND { And }
  | LOR { Or }

LetBindings :
  | binding=LetBinding { [binding] }
  | binding=LetBinding AND l=LetBindings { binding :: l }

%inline LetBinding :
  | x=ValueName params=list(ID) EQ e=Expr { (x, curry params e) }

LetRecBindings :
  | binding=LetRecBinding { [binding] }
  | binding=LetRecBinding AND bs=LetRecBindings { binding :: bs }

%inline LetRecBinding :
  | x=ValueName EQ FUN param=ID RARROW e=Expr { (x, param, e) }

Decls :
  | { [] }
  | LET bs=LetBindings ds=Decls { bs :: ds }
