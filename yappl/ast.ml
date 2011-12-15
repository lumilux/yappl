module StringMap = Map.Make(String)

type binop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | ListConcat | ListBuild
type unop = Neg | Not
type assignop = Assign | MemoAssign

type expr =
  (* Literal of literal*)
    IntLiteral of int
  | BoolLiteral of bool
  | FloatLiteral of float 
  | Id of string
  | ExprSeq of expr * expr
  | Eval of string * expr list * expr (* id args predicate *)
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | If of expr * expr * expr
  | Match of expr * pattern_match
  | ValBind of val_bind list * expr
  | FuncBind of func_bind list * expr
  | ListBuilder of expr list 
  | Noexpr

(*and literal =
    IntLiteral of int
  | BoolLiteral of bool
  | FloatLiteral of float 
*)
and pattern = 
    Ident of string 
  | Wildcard 
  | Concat of pattern * pattern

and pattern_match =
    Pattern of pattern * expr * pattern_match
  | NoPattern

(* let <type> : <name> = <expr> *)
and val_bind = {
    vdecl : decl;
    vexpr : expr;
} 

(* <type> : <name> *)
and decl = {
    dtype : fv_type;
    dname : string;
} 
   
(* <fdecl> <assignop> <expr>*)   
and func_bind = {
    fdecl : func_decl;
    op : assignop;
    body : expr;
} 

(* <type> : <fname> { <type> : <arg> } *) 
and func_decl = {
    freturn : fv_type; 
    fname : string;
    fargs : decl list
} 

(* For the symbol table *)
and sym_table = { 
    table : fv_type StringMap.t;
    parent : sym_table option;
} 

(*and st_entry = ValEntry of fv_type | FuncEntry of fv_type * sym_table*)

(* Types *)
and fv_type = FuncType of func_type | ValType of t
and func_type = {
    args_t : fv_type list;
    return_t : fv_type;
} 
and t = Void | Bool | Int | Float | List of t 

type program = expr 

