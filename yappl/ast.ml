module StringMap = Map.Make(String)

type binop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | ListConcat | ListBuild
type unop = Neg | Not
type assignop = Assign | MemoAssign

type expr =
    Literal of literal
  | Id of string
  | ExprSeq of expr * expr
  | Eval of string * expr list * expr
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | If of expr * expr * expr
  | Match of expr * pattern_match
  | ValBind of val_bind list * expr
  | FuncBind of func_bind list * expr
  | List of expr list 
  | Noexpr

and literal =
    IntLiteral of int
  | BoolLiteral of bool
  | FloatLiteral of float 

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
    table : st_entry StringMap.t;
    parent : sym_table option;
} 

and st_entry = VarEntry of fv_type | FuncEntry of fv_type * sym_table

and fv_type = FuncType of func_type | VarType of val_type
and func_type = {
    args : fv_type list;
    return : fv_type;
} 
and val_type = BaseType of base_type | ListType of val_type
and base_type = Bool | Int | Float 

type program = expr 

