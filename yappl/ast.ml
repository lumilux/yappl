module StringMap = Map.Make(String)

type binop = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Mod | ListConcat | ListBuild
type unop = Neg | Not
type assignop = Assign | MemoAssign

type expr =
    Literal of literal
  | Id of string
  | ExprConcat of expr * expr
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

and val_bind = val_decl * expr

and val_decl = val_type * string

and func_bind = string * assignop * func_decl

and func_decl = fv_type * string * (fv_type * string) list

(* For the symbol table *)
and sym_table = SymTable of fv_type StringMap.t * sym_table | NoTable

and fv_type = FTYPE of func_type | VTYPE of val_type
and func_type = {
    args : fv_type list;
    return : fv_type;
} 
and val_type = BTYPE of base_type | LIST of val_type
and base_type = BOOL | INT | FLOAT 

(*type var_decl = {
  vtype : type_decl;
  vid : string;
}

type fdecl = {
  fname : string;
  formals : var_decl list;
  return : type_decl list;
  body : expr list;
}

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : expr list;
  }*)

(* should 'expr' be replaced with 'func_decl'?
 everything is a function, not an expression *)

type program = expr 

