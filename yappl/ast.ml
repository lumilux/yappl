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

(* let <type> : <name> = <expr> *)
and val_bind = {
    vdecl : val_decl;
    vexpr : expr;
} 

(* <vtype> : <vname> *)
and val_decl = {
    vtype : val_type;
    vname : string;
} 
   
(* <fdecl> <assignop> <expr>*)   
and func_bind = {
    fdecl : func_decl;
    op : assignop;
    body : expr;
} 

(* <type> : <fname> { <type> : <arg> } *) 
and func_decl = {
    return : fv_type; 
    fname : string
      (fv_type * string) list

(* For the symbol table *)
and sym_table = { 
    table : fv_type StringMap.t;
    parent : sym_table option;
  } 

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

type program = expr 

