module StringMap = Map.Make(String)

type binop = Add | And | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | Mod | ListConcat | ListBuild
type unop = Neg | Not
type assignop = Assign | MemoAssign

type expr =
  (* Literal of literal*)
    IntLiteral of int
  | BoolLiteral of bool
  | FloatLiteral of float 
  | Id of string
  | CondVar
  | ExprSeq of expr * expr
  | Eval of string * expr list * expr (* id args predicate *)
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | If of expr * expr * expr
  | Match of expr * pattern_match
  | ValBind of val_bind list * expr
  | FuncBind of func_bind list * expr
  | ListBuilder of expr list 
  | GetIndex of string * expr
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

(* to string ... *)

let rec string_of_expr indent expr =
  let more_indent = "    " ^ indent in
  match expr with
      IntLiteral(i) -> indent ^ "IntLit " ^ (string_of_int i) ^ "\n"
    | BoolLiteral(b) -> indent ^ "BoolLit " ^ (string_of_bool b) ^ "\n"
    | FloatLiteral(f) -> indent ^ "FloatLit " ^ (string_of_float f) ^ "\n"
    | Id(id) -> indent ^ "Id " ^ id ^ "\n" 
    | CondVar -> indent ^ "CondVar\n"
    | ExprSeq(e1, e2) -> 
	indent ^ "ExprSeq\n" ^ (string_of_expr more_indent e1) ^ (string_of_expr more_indent e2)
    | Eval(id, args, p) -> 
	let expr_strs = String.concat "" (List.map (string_of_expr more_indent) args) in
	indent ^ "Eval " ^ id ^ "\n" ^ expr_strs ^  (string_of_expr more_indent p)
    | Binop(e1, op, e2) -> indent ^ "Binop " ^ (string_of_binop op) ^ "\n" ^ (string_of_expr more_indent e1) ^ (string_of_expr more_indent e2) 
    | Unop(op, e) -> indent ^ "Unop " ^ (string_of_unop op) ^ "\n" ^ (string_of_expr more_indent e)
    | If(pred, e1, e2) ->  indent ^ "IfThenElse\n" ^  (string_of_expr more_indent pred) ^ (string_of_expr more_indent e1) ^ (string_of_expr more_indent e2)
   (* | ValBind(bindings, e) -> val_bindings_to_string table bindings e *)
    | FuncBind(bindings, e) -> indent ^ "FuncBindings\n" ^ (String.concat "" (List.map (string_of_func_bind more_indent) bindings)) ^ (string_of_expr more_indent e)
    | Noexpr -> indent ^ "Noexpr\n"
(*    | ListBuilder(l) -> list_to_string table l*)
    | _ -> raise Not_found
 


and string_of_func_bind indent fb =
  let more_indent = "    " ^ indent in
  indent ^ "FuncBind " ^ (string_of_assignop fb.op) ^ "\n" ^ (string_of_func_decl more_indent fb.fdecl) ^  (string_of_expr more_indent fb.body)

and string_of_func_decl indent fd =
  indent ^ "FuncDecl(" ^ fd.fname ^ ", " ^ (string_of_fv_type fd.freturn) ^ ", " ^ (String.concat " " (List.map string_of_decl fd.fargs)) ^ "\n"

and string_of_decl d =
  "Decl(" ^ d.dname ^ ", " ^ (string_of_fv_type d.dtype) ^ ")"
    
and string_of_fv_type = function
    FuncType(ft) -> "FuncType(" ^ (string_of_fv_type ft.return_t) ^ ", " ^ (String.concat ", " (List.map string_of_fv_type ft.args_t)) ^ ")"
  | ValType(vt) -> "ValType(" ^ (string_of_t vt) ^ ")"

and string_of_t = function
    Void -> "Void"
  | Bool -> "Bool"
  | Int  -> "Int"
  | Float -> "Float"
  | List typ -> (string_of_t typ) ^ "[]"

and string_of_binop = function
    Add -> "+" 
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/" 
  | Equal -> "=" 
  | Neq -> "!=" 
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | Mod -> "%" 
  | ListConcat -> "@"
  | ListBuild -> "::"
  | Or -> "or"
  | And -> "and"

and string_of_unop = function
  | Neg -> "-"
  | Not -> "!"
	
and string_of_assignop = function 
  | Assign -> "="
  | MemoAssign -> ":="
