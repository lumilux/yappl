(* translate to ocaml *)

open Ast

exception No_such_symbol_found
exception Function_identifier_expected

(* lookup a identifier in the symbol table, recursing upward as necessary *)
let rec sym_table_lookup table id =
  try
    table.table.find id
  with
    match table.parent with
      Some(p) -> sym_table_lookup p id
    | None    -> raise No_such_symbol_found

let rec sym_table_lookup_type table id
    match sym_table_lookup table id with
      VarEntry(t) -> t
    | FuncEntry(t, _) -> t

and let ident_to_string table id
    id, (sym_table_lookup_type table id)

and let lit_to_string table = function
    IntLiteral(v) -> (v, Int)
  | BoolLiteral(v) -> (v, Bool)
  | FloatLiteral(v) -> (v, Float)

and let seq_to_string table e1 e2 =
  let (s1, _) = expr_to_string table e1 in
  let (s2, t) = expr_to_string table e2 in
  ("ignore " ^ s1 ^ "; " ^ s2, t)

and let eval_to_string table id args e =
  match sym_table_lookup table id with 
    FuncEntry(t, st) -> (* todo *)
  | _ -> raise Function_identifier_expected

and let val_bind_to_string table = function 
    vd, e -> (* todo *)

and let val_decl_to_string table = function 
 (* todo *)

and let expr_to_string table = function
      Literal(l) -> lit_to_string l  
    | ValBind(bindings, e) -> (* todo *)
	let bstr = String.concat List.iter (val_bind_to_string table) bindings in
	bstr ^ "\n" ^ (expr_to_string table e)
in

let translate prog =
  let global_sym_table = { table = Some(StringMap.empty); parent = None } in
  (* todo: handle print and rand *)
  let (s, _) = expr_to_string global_sym_table prog in
  s
  
  
