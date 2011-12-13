(* translate to ocaml *)

open Ast
open Builtin

exception No_such_symbol_found
exception Function_identifier_expected of string
exception Argument_count_mismatch
exception Argument_type_mismatch
exception Error of string

(* lookup a identifier in the symbol table, recursing upward as necessary *)
let rec sym_table_lookup table id =
  try
    StringMap.find id table.table 
  with No_such_symbol_found  ->
    match table.parent with
      Some(p) -> sym_table_lookup p id
    | None    -> raise No_such_symbol_found

and sym_table_lookup_type table id = 
  match sym_table_lookup table id with
    ValEntry(t) -> t
  | FuncEntry(t, _) -> t
	
and id_to_ocaml_id id = 
  "yappl_" ^ id
	       
(* expr to string functions *)
	       
and ident_to_string table id =
  id, (sym_table_lookup_type table id)
    
and seq_to_string table e1 e2 =
  let (s1, _) = expr_to_string table e1 in
  let (s2, t) = expr_to_string table e2 in
  ("ignore " ^ s1 ^ "; " ^ s2, t)
    
and eval_to_string table id args p =
  match sym_table_lookup table id with 
    FuncEntry(FuncType(ft), _) -> 
      let args_and_types = List.rev (List.rev_map (expr_to_string table) args) in
      let check b ea at =
	let (_, et) = ea in
	b || et <> at
      in
      let err = try (* check that arg and actual expr types match *)
	List.fold_left2 check false args_and_types ft.args_t 
      with Not_found ->
	raise Argument_count_mismatch
      in
      if err then 
	raise Argument_type_mismatch
      else
	let rev_args = List.rev_map (fun x -> let (s, _) = x in "( " ^ s ^ " )") args_and_types
	and oid = id_to_ocaml_id id in
	let str = 
	  match p with 
	    Noexpr -> "( " ^ oid ^ " " ^  (String.concat " " (List.rev rev_args)) ^ " )"
	  |  _ ->
	      let (pred, ptype) = expr_to_string table p in
	      if ptype <> ValType(Bool) then
		raise (Error "predicate does not evaluate to boolean")    (*predicate does not evaluate to boolean*)
	      else
		let (arg, rest) =
		  match rev_args with 
		    last :: rrest -> last, List.rev rrest
		  | [] -> "()", []
		in (* todo: need to check that $ in predicate has right type *)
		"let arg = " ^ arg ^ " in \n(Builtin.cond_eval ( fun " ^ Builtin.pred_special_var ^ " -> " ^  pred ^ ") (" ^ oid ^ " " ^  (String.concat " " rest) ^ " ) arg" 
	in
	str, ft.return_t 
  | _ -> raise (Function_identifier_expected id)

and val_bind_to_string table = function 
    vd, e -> "" (* todo *)

and val_decl_to_string table = function
    _ -> ()
 (* todo *)

and expr_to_string table = function
    IntLiteral(i) -> string_of_int i, ValType(Int)
  | BoolLiteral(b) -> string_of_bool b, ValType(Bool)
  | FloatLiteral(f) -> string_of_float f, ValType(Float)
  | Id(id) -> ident_to_string table id
  | ExprSeq(e1, e2) -> seq_to_string table e1 e2
  | Eval(id, args, p) -> eval_to_string table id args p
(*  | ValBind(bindings, e) -> (* todo *)
	let bstr = String.concat " "  (List.map (val_bind_to_string table) bindings) in
	bstr ^ "\n" ^ (expr_to_string table e), ValType(Int) *)
  | _ -> raise (Error "unsupported expression type")

let translate prog =
  let global_sym_table = { table = StringMap.empty; parent = None } in
  (* todo: handle print and rand *)
  let (s, _) = expr_to_string global_sym_table prog in
  s
  
  
