(* translate to ocaml *)

open Ast
open Builtin

exception No_such_symbol_found
exception Function_identifier_expected of string
exception Argument_count_mismatch
exception Argument_type_mismatch
exception Error of string

(* utility functions *)

let match_num_types  = function
    (ValType(Int), ValType(Int)) -> Some(Int)
  | (ValType(Float), ValType(Float)) -> Some(Float)
  | _ -> None

(* lookup a identifier in the symbol table, recursing upward as necessary *)
let rec sym_table_lookup table id =
  try
    StringMap.find id table.table 
  with No_such_symbol_found  ->
    match table.parent with
      Some(p) -> sym_table_lookup p id
    | None    -> raise No_such_symbol_found

let sym_table_lookup_type table id = 
  match sym_table_lookup table id with
    ValEntry(t) -> t
  | FuncEntry(t, _) -> t
	
let id_to_ocaml_id id = 
  "yappl_" ^ id
	       
(* expr to string functions *)
	       
let rec ident_to_string table id =
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
	    Noexpr -> oid ^ " " ^  (String.concat " " (List.rev rev_args))
	  |  _ ->
	      let temp_table = { table = StringMap.add "$" (ValEntry(ft.return_t)) table.table; 
				 parent = table.parent } in (* add special predicate value *)
	      let (pred, ptype) = expr_to_string temp_table p in
	      if ptype <> ValType(Bool) then
		raise (Error "predicate does not evaluate to boolean")    (*predicate does not evaluate to boolean*)
	      else
		let (arg, rest) =
		  match rev_args with 
		    last :: rrest -> last, List.rev rrest
		  | [] -> "()", []
		in 
		"let arg = " ^ arg ^ " in \n(Builtin.cond_eval ( fun " ^ Builtin.pred_special_var ^ " -> " ^  pred ^ ") (" ^ oid ^ " " ^  (String.concat " " rest) ^ " ) arg" 
	in
	str, ft.return_t 
  | _ -> raise (Function_identifier_expected id)

and binop_to_string table e1 e2 op =
  let (s1, t1) = expr_to_string table e1 
  and (s2, t2) = expr_to_string table e2 
  in 
  let ocaml_op, return_t =  (* there should be a better way to do this *)
    match op with
      Add -> 
	(match match_num_types (t1, t2) with
	  Some(Int) -> "+", ValType(Int)
	| Some(Float) -> "+.", ValType(Float)
	| _ -> raise (Error("Type mismatch for +")))
    | Sub -> 
	(match match_num_types (t1, t2) with
	  Some(Int) -> "-", ValType(Int)
	| Some(Float) -> "-.", ValType(Float)
	| _ -> raise (Error("Type mismatch for -")))
    | Mult -> 
	(match match_num_types (t1, t2) with
	  Some(Int) -> "*", ValType(Int)
	| Some(Float) -> "*.", ValType(Float)
	| _ -> raise (Error("Type mismatch for *")))
    | Div -> 
	(match match_num_types (t1, t2) with
	  Some(Int) -> "/", ValType(Int)
	| Some(Float) -> "/.", ValType(Float)
	| _ -> raise (Error("Type mismatch for /")))
    | Equal -> 
	if t1 = t2 then
	  "=", ValType(Bool) 
	else 
	  raise (Error("Type mismatch for ="))
    | Neq -> 
	if t1 = t2 then
	  "<>", ValType(Bool) 
	else 
	  raise (Error("Type mismatch for !="))
    | Less ->
	(match match_num_types (t1, t2) with
	  Some(_) -> "<", ValType(Bool)
	| None -> raise (Error("Type mismatch for <")))
    | Leq ->
	(match match_num_types (t1, t2) with
	  Some(_) -> "<=", ValType(Bool)
	| None -> raise (Error("Type mismatch for <=")))
    | Greater ->
	(match match_num_types (t1, t2) with
	  Some(_) -> ">", ValType(Bool)
	| None -> raise (Error("Type mismatch for >")))
    | Geq ->
	(match match_num_types (t1, t2) with
	  Some(_) -> ">=", ValType(Bool)
	| None -> raise (Error("Type mismatch for >=")))
    | Mod -> 
	if t1 = ValType(Int) && t2 = ValType(Int) then
	  "mod", ValType(Int)
	else
	  raise (Error("Invalid types for %"))
    | ListConcat -> 
	(match (t1,t2) with
	  ValType(List(lt1)), ValType(List(lt2)) when lt1 = lt2 -> "@", t1
	| _ -> raise (Error("Type mismatch for @")))
    | ListBuild ->
	(match (t1,t2) with
	  ValType(lt1), ValType(List(lt2)) when lt1 = lt2 -> "::", t2
	| _ -> raise (Error("Type mismatch for ::")))
  in
  "(" ^ s1 ^ ") " ^ ocaml_op ^  " (" ^ s2 ^ ")", return_t

and unop_to_string table e op = 
  let (s, et) = expr_to_string table e in
  let opstr = 
    match op, et with
      Not, ValType(Bool) -> "not"
    | Neg, ValType(Int)
    | Neg, ValType(Float) -> "-"
    | _ -> raise (Error("Type mismatch with unary operator"))
  in
  opstr ^ " (" ^ s ^ ")", et

and if_to_string table pred e1 e2 =
    let (pred_str, pt) = expr_to_string table pred in
    if pt <> ValType(Bool) then
      raise (Error("Predicate for if expression not a boolen"))
    else 
      let (s1, t1) = expr_to_string table e1 
      and (s2, t2) = expr_to_string table e2 in
      if t1 = t2 then
	"if ( " ^ pred_str ^ " ) then ( " ^ s1 ^ " ) else ( " ^ s2 ^ " )", t1
      else
	raise (Error("Type mismatch of if expressions"))


and val_bindings_to_string table bindings e =
  let proc ts vb =
    let (tabl, s) = ts in
    let (new_tabl, new_s) = val_bind_to_string tabl vb in
    new_tabl, s ^ "\n" ^ new_s 
  in
  let (new_table, bstr) = List.fold_left proc (table, "") (List.rev bindings) in
  let (s, et) = expr_to_string new_table e in 
  bstr ^ "\n ( " ^ s ^ " )", et

and val_bind_to_string table vb =
  try 
    ignore (sym_table_lookup table vb.vdecl.dname);
    let (s, et) = expr_to_string table vb.vexpr in
    if et <> vb.vdecl.dtype then
      raise (Error("Incomptible type for value binding"))
    else
      let new_table = { table with table = StringMap.add vb.vdecl.dname (ValEntry et) table.table } in
      new_table, "let " ^ vb.vdecl.dname ^ " = " ^ s ^ " in "
  with No_such_symbol_found -> 
    raise (Error("Duplicate value identifier: " ^  vb.vdecl.dname))

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
  | Binop(e1, op, e2) -> binop_to_string table e1 e2 op
  | Unop(op, e) -> unop_to_string table e op
  | If(pred, e1, e2) -> if_to_string table pred e1 e2
  | ValBind(bindings, e) -> val_bindings_to_string table bindings e
  | Noexpr -> "", ValType(Void)
  | _ -> raise (Error "unsupported expression type")

let translate prog =
  let global_sym_table = { table = StringMap.empty; parent = None } in
  (* todo: handle print and rand *)
  let (s, _) = expr_to_string global_sym_table prog in
  s
  
  
