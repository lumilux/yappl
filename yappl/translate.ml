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
    ValType(Int), ValType(Int) -> Some(Int)
  | ValType(Float), ValType(Float) -> Some(Float)
  | ValType(x), ValType(y) -> print_endline ((string_of_t x) ^ ", " ^ (string_of_t y)); None
  | _ -> None

(* lookup a identifier in the symbol table, recursing upward as necessary *)
let rec sym_table_lookup table id =
  try
    StringMap.find id table.table 
  with Not_found  ->
    match table.parent with
      Some(p) -> sym_table_lookup p id
    | None    -> raise No_such_symbol_found

(*let sym_table_lookup_type table id = 
  match sym_table_lookup table id with
    FuncType ft -> 
  | FuncEntry(t, _) -> t*)
	
let id_to_ocaml_id = function
    "rand" | "seed" as id -> "Builtin." ^ id
  | _ as id -> "yappl_" ^ id
	       
(* expr to string functions *)
	       
let rec ident_to_string table id =
  id, (sym_table_lookup table id)
    
and seq_to_string table e1 e2 =
  let (s1, _) = expr_to_string table e1 in
  let (s2, t) = expr_to_string table e2 in
  ("(ignore (" ^ s1 ^ ")); " ^ s2, t )

and type_to_string vt = 
 match vt with
   ValType(Int) -> "int"
 | ValType(Bool) -> "bool"
 | ValType(Float) -> "float"
 | ValType(Void) -> "void"
 | _ -> "unknown crazy voodoo"

(* "expected ... got" string *) 
and eg2s s1 s2 = 
 (" Expected " ^ type_to_string s1 ^ ", surprised by " ^ type_to_string s2 )
    
and eval_to_string table id args p =
  match id with
    "print" ->
      (match p with 
	Noexpr -> 
	  let arg = 
	    match args with 
	      arg :: [] -> arg 
	    | _ -> raise (Error("invalid number of args to print"))
	  in 
	  let ret_t = ValType Bool in
	  let (arg_s, arg_t) = expr_to_string table arg in
	  (match arg_t with
	    ValType Bool -> "print_string (string_of_bool ( " ^ arg_s ^ " )); true ", ret_t
	  | ValType Int -> "print_int ( " ^ arg_s ^ " ); true ", ret_t
	  | ValType Float -> "print_float ( " ^ arg_s ^ " ); true ", ret_t
	  | _ -> raise (Error("unsupported print expression type")))
      | _ -> raise (Error("print does not support predicates")))  
  | _ ->
    match sym_table_lookup table id with 
      FuncType ft -> 
	let rev_args_and_types = List.rev_map (expr_to_string table) args in
	let check b ea at =
	  let (_, et) = ea in
	  b || et <> at
	in
	let err = try (* check that arg and actual expr types match *)
	  List.fold_left2 check false rev_args_and_types (List.rev ft.args_t)
	with Not_found ->
	  raise Argument_count_mismatch
	in
	if err then 
	  raise Argument_type_mismatch
	else
	  let eval_str_no_unit = (id_to_ocaml_id id) ^ " " ^ (String.concat " " (List.rev_map (fun (s, _) -> "( " ^ s ^ " )") rev_args_and_types)) in
	  let str = 
	    match p with 
	      Noexpr -> eval_str_no_unit ^ " ()"
	    |  _ ->
		let temp_table = { table with table = StringMap.add "$" ft.return_t table.table } in (* add special predicate value *)
		let (pred, ptype) = expr_to_string temp_table p in
		if ptype <> ValType(Bool) then
		  raise (Error "predicate does not evaluate to boolean")    (*predicate does not evaluate to boolean*)
		else 
		  "Builtin.cond_eval ( fun " ^ Builtin.pred_special_var ^ " -> " ^  pred ^ ") (" ^ eval_str_no_unit ^ " )" 
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
  opstr ^ "(" ^ s ^ ")", et

and if_to_string table pred e1 e2 =
    let (pred_str, pt) = expr_to_string table pred in
    if pt <> ValType(Bool) then
      raise (Error("Predicate for if expression not a boolean"))
    else 
      let (s1, t1) = expr_to_string table e1 
      and (s2, t2) = expr_to_string table e2 in
      if t1 = t2 then
	"if ( " ^ pred_str ^ " ) then ( " ^ s1 ^ " ) else ( " ^ s2 ^ " )", t1
      else if s2 = "" then (* in case there was no else *)
  "if ( " ^ pred_str ^ " ) then ( " ^ s1 ^ " )", t1
      else
	raise (Error("Type mismatch of if expressions"))


and list_to_string table l = 
    match l with
      []  -> "[]", ValType(Void)
    | _   -> let head = List.hd (List.rev l) in 
             let (_,vt) = expr_to_string table head in
             let sl = List.map ( fun e -> 
				  (match expr_to_string table e with
			             (s1, t1) -> if t1 = vt then s1
			             else raise (Error("Type mismatch in list" ^ (eg2s vt t1))) 
                                   )
			       ) l in
     	     ("[" ^ (String.concat "," (List.rev sl)) ^ "]"), vt  

and val_bindings_to_string table bindings e =
  let proc (tabl, s) vb =
    let (new_tabl, new_s) = val_bind_to_string tabl vb in
    new_tabl, s ^ "\n" ^ new_s 
  in
  let (new_table, bstr) = List.fold_left proc (table, "") (List.rev bindings) in
  let (s, et) = expr_to_string new_table e in 
  bstr ^ "\n ( " ^ s ^ " )", et

and val_bind_to_string table vb =
  try 
    ignore (sym_table_lookup table vb.vdecl.dname);  (* make sure id doesn't already exist *)
    raise (Error("Duplicate value identifier: " ^  vb.vdecl.dname))
  with No_such_symbol_found -> 
    let (s, et) = expr_to_string table vb.vexpr in
    if et <> vb.vdecl.dtype then
      raise (Error("Incomptible type for value binding"))
    else
      let new_table = { table with table = StringMap.add vb.vdecl.dname et table.table } in
      new_table, "let " ^ vb.vdecl.dname ^ " = " ^ s ^ " in "


and func_bindings_to_string table bindings e  =
  let proc (tabl, s) fb =
    let (new_tabl, new_s) = func_bind_to_string tabl fb in
    new_tabl, s ^ new_s 
  in
  let (new_table, bstr) = List.fold_left proc (table, "") (List.rev bindings) in
  let (s, et) = expr_to_string new_table e in 
  bstr ^ s, et

and func_bind_to_string table fb =
   try 
     ignore (sym_table_lookup table fb.fdecl.fname); (* make sure id doesn't already exist *)
     raise (Error("Duplicate function identifier: " ^  fb.fdecl.fname))
  with No_such_symbol_found -> 
     let build_table (tabl, args_t) decl =
       let new_tabl = StringMap.add decl.dname decl.dtype tabl in
       new_tabl, decl.dtype :: args_t
     in
     let func_table, args_t = List.fold_left build_table (StringMap.empty, []) fb.fdecl.fargs in
     let func_t = FuncType { args_t = List.rev args_t; return_t = fb.fdecl.freturn } in
     let new_table = { table with table = StringMap.add fb.fdecl.fname func_t table.table } in
     let (body_s, body_t) = expr_to_string { table = func_table; parent = Some(new_table) } fb.body in
     if body_t <>  fb.fdecl.freturn  then
       raise (Error("mismatched return and function body types: " ^ (string_of_fv_type body_t) ^ " " ^ (string_of_fv_type fb.fdecl.freturn)))
     else
       let arg_names = List.map (fun decl -> decl.dname) fb.fdecl.fargs in
       let oid = id_to_ocaml_id fb.fdecl.fname in
       new_table, "let rec " ^ oid  ^ " " ^ (String.concat " " arg_names) ^ " unit = \n " ^ body_s ^ "\nin\n"
       (* todo : memoization support *)
  
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
  | FuncBind(bindings, e) -> func_bindings_to_string table bindings e
  | Noexpr -> "", ValType(Void)
  | ListBuilder(l) -> list_to_string table l
  | _ -> raise (Error "unsupported expression type")

let translate prog =
  (*print_endline (string_of_expr "" prog);*)
  let init_table = List.fold_left (fun tabl (id, id_t) -> StringMap.add id id_t tabl) StringMap.empty Builtin.builtins in
  let global_sym_table = { table = init_table; parent = None } in
  let s, _ = expr_to_string global_sym_table prog in
  "open Builtin\n\nlet _ =\n" ^ s
  
  
