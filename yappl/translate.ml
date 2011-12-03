(* translate to ocaml *)

open Ast

let val_bind_string table = function 
    vd, e -> (* todo *)

and let val_decl_to_string table = function 
 (* todo *)

and let expr_to_string table = function
      Literal(l) -> lit_to_string l  
    | ValBind(bindings, e) -> 
	let bstr = String.concat List.iter (val_bind_to_string table) bindings in
	bstr ^ "\n" ^ (expr_to_string table e)
  in

let translate prog =
  let global_sym_table = { table = Some(StringMap.empty); parent = None } in
  (* todo: handle print and rand *)
  
  
