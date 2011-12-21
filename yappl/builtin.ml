(* builtin ocaml functionality that yappl-generated code needs to access *)

open Random
open Ast
open Unix

module Builtin =
  struct
    let pred_special_var = "pred_var"
    let builtins = ["print", FuncType { args_t = []; return_t = ValType Bool };
		    "rand", FuncType  { args_t = []; return_t = ValType Float };
		    "seed", FuncType  { args_t = []; return_t = ValType Bool }   ]

    let rec cond_eval pred f =
      let x = f ()
      in
      if pred x then 
	x
      else
	cond_eval pred f

    let rand () = Random.float 1.0

    let seed () = Random.init (int_of_float(10000. *. fst(modf(Unix.gettimeofday())))); true

(* Prototype for memoization:
    let no_mem_my_func arg1 arg2 = 
      arg1 + arg2

    let my_func tabl arg1 arg2 =
      try 
	Hashtbl.find tabl (arg1, arg2) 
      with Not_found ->
	let result = no_mem_my_func arg1 arg2 in
	Hashtbl.add tabl (arg1, arg2) result;
	result
*)
	  
      
  end


    
