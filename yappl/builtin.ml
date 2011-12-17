(* builtin ocaml functionality that generated code needs to access *)

open Random
open Ast
open Unix

module Builtin =
  struct
    let pred_special_var = "x"
    let builtins = ["print", ValType Bool; "rand", ValType Float; "seed", ValType Bool]

    let rec cond_eval pred f =
      let x = f ()
      in
      if pred x then 
	x
      else
	cond_eval pred f

    let rand () = Random.float 1.0

    let seed () = Random.init (int_of_float (Unix.time ())); true

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


    
