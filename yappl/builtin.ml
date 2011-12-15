(* builtin ocaml functionality that generated code needs to access *)

open Random
open Ast
open Unix

module Builtin =
  struct
    let pred_special_var = "x"
    let builtins = ["rand", ValType Float; "seed", ValType Bool]

    let rec cond_eval pred f =
      let x = f ()
      in
      if pred x then 
	x
      else
	cond_eval pred f
	  
    let rand () = Random.float 1.0

    let seed () = Random.init (int_of_float (Unix.time ())); true
      
  end
