(* builtin ocaml functionality that generated code needs to access *)

module Builtin =
  struct
    let pred_special_var = "x"

    let rec cond_eval pred f arg =
      let x = f arg
      in
      if pred x then 
	x
      else
	cond_eval pred f arg
  end
