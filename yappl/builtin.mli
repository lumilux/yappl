open Ast

module Builtin :
    sig
      val pred_special_var : string
      val builtins : (string * fv_type) list

      val cond_eval : ('a -> bool) -> (unit -> 'a) -> 'a
      val rand : unit -> float
      val seed : unit -> bool
    end
