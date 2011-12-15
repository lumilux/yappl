module Builtin :
    sig
      val pred_special_var : string
      val cond_eval : ('a -> bool) -> ('b -> 'a) -> 'b -> 'a
    end
