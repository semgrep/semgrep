(* functions that has format string on the n+1th argument *)
val printf_like_functions_list: (string * int) list

(* modifies Error_php._errors by side effect *)
val check:
   Cst_php.program -> unit
