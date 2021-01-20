(*
   Decorate a pattern and target ASTs to make the suitable for memoization
   during matching.
*)

(*
   These functions set the fields that are required for
   semgrep matching with memoization. This is one-time initialization.
   These fields will not be mutated during the matching process.
*)
val prepare_pattern : AST_generic.any -> unit

module Cache : sig
  type 'a t

  type pattern = AST_generic.stmt list
  type target = AST_generic.stmt list

  val create : unit -> 'a t

  (*
     Match a pattern against a statement, using the cache and the provided
     match function.

     Usage:

       match_stmt get_env cache compute_match_stmt pat_stmt target_stmt env

     where 'compute_match_stmt' is the function that the cache memoizes.
     It is the user's responsibility to always use the same
     'compute_match_stmt' with a given cache.
  *)
  val match_stmt_list :
    ('tin -> Metavars_generic.Env.t) ->
    'tout t ->
    (pattern -> target -> 'tin -> 'tout) ->
    pattern -> target -> 'tin -> 'tout
end

val print_stats : unit -> unit
