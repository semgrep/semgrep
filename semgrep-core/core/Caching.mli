(*
   Decorate a pattern and target ASTs to make the suitable for memoization
   during matching.
*)

(*
   These functions set the fields that are required for
   semgrep matching with memoization. This is one-time initialization.
   These fields will not be mutated during the matching process.
*)
val prepare_pattern : ?debug:bool -> AST_generic.any -> unit
val prepare_target : AST_generic.program -> unit

module Cache : sig
  type 'a t

  type env = Metavars_generic.Env.t
  type pattern = AST_generic.stmt (* only works for statements at the moment *)
  type target = AST_generic.stmt

  val create : unit -> 'a t

  (*
     Match a pattern against a statement, using the cache and the provided
     match function.

     Usage:

       match_stmt cache compute_match_stmt env pat_stmt target_stmt

     where 'compute_match_stmt' is the function that the cache memoizes.
     It is the user's responsibility to always use the same
     'compute_match_stmt' with a given cache.
  *)
  val match_stmt :
    'a t ->
    (env -> pattern -> target -> 'a) ->
    env -> pattern -> target -> 'a
end
