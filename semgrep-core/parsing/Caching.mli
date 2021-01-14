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

(*
   Create a memoized function that matches a pattern against a target AST
   in the environment min_env. min_env must contain the bound metavariables
   needed to match the rest of the pattern.
*)
module Memoize : sig
  type env = Metavars_generic.Env.t
  type pattern = AST_generic.stmt (* only works for statements at the moment *)
  type target = AST_generic.stmt

  (*
     Create a memoized match_ function.

     Usage:
       let memoized_match = Caching.Memoize match_ in
       match_ memoized_match env pattern_stmt target_stmt

     The original match_ function must take the memoized version of itself
     as argument so as to allow memoized recursive calls.
  *)
  val create :
    ((env -> pattern -> target -> 'a) -> env -> pattern -> target -> 'a) ->
    env -> pattern -> target -> 'a
end
