val check :
  hook:(Pattern_match.t -> unit) ->
  ?mvar_context:Metavariable.bindings option ->
  ?range_filter:(Tok.location * Tok.location -> bool) ->
  Rule_options.t * Equivalence.equivalences ->
  Mini_rule.rules ->
  Common.filename * Lang.t * Target.t ->
  Pattern_match.t list
(** Match mini-rules (patterns) against a target file.
  *
  * @param range_filter A predicate that defines "regions of interest" when
  * matching expressions. This is e.g. used for optimizing `pattern: $X` by
  * filtering out any matches that are not within the ranges of preceding
  * `pattern-inside`s. Typically you will check that the range of the
  * expression is strictly inside your area(s) of interest, no need to
  * worry about sub-expressions, they will be visited regardless. *)

val last_matched_rule : Mini_rule.t option ref

(* used by tainting *)
val match_e_e : Mini_rule.t -> AST_generic.expr Matching_generic.matcher

(* for unit testing *)
val match_any_any : AST_generic.any Matching_generic.matcher
