(*
   A cache meant to avoid costly operations of determining whether a target
   is suitable over and over again.

   Some rules will use 'include' (required_path_patterns) and 'exclude'
   (excluded_path_patterns) to select targets that don't have an extension
   such as 'Dockerfile'. We expect most rules written for a language
   to use the same combination of include/exclude. This allows caching
   across the many rules that target the same language.
*)
type target_cache

val create_cache : unit -> target_cache

(*
   ??
*)
val filter_target_for_lang :
  cache:target_cache ->
  lang:Xlang.t ->
  required_path_patterns:string list ->
  excluded_path_patterns:string list ->
  Fpath.t ->
  bool

(* TODO?
      For a rule, select all the applicable targets of these rules.
      Preserve the original order.

      Usage: let rule_targets = filter_targets_for_rule ~cache global_targets rule
   val filter_targets_for_rule :
     target_cache -> Rule.t -> Fpath.t list -> Fpath.t list

   (*
      Determine whether a rule is applicable to a file.
   *)
   val filter_target_for_rule : target_cache -> Rule.t -> Fpath.t -> bool
*)
