(*
   Identify suitable file targets based on filters and options from the
   command line, then for each rule.
*)

type baseline_handler = TODO
type file_ignore = TODO
type file_targeting_log
type path = string

(*
   Take a set of scanning roots which are files or folders and
   expand them into the set of files that could be targets for some
   rules.

   Usage: let global_targets = select_global_targets
*)
val select_global_targets :
  includes:string list ->
  excludes:string list ->
  ?max_target_bytes:int ->
  ?respect_git_ignore:bool ->
  ?baseline_handler:baseline_handler ->
  ?file_ignore:file_ignore ->
  scanning_roots:path list ->
  path Set_.t

(*
   For one rule or multiple rules, select all the targets of these
   rules.

   Usage: let rule_targets = filter_targets_for_rule global_targets rule
*)
val filter_targets_for_rule :
  ?allow_unknown_extensions:bool -> Rule.t -> path Set_.t -> path Set_.t
