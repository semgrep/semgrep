(*
   Target discovery can now be done in semgrep-core using scanning roots
   passed by pysemgrep.

   This module takes care of splitting targets by language as requested
   by the legacy semgrep-core input interface.
*)

(* reused for Git_remote.ml in pro *)
val split_jobs_by_language :
  Find_targets.conf -> Rule.t list -> Fpath.t list -> Lang_job.t list

val targets_and_rules_of_lang_jobs :
  Lang_job.t list -> Target.t list * Rule.t list

(* Helper used in Test_subcommand.ml and Core_scan.ml where we now
   discover targets in semgrep-core rather than in pysemgrep.
   Used also by `semgrep show dump-targets <dir> <rules_config>`
*)
val targets_for_files_and_rules : Fpath.t list -> Rule.t list -> Target.t list
