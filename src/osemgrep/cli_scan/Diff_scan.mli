(* TODO: rename this module to indicate that it only works on a git repo *)
(* TODO: provide a function to obtain the list of target files. *)

type diff_scan_func =
  ?diff_config:Diff_scan_config.t ->
  Fpath.t list ->
  Rule.rules ->
  Core_result.result_or_exn

(* Execute the engine again on the baseline checkout, utilizing only
 * the files and rules linked with matches from the head checkout
 *  scan. Subsequently, eliminate any previously identified matches
 * from the results of the head checkout scan.
 * Regarding the capabilities:
 *  - Cap.chdir and Cap.tmp for Git_wrapper.run_with_worktree
 *  - Cap.readdir is used via an unsafe call for recomputing some targets for
 *     interfile
 *)

val scan_baseline :
  < Cap.chdir ; Cap.tmp ; .. > ->
  Scan_CLI.conf ->
  Profiler.t ->
  string (* baseline commit *) ->
  Fpath.t list ->
  Rule.rules ->
  diff_scan_func ->
  Core_result.result_or_exn
