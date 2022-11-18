(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Handles all file include/exclude logic for semgrep

   Assumes file system does not change during it's existence to cache
   files for a given language etc. If file system changes (i.e. git checkout),
   create a new TargetManager object

   If respect_git_ignore is true then will only consider files that are
   tracked or (untracked but not ignored) by git

   If git_baseline_commit is true then will only consider files that have
   changed since that commit

   If allow_unknown_extensions is set then targets with extensions that are
   not understood by semgrep will always be returned by get_files. Else will discard
   targets with unknown extensions

   TargetManager not to be confused with https://jobs.target.com/search-jobs/store%20manager

   Translated from target_manager.py

   LATER: we probably want to merge this file with Find_target.ml
   in semgrep-core.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = {
  exclude : string list;
  include_ : string list;
  max_target_bytes : int;
  respect_git_ignore : bool;
  (* TODO? use, and better parsing of the string? a Git.version type? *)
  baseline_commit : string option;
  (* TODO: use *)
  scan_unknown_extensions : bool;
}
[@@deriving show]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let get_targets conf target_roots =
  Find_target.select_global_targets ~includes:conf.include_
    ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
    ~respect_git_ignore:conf.respect_git_ignore target_roots
