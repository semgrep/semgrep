(*
   Find target files suitable to be analyzed by semgrep,
   regardless of rules or languages.
 *)

type project_root =
  | Filesystem of Rfpath.t
  (* currently used to optimize Semgrep query console *)
  | Git_remote of git_remote

and git_remote = { url : Uri.t } [@@deriving show]

(*
   Abstract type designed for quickly determining whether a path is in the
   set of explicit targets. An explicit target is a target file passed directly
   on the command line.
*)
module Explicit_targets : sig
  type t

  val empty : t
  val of_list : Fpath.t list -> t
  val to_list : t -> Fpath.t list

  (* Fast O(1) operation *)
  val mem : t -> Fpath.t -> bool
  val pp : Format.formatter -> t -> unit
end

type conf = {
  (* global exclude list, passed via semgrep --exclude (a glob) *)
  exclude : string list;
  (* global include list, passed via semgrep --include (a glob)
   * Those are flags copied from grep (and ripgrep).
   *)
  include_ : string list option;
  max_target_bytes : int;
  (* whether or not follow what is specified in the .gitignore
   * TODO? what about .semgrepignore?
   *)
  respect_gitignore : bool;
  (* Language-specific filtering: CLI option '--scan-unknown-extensions'
     allows explicit targets (files on the command line) to bypass
     normal language detection.
     This forces all target files passed explicitly on the
     command line to be analyzed any analyzer specified in rules (--config) or
     command-line patterns (-e/-f):

       semgrep scan --scan-unknown-extensions dockerfiles/*

     Target files discovered by scanning folders are not affected by
     this option.
  *)
  always_select_explicit_targets : bool;
  (* Paths to target files specified directly on the command-line.
     For the purpose of --scan-unknown-extensions, this
     could also be stored as a bool alongside each target file.
     It's a hash table for fast access.
  *)
  explicit_targets : Explicit_targets.t;
  (* osemgrep-only: option (see Git_project.ml and the force_root parameter) *)
  force_project_root : project_root option;
  (* osemgrep-only: exclude scanning large files based on
      max_target_bytes, default true *)
  exclude_minified_files : bool;
  (* TODO: not used for now *)
  baseline_commit : string option;
  diff_depth : int;
}
[@@deriving show]

val default_conf : conf

(* Entry point used by osemgrep.

   Take a set of scanning roots which are files or folders (directories) and
   expand them into the set of files that could be targets for some
   rules. Return a list of deduplicated paths to regular files.

   If a scanning root is a symbolic link, it is dereferenced recursively
   until it results in a regular file or a directory. However, we
   don't follow symbolic links discovered when listing directories.

   The order of the files isn't guaranteed to be anything special
   at the moment but we could obey some ordering if it makes sense to do it
   here.

   This may raise Unix.Unix_error if the scanning root does not exist.
*)
val get_targets :
  conf ->
  Scanning_root.t list ->
  Scanning_root.processed list * Semgrep_output_v1_t.skipped_target list

(* Same as get_targets but drop the ppath (path within the project) *)
val get_target_fpaths :
  conf ->
  Scanning_root.t list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list

(* internals used also in Find_targets_old.ml *)
val get_reason_for_exclusion :
  Gitignore.selection_event list -> Semgrep_output_v1_t.skip_reason
