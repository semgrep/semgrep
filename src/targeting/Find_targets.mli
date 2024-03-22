(*
   Find target files suitable to be analyzed by semgrep,
   regardless of rules or languages.
 *)

type git_remote = { url : Uri.t } [@@deriving show]

type project_root = Git_remote of git_remote | Filesystem of Rfpath.t
[@@deriving show]

(*
   Abstract type designed for quickly determining whether a path is in the
   set of explicit targets. An explicit target is a target file passed directly
   on the command line.

   This is a bit fragile as it assumes that target file paths found in the file
   system have the same form as those passed on the command line. It won't
   work with unnormalized paths such as 'foo/../bar.js' that will likely
   be rewritten into 'bar.js'. See:

     $ git ls-files libs/../README.md
     README.md

   This results in 'README.md' being treated as non-explicit target file.

   TODO: use pairs (project, ppath) instead as keys? If we use a dedicated
   record for targets, we can extract the pair (project, ppath):

     type target = {
       project: Project.t; (* provides normalized project root *)
       path: Fppath.t; (* provides (normalized) ppath *)
     }

   If we go this path, we could also add a field 'is_explicit: bool' to the
   target type.
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
  (* global exclude list, passed via semgrep --exclude *)
  exclude : string list;
  (* global include list, passed via semgrep --include
   * Those are flags copied from grep (and ripgrep).
   *)
  include_ : string list option;
  max_target_bytes : int;
  (* whether or not follow what is specified in the .gitignore
   * TODO? what about .semgrepignore?
   *)
  respect_gitignore : bool;
  (* TODO: not used for now *)
  baseline_commit : string option;
  (* TODO: not used for now *)
  diff_depth : int;
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
  project_root : project_root option;
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
  Fppath.t list * Semgrep_output_v1_t.skipped_target list

(* Same as get_targets but drop the ppath (path within the project) *)
val get_target_fpaths :
  conf ->
  Scanning_root.t list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list

(* internals used also in Find_targets_old.ml *)
val get_reason_for_exclusion :
  Gitignore.selection_event list -> Semgrep_output_v1_t.skip_reason
