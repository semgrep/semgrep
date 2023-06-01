(*
   Find target files suitable to be analyzed by semgrep,
   regardless of rules or languages.
 *)

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
  respect_git_ignore : bool;
  (* TODO: not used for now *)
  baseline_commit : string option;
  (* TODO: not used for now *)
  scan_unknown_extensions : bool;
  (* osemgrep-only: option (see Git_project.ml and the force_root parameter) *)
  project_root : Fpath.t option;
}
[@@deriving show]

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
  Fpath.t list (* scanning roots *) ->
  Fpath.t list * Output_from_core_t.skipped_target list

(* internals used also in Find_targets_old.ml *)
val get_reason_for_exclusion :
  Gitignore.selection_event list -> Output_from_core_t.skip_reason
