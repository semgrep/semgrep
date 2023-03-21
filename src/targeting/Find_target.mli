(*
   Find target files suitable to be analyzed by semgrep.

   A file tree is a mix of files in different languages. Different rules
   will target different files. We provide two main operations:

   1. Select global targets: obtain all the files that Semgrep accepts to
      scan regardless of rules or languages.
   2. Filter suitable targets for a language or for a rule
      (multiple languages).
 *)

type conf = {
  exclude : string list;
  (* [!] include_ = None is the opposite of Some [].
     If a list of include patterns is specified, a path must match
     at least of the patterns to be selected. *)
  include_ : string list option;
  max_target_bytes : int;
  respect_git_ignore : bool;
  baseline_commit : string option;
  scan_unknown_extensions : bool;
}
[@@deriving show]

(* Entry point.

   Take a set of scanning roots which are files or folders (directories) and
   expand them into the set of files that could be targets for some
   rules. Return a list of deduplicated paths to regular files.

   If a scanning root is a symbolic link, it is dereferenced recursively
   until it results in a regular file or a directory. However, we
   don't follow symbolic links discovered when listing directories.

   The order of the files isn't guaranteed to be anything special
   at the moment but we could obey some ordering if it makes sense to do it
   here.

   Usage: let possible_targets = get_files scanning_roots

   This may raise Unix.Unix_error if the scanning root does not exist.
*)
val get_targets :
  conf ->
  Fpath.t list (* scanning roots *) ->
  Fpath.t list * Output_from_core_t.skipped_target list

type baseline_handler = TODO
type file_ignore = TODO

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
   For a rule, select all the applicable targets of these rules.
   Preserve the original order.

   Usage: let rule_targets = filter_targets_for_rule ~cache global_targets rule
*)
val filter_targets_for_rule :
  target_cache -> Rule.t -> Fpath.t list -> Fpath.t list

(*
   Determine whether a rule is applicable to a file.
*)
val filter_target_for_rule : target_cache -> Rule.t -> Fpath.t -> bool

(*
   Low-level version of 'filter_target_for_rule'.
*)
val filter_target_for_lang :
  cache:target_cache ->
  lang:Xlang.t ->
  required_path_patterns:string list ->
  excluded_path_patterns:string list ->
  Fpath.t ->
  bool

(*
   [legacy implementation used in semgrep-core]

   Scan a list of folders or files recursively and return a list of files
   in the requested language. This takes care of ignoring undesirable
   files, which are returned in the semgrep-core response format.

   Reasons for skipping a file include currently:
   - the file looks like it's the wrong language.
   - a 'skip_list.txt' file was found at a conventional location (see
     skip_list.ml in pfff).
   - files over a certain size.
   See Skip_target.ml for more information.

   If keep_root_files is true (the default), regular files passed directly
   to this function are considered ok and bypass the other filters.

   By default files are sorted alphabetically. Setting
   'sort_by_decr_size' will sort them be decreasing size instead.

   This is a replacement for Lang.files_of_dirs_or_files.
*)
val files_of_dirs_or_files :
  ?keep_root_files:bool ->
  ?sort_by_decr_size:bool ->
  Lang.t option ->
  Fpath.t list ->
  Fpath.t list * Output_from_core_t.skipped_target list

(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)
val sort_targets_by_decreasing_size :
  Input_to_core_t.target list -> Input_to_core_t.target list
