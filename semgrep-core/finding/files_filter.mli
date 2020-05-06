(*s: semgrep/finding/files_filter.mli *)

(* This uses an external "globbing" library whose syntax is similar
 * to UNIX globbing (as in .gitignore file for example).
 * See https://dune.readthedocs.io/en/stable/concepts.html#glob
 * for more information on its syntax.
 *)
type glob

(*s: type [[Files_filter.filters]] *)
type filters = {
  excludes: glob list;
  includes: glob list;
  exclude_dirs: glob list;
  include_dirs: glob list;
}
(*e: type [[Files_filter.filters]] *)

(*s: exception [[Files_filter.GlobSyntaxError]] *)
exception GlobSyntaxError of string
(*e: exception [[Files_filter.GlobSyntaxError]] *)
(*s: signature [[Files_filter.mk_filters]] *)
(* may raise GlobSyntaxError *)
val mk_filters: 
  excludes: string list -> includes: string list -> exclude_dirs: string list -> include_dirs: string list ->
  filters
(*e: signature [[Files_filter.mk_filters]] *)

(*s: signature [[Files_filter.filter]] *)
(* entry point *)
val filter: filters -> Common.filename list -> Common.filename list
(*e: signature [[Files_filter.filter]] *)

(*e: semgrep/finding/files_filter.mli *)
