
(* This uses an external "globbing" library whose syntax is similar
 * to UNIX globbing (as in .gitignore file for example).
 * See https://dune.readthedocs.io/en/stable/concepts.html#glob
 * for more information on its syntax.
 *)
type glob

type filters = {
  excludes: glob list;
  includes: glob list;
  exclude_dirs: glob list;
}

exception GlobSyntaxError of string
(* may raise GlobSyntaxError *)
val mk_filters: 
  excludes: string list -> includes: string list -> exclude_dirs: string list->
  filters

(* entry point *)
val filter: filters -> Common.filename list -> Common.filename list

