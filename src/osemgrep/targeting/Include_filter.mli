(*
   Similar to Gitignore_filter but select paths to be kept rather than ignored.

   Negated patterns are not supported as they wouldn't make much sense here.
   Exclusions can be made via --exclude or gitignore/semgrepignore.

   Example: --include a --include '*.ml'
     /a/b.c  --> selected because /a matches a pattern
     /x/a    --> selected because a matches a pattern (in folder /x)
     x.ml    --> selected because x.ml matches a pattern
     x.c     --> excluded because x.c doesn't match a pattern

   The logic is different from gitignore/semgrepignore and --exclude.
   It is applied before all of these filters.

   In semgrep, this is used in two contexts:
   - pre-filtering of all target files based on the --include command-line
     options
   - per-rule filtering, which is an extra filtering step requested
     by specific semgrep rules
*)

type t

val create : project_root:Fpath.t -> string list -> t
val select : t -> Ppath.t -> Gitignore.status * Gitignore.selection_event list
