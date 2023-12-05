(*
   Path information for a file in a git project, which includes:
   - a path in the file system
   - the path relative to the project root
*)

(* For gitignore filtering, we need to operate on Ppath (see
 * the signature of Gitignore_filter.select()), but when semgrep
 * displays findings or errors, we want filenames derived from
 * the scanning roots, not the root of the project. This is why we need to
 * keep both the fpath and ppath for each target file as we walked
 * down the filesystem hierarchy.
 *)
type t = { fpath : Fpath.t; ppath : Ppath.t }

val compare : t -> t -> int
