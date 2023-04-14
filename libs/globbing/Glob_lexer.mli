(*
   Parse a glob pattern according to glob(3) and glob(7), which is the
   POSIX standard for old-fashioned shell globbing patterns for matching
   file paths. Additionally, we support '**' as per the gitignore
   specification.
*)

exception Syntax_error of string

(* Parse a pattern or fail with exception 'Syntax_error' *)
val parse_string : string -> Glob_pattern.t
