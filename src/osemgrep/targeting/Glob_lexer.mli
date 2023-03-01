(*
   Parse a glob pattern according to glob(3) and glob(7), which is the
   POSIX standard for old-fashioned shell globbing patterns for matching
   file paths. Additionally, we support '**' as per the gitignore
   specification.

   Examples:

   *.c          # all local files with a '.c' extension
   /tmp/**      # all valid paths under '/tmp/'
   Thing.ml?    # matches 'Thing.ml' as well as 'Thing.mli', 'Thing.mll', etc.
   [a-z0-9]     # matches a single character in these ranges
*)

exception Syntax_error of string

(* Parse a pattern or fail with exception 'Syntax_error' *)
val parse_string : string -> Glob_matcher.pattern
