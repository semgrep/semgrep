(*
   Parse a glob pattern according to glob(3) and glob(7), which is the
   POSIX standard for old-fashioned shell globbing patterns for matching
   file paths. Additionally, we support '**' as per the gitignore
   specification.

   may raise Glob.Lexer.Syntax_error
*)
val parse_string : string -> Pattern.t
