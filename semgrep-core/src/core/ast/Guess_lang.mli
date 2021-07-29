(*
   Guess whether a given file is indeed written in the specified
   programming language.

   - uses file name, permissions, file contents
   - this is used to filter candidates for a given language
   - a given file may be in multiple languages

   This will exclude files we don't want to handle with semgrep, such
   as the '.min.js' files (JavaScript minified files which are not human-
   readable and usually really big) or '.d.ts' (TypeScript typed interfaces
   for which we don't have a parser).
*)
val is_acceptable : Lang.t -> Common.filename -> bool
