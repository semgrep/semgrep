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
val inspect_file_p : Lang.t -> Common.filename -> bool

val inspect_file :
  Lang.t ->
  Common.filename ->
  (Common.filename, Semgrep_core_response_t.skipped_target) result

(*
   Split selected files (left) from excluded files (right).
*)
val inspect_files :
  Lang.t ->
  Common.filename list ->
  Common.filename list * Semgrep_core_response_t.skipped_target list
