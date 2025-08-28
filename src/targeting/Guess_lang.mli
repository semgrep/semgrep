(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
val inspect_file_p : Lang.t -> Fpath.t -> bool

val inspect_file :
  Lang.t -> Fpath.t -> (Fpath.t, Semgrep_output_v1_t.skipped_target) result

(*
   Split selected files (left) from excluded files (right).
*)
val inspect_files :
  Lang.t ->
  Fpath.t list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list

(*
   Get the first 'block_size' bytes of the file, which is ideally obtained
   from a single filesystem block. 'block_size' defaults to 4096.

   This is intended for peeking into target file contents and finding out
   if it looks like it's in the desired format.
*)
val get_first_block : ?block_size:int -> Fpath.t -> string
