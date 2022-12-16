(*
   Utilities for dealing with Unicode issues.

   Notes for later:

   Eventually, we want to support Unicode properly! At a minimum, we want:
   - comparing raw UTF-8 data is done correctly i.e. the pattern
     '"😀"' matches the target code '"😀"' and not '"🚀"'.
   - positions in source code (number of lines, position within the line)
     must be interpreted identically in javascript, python, and ocaml.
*)

(*
   Convert each non-ascii byte by one ascii byte.
   The default replacement byte is 'X'.

   This is meant to be used as a hack to tolerate non-ascii input in
   lexers that only support ascii.
*)
val input_and_replace_non_ascii :
  replacement_byte:char -> in_channel -> string
