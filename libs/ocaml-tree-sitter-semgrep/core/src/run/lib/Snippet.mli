(*
   Functions to extract a good snippet of code, with the error highlighted.
*)

type position = Tree_sitter_bindings.Tree_sitter_output_t.position = {
  row: int; (* line number, starting from 0. *)
  column: int; (* position within the line, starting from 0. *)
}

type snippet_fragment =
  | Normal of string
  | Highlight of string
  | Ellipsis

type snippet_line = snippet_fragment list

(*
   A snippet is a list of lines broken up into fragments to be highlighted
   or not.
*)
type t = snippet_line list

(*
   Extract a snippet from the location in a source file.
*)
val extract :
  ?lines_before:int ->
  ?lines_after:int ->
  start_pos:position ->
  end_pos:position -> Src_file.t -> t

type style =
  | Auto (* best-effort guess based on stdout and stderr *)
  | Color (* use ANSI-terminal-compatible highlighting *)
  | Text (* use text-based highlighting *)

(*
   Render a snippet as text. If 'color' is true, the output will use
   color in a manner compatible with an ANSI terminal. Otherwise
   the text to be highlighted is underlined with '^^^'. The latter will
   produce incorrect results on lines containing multi-byte characters.

   Missing newlines are added automatically.
*)
val format : style:style -> t -> string
