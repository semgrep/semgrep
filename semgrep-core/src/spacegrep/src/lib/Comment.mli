(*
   Strip comments and the like as a preprocessing phase.

   The issue is that comment syntax need to be configurable but the
   ocamllex lexer isn't flexible enough. The solution adopted here
   is to make a copy of the original input while replacing comments
   by blanks.
*)

type delimiters = { start : string; end_ : string }

(*
   I don't like giving users the ability to specify regexps, but since
   some syntaxes may be tricky to configure without regexps, the current
   design restricts the user to a choice of predefined styles.
*)
type style = {
  name : string;
  (* end-of-line comment prefix *)
  start : string option;
  (* multiline comment delimiters *)
  delimiters : delimiters option;
}

(* Shorthand for creating a 'style' record of a give name. *)
val style : ?start:string -> ?delimiters:string * string -> string -> style

(* Any occurrence of '#' until the end of the line.
   Unlike proper Bash syntax, this doesn't take into account what precedes
   the '#' to determine whether it's a comment. *)
val shell_style : style

(* Multiline comment starting with '/*' and ending with the earliest occurrence
   of '*/'. *)
val c_style : style

(* Same as 'classic_c_style' but adds end-of-line comments that start with
   '//' like in C++. *)
val cpp_style : style

(* A list of the predefined styles above. *)
val predefined_styles : style list

(* Replace comments by whitespace so as to preserve the (line, column)
   position of the other characters in the file. *)
val remove_comments_from_string : style -> string -> string

val remove_comments_from_src : style -> Src_file.t -> Src_file.t

(**************************************************************************)
(* Command-line handling for spacecat and spacegrep *)
(**************************************************************************)

module CLI : sig
  (* Command-line options *)
  val comment_style_term : style option Cmdliner.Term.t

  val eol_comment_start_term : string option Cmdliner.Term.t

  val multiline_comment_start_term : string option Cmdliner.Term.t

  val multiline_comment_end_term : string option Cmdliner.Term.t

  (* Merge values collected from the command line into a style record. *)
  val merge_comment_options :
    comment_style:style option ->
    eol_comment_start:string option ->
    multiline_comment_start:string option ->
    multiline_comment_end:string option ->
    style
end
