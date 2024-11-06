(*
   Turn tokenized lines into a tree, based on:
   - indentation,
   - matching delimiters (), [], or {} within the same line.
*)

type error = { loc : Loc.t; msg : string }

val of_lexbuf : ?is_doc:bool -> Lexing.lexbuf -> (Pattern_AST.t, error) result
(** NOTE: Errors can only be returned when ~is_doc:false. *)

val of_src : ?is_doc:bool -> Src_file.t -> (Pattern_AST.t, error) result
(** NOTE: Errors can only be returned when ~is_doc:false. *)
