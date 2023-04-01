(*
   Compile a pattern into a regexp.
*)

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)

(* metavariable kind, bare name *)
type metavariable = metavariable_kind * string

type t = private {
  source_name : string;
  pattern : string;
  pattern_ast : Pat_AST.t;
  pcre_pattern : string;
  pcre : Pcre.regexp;
  (* Array of PCRE capturing groups. Each capturing group has a metavariable
     name. *)
  metavariable_groups : metavariable array;
}

(*
   Convert a pattern AST into a PCRE pattern and the array of metavariables
   corresponding to the PCRE matching groups.
   This function is for internal use. It's provided for testing and debugging
   purposes.
*)
val compile_ast :
  ?source_name:string -> Pat_AST.t -> string * metavariable array
