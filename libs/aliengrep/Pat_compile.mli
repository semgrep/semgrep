(*
   Compile a pattern into a regexp.
*)

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)
[@@deriving show]

(* metavariable kind, bare name *)
type metavariable = metavariable_kind * string [@@deriving show]

type t = private {
  pcre_pattern : string;
  pcre : Pcre.regexp;
  metavariable_groups : metavariable array;
}

(*
   Convert a pattern AST into a PCRE pattern and the array of metavariables
   corresponding to the PCRE matching groups.
*)
val compile : Conf.t -> Pat_AST.t -> t

(* Shortcut for all parsing + compilation *)
val from_string : Conf.t -> string -> t
