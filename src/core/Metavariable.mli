(* metavariable name (e.g., "$FOO") *)
type mvar = string [@@deriving show, eq, hash]

(* metavariable content *)
type mvalue =
  | Id of AST_generic.ident * AST_generic.id_info option
  | N of AST_generic.name
  | E of AST_generic.expr
  | S of AST_generic.stmt
  | T of AST_generic.type_
  | P of AST_generic.pattern
  | Raw of AST_generic.raw_tree
  | XmlAt of AST_generic.xml_attribute
  (* Those cases can be now empty with $...XXX metavariables *)
  | Ss of AST_generic.stmt list
  | Args of AST_generic.argument list
  | Params of AST_generic.parameter list
  | Xmls of AST_generic.xml_body list
  (* Text below is used to match the content of a string or atom, without the
   * enclosing quotes. For a string this can actually be empty. Includes both
   * the original string token with the enclosing quotes (for use in autofix
   * where we want to print the original quotes when possible) and the modified
   * token without the quotes for use in, e.g. metavariable-pattern.
   *)
  | Text of
      string
      * (* token without enclosing quotes *) AST_generic.tok
      * (* original token *) AST_generic.tok
[@@deriving show, eq, hash]

(* note that the mvalue acts as the value of the metavar and also
   as its concrete code "witness". You can get position information from it
   (if it is not Tok.Ab(stractPos)).
*)
type bindings = (mvar * mvalue) list [@@deriving show, eq, hash]

(* return whether a string could be a metavariable name (e.g., "$FOO", but not
 * "FOO"). This mostly check for the regexp $[A-Z_][A-Z_0-9]* but
 * also handles special variables like $_GET in PHP which are actually
 * not metavariables.
 *)
val is_metavar_name : string -> bool

(* example: "$...FOO" is a metavariable ellipsis *)
val is_metavar_ellipsis : string -> bool

(* example: "$1" *)
val is_metavar_for_capture_group : string -> bool
val ii_of_mval : mvalue -> Tok.t list
val str_of_mval : mvalue -> string
val range_of_mvalue : mvalue -> (Common.filename * Range.t) option

(* we sometimes need to convert to an any to be able to use
 * Lib_AST.ii_of_any, or Lib_AST.abstract_position_info_any
 *)
val mvalue_to_any : mvalue -> AST_generic.any
val mvalue_of_any : AST_generic.any -> mvalue option

(* This is used for metavariable-pattern: where we need to transform the content
 * of a metavariable into a program so we can use evaluate_formula on it *)
val program_of_mvalue : mvalue -> AST_generic.program option

module Structural : sig
  val equal_mvalue : mvalue -> mvalue -> bool
  val equal_bindings : bindings -> bindings -> bool
end

module Referential : sig
  val equal_mvalue : mvalue -> mvalue -> bool
  val equal_bindings : bindings -> bindings -> bool
  val hash_bindings : bindings -> int
end
