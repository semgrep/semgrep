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
  | Any of AST_generic.any
[@@deriving show, eq]

(* note that the mvalue acts as the value of the metavar and also
   as its concrete code "witness". You can get position information from it
   (if it is not Tok.Ab(stractPos)).
*)
type bindings = (mvar * mvalue) list [@@deriving show, eq]

(* Mvalue equality reduces to equality on ASTs, which ignores the tokens
   and instead compares the structure. It may optionally include the ident
   info IDs, but the point is that it's agnostic to the positions of the
   mvalues.
   Sometimes we don't want this behavior. For instance, if we have two
   matches which have $A bound to "true", but in different places in the
   source, we might want to consider those matches different.
   This equality function simply first discriminates on location of the
   mvalues, and then checks for their literal equality, to make that
   possible.
*)
val location_aware_equal_mvalue : mvalue -> mvalue -> bool

(* return whether a string could be a metavariable name (e.g., "$FOO", but not
 * "FOO"). This mostly check for the regexp $[A-Z_][A-Z_0-9]* but
 * also handles special variables like $_GET in PHP which are actually
 * not metavariables.
 *)
val is_metavar_name : string -> bool

(* example: "$...FOO" is a metavariable ellipsis *)
val is_metavar_ellipsis : string -> bool
val mvars_of_regexp_string : string -> mvar list

(* metavariables like $_ *)
val is_anonymous_metavar : string -> bool

(* example: "$1" *)
val is_metavar_for_capture_group : string -> bool
val ii_of_mval : mvalue -> Tok.t list
val str_of_mval : mvalue -> string
val range_of_mvalue : mvalue -> (string (* filename *) * Range.t) option

(* we sometimes need to convert to an any to be able to use
 * Lib_AST.ii_of_any, or Lib_AST.abstract_position_info_any
 *)
val mvalue_to_any : mvalue -> AST_generic.any
val mvalue_of_any : AST_generic.any -> mvalue

(* This is used for metavariable-pattern: where we need to transform the content
 * of a metavariable into a program so we can use evaluate_formula on it *)
val program_of_mvalue : mvalue -> AST_generic.program option

(* See the comment in AST_generic_equals.ml for the difference between
 * the Syntactic and Structural equal.
 *)
module Syntactic : sig
  val equal_mvalue : mvalue -> mvalue -> bool
  val equal_bindings : bindings -> bindings -> bool
end

module Structural : sig
  val equal_mvalue : mvalue -> mvalue -> bool
  val equal_bindings : bindings -> bindings -> bool
end
