val str_of_ident : AST_generic.ident -> string

(* expr conversions *)

exception NotAnExpr

val expr_to_pattern : AST_generic.expr -> AST_generic.pattern

(* may raise NotAnExpr *)
val pattern_to_expr : AST_generic.pattern -> AST_generic.expr

(* may raise NotAnExpr *)
val argument_to_expr : AST_generic.argument -> AST_generic.expr
val expr_to_type : AST_generic.expr -> AST_generic.type_
val expr_to_class_parent : AST_generic.expr -> AST_generic.class_parent

(* should avoid; used mainly during expr->condition migration for If/While/..*)
val cond_to_expr : AST_generic.condition -> AST_generic.expr

(* stmt conversions *)

val vardef_to_assign :
  AST_generic.entity * AST_generic.variable_definition -> AST_generic.expr

val funcdef_to_lambda :
  AST_generic.entity * AST_generic.function_definition ->
  AST_generic.resolved_name option ->
  AST_generic.expr

val funcbody_to_stmt : AST_generic.function_body -> AST_generic.stmt

(* name building *)

val name_of_id : AST_generic.ident -> AST_generic.name
val name_of_ids : AST_generic.dotted_ident -> AST_generic.name

val name_of_ids_with_opt_typeargs :
  (AST_generic.ident * AST_generic.type_arguments option) list ->
  AST_generic.name

val add_id_opt_type_args_to_name :
  AST_generic.name ->
  AST_generic.ident * AST_generic.type_arguments option ->
  AST_generic.name

val add_type_args_to_name :
  AST_generic.name -> AST_generic.type_arguments -> AST_generic.name

val add_type_args_opt_to_name :
  AST_generic.name -> AST_generic.type_arguments option -> AST_generic.name

(* takes in a suffix, and then its name *)
val add_suffix_to_name :
  AST_generic.ident -> AST_generic.name -> AST_generic.name

(* Tries to re-interpreted a DotAccess expression a.b.c as an IdQualified. *)
val name_of_dot_access : AST_generic.expr -> AST_generic.name option

(* name conversions *)

(* You should avoid this function! *)
val dotted_ident_of_name : AST_generic.name -> AST_generic.dotted_ident

(* misc *)

val parameter_to_catch_exn_opt :
  AST_generic.parameter -> AST_generic.catch_exn option

val opt_to_label_ident : AST_generic.ident option -> AST_generic.label_ident
val gensym_counter : int ref
val gensym : unit -> int

val has_keyword_attr :
  AST_generic.keyword_attribute -> AST_generic.attribute list -> bool

val abstract_for_comparison_any : AST_generic.any -> AST_generic.any
(** Abstract away position and svalue for comparison
 * with polymorphic operators.
*)

val is_associative_operator : AST_generic.operator -> bool
(** Test whether an operator is suitable for associative-matching.  *)

val ac_matching_nf :
  AST_generic.operator ->
  AST_generic.argument list ->
  AST_generic.expr list option
(** [ac_matching_nf op args] converts the operands [args] of an
 * AC-operator [op] to a normal form (NF) better suited for AC-matching.
 * Essentially, we flatten out all [op]-operations. Note that this is not
 * a canonical form (i.e., it is not a unique representation).
 * E.g.
 *    ac_matching_nf And [a; Call(Op And, [b; c])] = Some [a; b; c]
 * The function returns [None] if [op] is not an AC-operator. It also
 * returns [None] if the operands have an unexpected shape (see code),
 * in which case we just log an error and skip AC-matching (rather
 * than crashing). *)

val undo_ac_matching_nf :
  Parse_info.t ->
  AST_generic.operator ->
  AST_generic.expr list ->
  AST_generic.expr option
(** [undo_ac_matching_nf tok op args_nf] folds [args_nf] using [op]
 * (in a sense, it "undoes" [ac_matching_nf]). Here [tok] is the token of
 * the operand [op] in the source file. Note that this does not remember
 * the original grouping of the operands!
 * E.g.
 *    undo_ac_matching_nf tok And [a; b; c] = Call(Op And, [Call(Op And, [a; b]); c])
 *)

(* AST_generic_ conversions *)

val conv_op : AST_generic_.operator -> AST_generic.operator
val conv_incr : AST_generic_.incr_decr -> AST_generic.incr_decr
val conv_prepost : AST_generic_.prefix_postfix -> AST_generic.prefix_postfix

val conv_incdec :
  AST_generic_.incr_decr * AST_generic_.prefix_postfix ->
  AST_generic.incr_decr * AST_generic.prefix_postfix

val conv_class_kind :
  AST_generic_.class_kind * Parse_info.t ->
  AST_generic.class_kind * Parse_info.t

val conv_function_kind :
  AST_generic_.function_kind * Parse_info.t ->
  AST_generic.function_kind * Parse_info.t
