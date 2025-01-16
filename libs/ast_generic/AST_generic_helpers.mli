val str_of_ident : AST_generic.ident -> string

(* expr conversions *)

exception NotAnExpr

val expr_to_pattern : AST_generic.expr -> AST_generic.pattern

(* This executes some simplifications, such as not producing
   ExprStmt (StmtExpr ...)
*)
val expr_to_stmt : AST_generic.expr -> AST_generic.stmt

(* may raise NotAnExpr *)
val pattern_to_expr : AST_generic.pattern -> AST_generic.expr

(* may raise NotAnExpr *)
val argument_to_expr : AST_generic.argument -> AST_generic.expr
val expr_to_type : AST_generic.expr -> AST_generic.type_
val expr_to_class_parent : AST_generic.expr -> AST_generic.class_parent
val expr_to_entity_name_opt : AST_generic.expr -> AST_generic.entity_name option

(* should avoid; used mainly during expr->condition migration for If/While/..*)
val cond_to_expr : AST_generic.condition -> AST_generic.expr

(* stmt conversions *)

val vardef_to_assign :
  AST_generic.entity * AST_generic.variable_definition -> AST_generic.expr

(* only translates an assign to a vardef if we definitely have a name on the LHS *)
val assign_to_vardef_opt :
  AST_generic.expr * AST_generic.tok * AST_generic.expr ->
  AST_generic.stmt option

val funcdef_to_lambda :
  AST_generic.entity * AST_generic.function_definition ->
  AST_generic.resolved_name option ->
  AST_generic.expr

val funcbody_to_stmt : AST_generic.function_body -> AST_generic.stmt

(* name building *)

val alias_of_ident : AST_generic.ident -> AST_generic.alias

val mk_import_from_kind :
  (* from *) AST_generic.ident ->
  (* as *) AST_generic.ident option ->
  AST_generic.import_from_kind

val id_of_import_from_kind : AST_generic.import_from_kind -> AST_generic.ident

val alias_opt_of_import_from_kind :
  AST_generic.import_from_kind -> AST_generic.alias option

val name_of_id : ?case_insensitive:bool -> AST_generic.ident -> AST_generic.name

val name_of_ids :
  ?case_insensitive:bool -> AST_generic.dotted_ident -> AST_generic.name

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

(* fetching a resolved name from Id, IdQualified and DotAccess *)
val resolved_name_of_dot_access :
  AST_generic.expr -> AST_generic.resolved_name option

(* name conversions *)

(* You should avoid this function! *)
val dotted_ident_of_name : AST_generic.name -> AST_generic.dotted_ident

(* misc *)

val id_of_name : AST_generic.name -> AST_generic.ident * AST_generic.id_info
val name_is_global : AST_generic.resolved_name_kind -> bool

val parameter_to_catch_exn_opt :
  AST_generic.parameter -> AST_generic.catch_exn option

val ctype_of_literal : AST_generic.literal -> AST_generic.const_type
val opt_to_label_ident : AST_generic.ident option -> AST_generic.label_ident

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
  Tok.t ->
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

(* Sets the e_range on the expression based on the left and right tokens
 * provided. No-op if either has a fake location. *)
val set_e_range : Tok.t -> Tok.t -> AST_generic.expr -> unit

(* Sets the e_range on the expression to the range defined by the given anys.
 * Noop if no location information for the anys is available (including if the
 * any list is empty). *)
val set_e_range_with_anys : AST_generic.any list -> AST_generic.expr -> unit
val ii_of_any : AST_generic.any -> Tok.t list
val info_of_any : AST_generic.any -> Tok.t

(* may raise NoTokenLocation *)
val first_info_of_any : AST_generic.any -> Tok.t
val range_of_tokens_unsafe : Tok.t list -> Tok_range.t
val range_of_tokens : Tok.t list -> Tok_range.t option
val range_of_any_opt : AST_generic.any -> (Tok.location * Tok.location) option

val nearest_any_of_pos :
  AST_generic.program ->
  int ->
  (AST_generic.any * (Tok.location * Tok.location)) option

val fix_token_locations_any :
  (Tok.location -> Tok.location) -> AST_generic.any -> AST_generic.any

val fix_token_locations_program :
  (Tok.location -> Tok.location) -> AST_generic.program -> AST_generic.program

val add_semicolon_to_last_var_def_and_convert_to_stmts :
  AST_generic.sc ->
  (AST_generic.entity * AST_generic.variable_definition) list ->
  AST_generic.stmt list

val add_semicolon_to_last_def_and_convert_to_stmts :
  AST_generic.sc -> AST_generic.definition list -> AST_generic.stmt list
