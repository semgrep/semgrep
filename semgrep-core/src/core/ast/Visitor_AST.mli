open AST_generic

(* the hooks *)
type visitor_in = {
  (* those are the one used by semgrep *)
  kexpr : (expr -> unit) * visitor_out -> expr -> unit;
  kstmt : (stmt -> unit) * visitor_out -> stmt -> unit;
  (* note that this will visit all stmt sequences, including subsequences *)
  kstmts : (stmt list -> unit) * visitor_out -> stmt list -> unit;
  ktype_ : (type_ -> unit) * visitor_out -> type_ -> unit;
  kpattern : (pattern -> unit) * visitor_out -> pattern -> unit;
  kfield : (field -> unit) * visitor_out -> field -> unit;
  (* this will not visit field subsequences like kstmts do *)
  kfields : (field list -> unit) * visitor_out -> field list -> unit;
  kattr : (attribute -> unit) * visitor_out -> attribute -> unit;
  kpartial : (partial -> unit) * visitor_out -> partial -> unit;
  kdef : (definition -> unit) * visitor_out -> definition -> unit;
  kdir : (directive -> unit) * visitor_out -> directive -> unit;
  kparam : (parameter -> unit) * visitor_out -> parameter -> unit;
  kcatch : (catch -> unit) * visitor_out -> catch -> unit;
  kident : (ident -> unit) * visitor_out -> ident -> unit;
  kname : (name -> unit) * visitor_out -> name -> unit;
  kentity : (entity -> unit) * visitor_out -> entity -> unit;
  kfunction_definition :
    (function_definition -> unit) * visitor_out -> function_definition -> unit;
  kclass_definition :
    (class_definition -> unit) * visitor_out -> class_definition -> unit;
  kinfo : (tok -> unit) * visitor_out -> tok -> unit;
  kid_info : (id_info -> unit) * visitor_out -> id_info -> unit;
  ksvalue : (svalue -> unit) * visitor_out -> svalue -> unit;
}

(* note that internally the visitor uses OCaml.v_ref_do_not_visit *)
and visitor_out = any -> unit

val default_visitor : visitor_in

val mk_visitor :
  ?vardef_assign:bool ->
  ?flddef_assign:bool ->
  ?attr_expr:bool ->
  visitor_in ->
  visitor_out
(** @param vardef_assign VarDef-Assign equivalence (default is [false])
    @param flddef_assign FieldDef-Assign equivalence (default is [false])
    @param attr_expr Attribute-expression equivalence (default is [false])
*)

(* Note that ii_of_any relies on Visitor_AST which itself
 * uses OCaml.v_ref_do_not_visit, so no need to worry about
 * tokens inside id_type or id_info.
 *)
val ii_of_any : AST_generic.any -> Parse_info.t list

(* may raise NoTokenLocation *)
val first_info_of_any : AST_generic.any -> Parse_info.t

val range_of_tokens : Parse_info.t list -> Parse_info.t * Parse_info.t

val range_of_any_opt :
  AST_generic.any ->
  (Parse_info.token_location * Parse_info.token_location) option

(* poor's man fold *)
(*
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
*)
