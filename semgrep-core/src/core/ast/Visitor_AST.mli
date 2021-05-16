(*s: pfff/lang_GENERIC/parsing/Visitor_AST.mli *)
open AST_generic

(* the hooks *)
(*s: type [[Visitor_AST.visitor_in]] *)
type visitor_in = {
  (* those are the one used by semgrep *)
  kexpr : (expr -> unit) * visitor_out -> expr -> unit;
  kstmt : (stmt -> unit) * visitor_out -> stmt -> unit;
  kstmts : (stmt list -> unit) * visitor_out -> stmt list -> unit;
  ktype_ : (type_ -> unit) * visitor_out -> type_ -> unit;
  kpattern : (pattern -> unit) * visitor_out -> pattern -> unit;
  kfield : (field -> unit) * visitor_out -> field -> unit;
  kattr : (attribute -> unit) * visitor_out -> attribute -> unit;
  kpartial : (partial -> unit) * visitor_out -> partial -> unit;
  kdef : (definition -> unit) * visitor_out -> definition -> unit;
  kdir : (directive -> unit) * visitor_out -> directive -> unit;
  kparam : (parameter -> unit) * visitor_out -> parameter -> unit;
  kident : (ident -> unit) * visitor_out -> ident -> unit;
  kname : (name -> unit) * visitor_out -> name -> unit;
  kentity : (entity -> unit) * visitor_out -> entity -> unit;
  kfunction_definition :
    (function_definition -> unit) * visitor_out -> function_definition -> unit;
  kclass_definition :
    (class_definition -> unit) * visitor_out -> class_definition -> unit;
  kinfo : (tok -> unit) * visitor_out -> tok -> unit;
  kid_info : (id_info -> unit) * visitor_out -> id_info -> unit;
  kconstness : (constness -> unit) * visitor_out -> constness -> unit;
}

(*e: type [[Visitor_AST.visitor_in]] *)
(* note that internally the visitor uses OCaml.v_ref_do_not_visit *)
(*s: type [[Visitor_AST.visitor_out]] *)
and visitor_out = any -> unit

(*e: type [[Visitor_AST.visitor_out]] *)

(*s: signature [[Visitor_AST.default_visitor]] *)
val default_visitor : visitor_in

(*e: signature [[Visitor_AST.default_visitor]] *)

(*s: signature [[Visitor_AST.mk_visitor]] *)
val mk_visitor : visitor_in -> visitor_out

(*e: signature [[Visitor_AST.mk_visitor]] *)

(* poor's man fold *)
(*
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
*)

(* Note that ii_of_any relies on Visitor_AST which itself
 * uses OCaml.v_ref_do_not_visit, so no need to worry about
 * tokens inside id_type or id_info.
 *)
val ii_of_any : AST_generic.any -> Parse_info.t list

val range_of_tokens : Parse_info.t list -> Parse_info.t * Parse_info.t

val range_of_any :
  AST_generic.any -> Parse_info.token_location * Parse_info.token_location

(*e: pfff/lang_GENERIC/parsing/Visitor_AST.mli *)
