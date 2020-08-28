(*s: pfff/lang_python/parsing/Visitor_python.mli *)
open AST_python

(* hooks *)
(*s: type [[Visitor_python.visitor_in]] *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktype_: (type_  -> unit) * visitor_out -> type_  -> unit;
  kdecorator: (decorator  -> unit) * visitor_out -> decorator  -> unit;
  kparameter: (parameter  -> unit) * visitor_out -> parameter  -> unit;
  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
(*e: type [[Visitor_python.visitor_in]] *)
(*s: type [[Visitor_python.visitor_out]] *)
and visitor_out = any -> unit
(*e: type [[Visitor_python.visitor_out]] *)

(*s: signature [[Visitor_python.default_visitor]] *)
val default_visitor: visitor_in
(*e: signature [[Visitor_python.default_visitor]] *)

(*s: signature [[Visitor_python.mk_visitor]] *)
val mk_visitor: visitor_in -> visitor_out
(*e: signature [[Visitor_python.mk_visitor]] *)
(*e: pfff/lang_python/parsing/Visitor_python.mli *)
