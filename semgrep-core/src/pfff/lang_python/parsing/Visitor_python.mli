open AST_python

(* hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktype_: (type_  -> unit) * visitor_out -> type_  -> unit;
  kdecorator: (decorator  -> unit) * visitor_out -> decorator  -> unit;
  kparameter: (parameter  -> unit) * visitor_out -> parameter  -> unit;
  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
and visitor_out = any -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
