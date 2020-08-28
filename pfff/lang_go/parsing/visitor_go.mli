open Ast_go

(* hooks *)
type visitor_in = {
  kident:   (ident       -> unit) * visitor_out -> ident       -> unit;
  kexpr:    (expr        -> unit) * visitor_out -> expr        -> unit;
  kstmt:    (stmt        -> unit) * visitor_out -> stmt        -> unit;
  ktype:    (type_         -> unit) * visitor_out -> type_         -> unit;
  kdecl:    (decl        -> unit) * visitor_out -> decl        -> unit;
  ktop_decl:    (top_decl        -> unit) * visitor_out -> top_decl   -> unit;
  kfunction: (function_ -> unit) * visitor_out -> function_ -> unit;
  kparameter: (parameter -> unit) * visitor_out -> parameter -> unit;
  kinit: (init -> unit) * visitor_out -> init -> unit;
  kprogram: (program     -> unit) * visitor_out -> program     -> unit;

  kinfo: (tok -> unit) * visitor_out -> tok -> unit;
}
and visitor_out = any -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
