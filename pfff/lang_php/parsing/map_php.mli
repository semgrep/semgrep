(*s: map_php.mli *)
open Cst_php
(* hooks *)
type visitor_in = {
  kexpr: (expr  -> expr) * visitor_out -> expr  -> expr;
  kstmt_and_def: 
   (stmt_and_def -> stmt_and_def) * visitor_out -> stmt_and_def ->stmt_and_def;
  kstmt: (stmt -> stmt) * visitor_out -> stmt -> stmt;
  kname: (name -> name) * visitor_out -> name -> name;
  kclass_def:  (class_def -> class_def) * visitor_out -> class_def -> class_def;
  kinfo: (tok -> tok) * visitor_out -> tok -> tok;
                                                                            
}
and visitor_out = {
  vtop: toplevel -> toplevel;
  vstmt_and_def: stmt_and_def -> stmt_and_def;
  vprogram: program -> program;
  vexpr: expr -> expr;
  vxhpattrvalue: xhp_attr_value -> xhp_attr_value;
  vany: any -> any;
}

val default_visitor: visitor_in
val mk_visitor: visitor_in -> visitor_out

(*e: map_php.mli *)
