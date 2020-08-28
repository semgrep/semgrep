(*s: visitor_php.mli *)
open Cst_php

(*s: type visitor_in *)
(* the hooks *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktop: (toplevel -> unit) * visitor_out -> toplevel  -> unit;
  kconstant: (constant -> unit) * visitor_out -> constant  -> unit;
  kscalar: (scalar -> unit) * visitor_out -> scalar  -> unit;
  kencaps: (encaps -> unit) * visitor_out -> encaps -> unit;
  kclass_stmt: (class_stmt -> unit) * visitor_out -> class_stmt -> unit;
  kparameter: (parameter -> unit) * visitor_out -> parameter -> unit;
  kargument: (argument -> unit) * visitor_out -> argument -> unit;
  kcatch: (catch -> unit) * visitor_out -> catch -> unit;
  kfinally: (finally -> unit) * visitor_out -> finally -> unit;

  (* xhp: *)
  kxhp_html: (xhp_html -> unit) * visitor_out -> xhp_html -> unit;
  kxhp_tag: (xhp_tag wrap -> unit) * visitor_out -> xhp_tag wrap -> unit;
  kxhp_attribute: 
    (xhp_attribute -> unit) * visitor_out -> xhp_attribute -> unit;

  kxhp_attr_decl:
    (xhp_attribute_decl -> unit) * visitor_out -> xhp_attribute_decl -> unit;
  kxhp_children_decl:
    (xhp_children_decl -> unit) * visitor_out -> xhp_children_decl -> unit;

  (* kfunc_def helps abstracting away whether a function/class... is defined
   * in a nested way or at the toplevel (e.g. FuncDefNested vs FuncDef).
   * Note that kfunc_def is also run for methods now. Look in
   * def.f_type to decide what to do if you want to filter methods.
   * !note! short lambdas are currently not in func_def, so take care
   * to visit also this case in kexpr.
   *)
  kfunc_def:  (func_def -> unit) * visitor_out -> func_def -> unit;
  kclass_def:  (class_def -> unit) * visitor_out -> class_def -> unit;

  kmethod_def: (method_def -> unit) * visitor_out -> method_def -> unit;

  (* Helps intercepting all the new blocks that in a real language should
   * defined a new scope
   *)
  kstmt_and_def_list_scope: 
    (stmt_and_def list -> unit) * visitor_out -> stmt_and_def list  -> unit;

  kname: (name -> unit) * visitor_out -> name -> unit;
  khint_type: (hint_type -> unit) * visitor_out -> hint_type -> unit;
  ktparam: (type_param -> unit) * visitor_out -> type_param -> unit;
  karray_pair: (array_pair -> unit) * visitor_out -> array_pair -> unit;

  karguments: (argument comma_list paren -> unit) * visitor_out ->
    argument comma_list paren -> unit;

  kcomma: (tok -> unit) * visitor_out -> tok -> unit; 
  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
(*e: type visitor_in *)
(*s: type visitor_out *)
and visitor_out = any -> unit
(*e: type visitor_out *)

(*s: visitor functions *)
val default_visitor : visitor_in
(*x: visitor functions *)
val mk_visitor: visitor_in -> visitor_out
(*x: visitor functions *)
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
(*e: visitor functions *)
(*e: visitor_php.mli *)
