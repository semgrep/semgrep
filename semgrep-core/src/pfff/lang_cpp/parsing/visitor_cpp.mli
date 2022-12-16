
open Ast_cpp

(* the hooks *)
type visitor_in = {
  kexpr: expr vin;
  kstmt: stmt vin;
  kinit: initialiser vin;
  ktypeC: typeC vin;

  kclass_member: class_member vin;

  kparameter: parameter vin;
  kcompound: compound vin;

  kclass_def: class_definition vin;
  kfunc_def: func_definition vin;
  kcpp: cpp_directive vin;

  kdeclaration: decl vin;
  ktoplevel: toplevel vin;

  kinfo: tok vin;
}
and visitor_out = any -> unit
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor : visitor_in

val mk_visitor: visitor_in -> visitor_out
