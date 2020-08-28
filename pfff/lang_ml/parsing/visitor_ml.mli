open Cst_ml

type visitor_in = {
  kitem: item vin;
  ktoplevel: toplevel vin;
  kexpr: expr vin;
  kpattern: pattern vin;
  kty: ty vin;
  kfield_decl: field_declaration vin;
  kfield_expr: field_and_expr vin;
  kfield_pat: field_pattern vin;
  ktype_declaration: type_declaration vin;
  klet_def: let_def vin;
  klet_binding: let_binding vin;
  kqualifier: qualifier vin;
  kmodule_expr: module_expr vin;
  kparameter: parameter vin;
  kargument: argument vin;
  kinfo: tok vin;
}
and visitor_out = any -> unit
  and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

val default_visitor: visitor_in

val mk_visitor: visitor_in -> visitor_out
