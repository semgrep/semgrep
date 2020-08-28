
val interfaces:
  Cst_php.class_def -> Cst_php.class_name list
val traits:
  Cst_php.class_def -> Cst_php.class_name list

(* __construct *)
val constructor_name: string 
val get_constructor: Cst_php.class_def -> Cst_php.method_def

val get_public_or_protected_vars_of_class:
  Cst_php.class_def -> Cst_php.dname list

val is_static_method:
  Cst_php.method_def -> bool
val has_visiblity_modifier:
  Cst_php.modifier Cst_php.wrap list -> bool
val is_interface:
  Cst_php.class_def -> bool

exception Use__Call
exception UndefinedClassWhileLookup of string

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found, or Use__Call *)
val lookup_method: 
  ?case_insensitive: bool ->
  (string (* class *) * string (* method *)) ->
  Entity_php.entity_finder -> Cst_php.method_def

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found *)
val lookup_member: 
  ?case_insensitive: bool ->
  (string (* class *) * string (* field *)) ->
  Entity_php.entity_finder ->
  Cst_php.class_variable * Cst_php.class_var_modifier

(* can raise UndefinedClassWhileLookup, Not_found, Multi_found *)
val lookup_constant:
  (string (* class *) * string (* constant *)) ->
  Entity_php.entity_finder ->
  Cst_php.class_constant

(* does not raise exception *)
val collect_members: 
  string (* class *) -> Entity_php.entity_finder -> Cst_php.dname list



val class_variables_reorder_first:
  Cst_php.class_def -> Cst_php.class_def

val class_kind_of_ctype:
  Cst_php.class_type -> Entity_code.class_kind
val string_of_class_type:
  Cst_php.class_type -> string

