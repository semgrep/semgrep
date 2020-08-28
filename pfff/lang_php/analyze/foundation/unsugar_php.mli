
(* post: the Self and Parent constructors are not anymore possible cases *)
val unsugar_self_parent_any: Cst_php.any -> Cst_php.any

(* special cases *)
val unsugar_self_parent_program: Cst_php.program -> Cst_php.program
