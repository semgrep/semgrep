(* TODO: probably need pass pwd, current file, import_callback so can customize
 * things as the desugar function will "desugar" imports.
 *)
val desugar_program : AST_jsonnet.program -> Core_jsonnet.program
