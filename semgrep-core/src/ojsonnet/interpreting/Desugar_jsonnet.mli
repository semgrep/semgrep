(* We pass the original file in addition to its AST so desugar can
 * handle correctly imports and import from the dirname of the file.
 * TODO: import_callback so can customize
 * things as the desugar function will "desugar" imports.
 *)
val desugar_program :
  Common.filename -> AST_jsonnet.program -> Core_jsonnet.program
