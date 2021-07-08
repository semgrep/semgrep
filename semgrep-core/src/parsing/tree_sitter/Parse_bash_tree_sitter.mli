(*
   Map tree-sitter-bash CST to an AST.
*)

val parse : Common.filename -> AST_bash.program Tree_sitter_run.Parsing_result.t
