(*
   Map tree-sitter-bash CST to an AST.
*)

type env = AST_bash.input_kind Parse_tree_sitter_helpers.env

(*
   This is for languages that embed bash scripts such as dockerfiles.
   The token 'tok' represents the first location of the shell script
   and is needed in case the script is empty.
*)
val program :
  env -> tok:Parse_info.t -> Tree_sitter_bash.CST.program -> AST_bash.blist

val parse :
  Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t

val parse_pattern : string -> AST_generic.any Tree_sitter_run.Parsing_result.t
