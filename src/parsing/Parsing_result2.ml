(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Mostly 'type t = (AST_generic, Parse_info.token_location) Parsing_result.t',
 * but the tokens part is actually used to represent skipped_tokens
 * instead of all the tokens (as intented in Parsing_result.t), hence
 * the new type.
 * TODO? merge with Parsing_result.t? add a skipped_tokens there too?
 *)
type t = {
  ast : AST_generic.program;
  (* partial errors tree-sitter was able to recover from *)
  skipped_tokens : Tok.location list;
  stat : Parsing_stat.t;
}
