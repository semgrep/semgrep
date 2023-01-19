(* a parser can also "return" an exception like Lexical_error,
 * or Parsing_error (unless Flag_parsing.error_recovery is true).
 *)
type ('ast, 'toks) t = {
  ast : 'ast;
  (* Note that the token list contains usually also the comment-tokens *)
  tokens : 'toks list;
  stat : Parsing_stat.t;
}
