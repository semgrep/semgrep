(* Print the given AST to a string, using the original text of nodes from the
 * fix pattern or target file wherever possible *)
val print_ast :
  lang:Lang.t ->
  (* Needed so that we can identify nodes that came from the original target
   * source via metavariable bindings, so that we can use their original text
   * instead of printing from scratch. *)
  metavars:Metavariable.bindings ->
  (* Needed so that when printing, we can lift the original text for AST nodes
   * that came from the target source via metavariable bindings. Lazy because in
   * practice, computing this requires a disk read, and it may not be needed. *)
  target_contents:string Lazy.t ->
  (* The original AST of the fix pattern written in the rule. Needed so that we
   * can identify nodes that came from the fix pattern, so that we can use their
   * original text instead of printing from scratch. *)
  fix_pattern_ast:AST_generic.any ->
  (* The original text of the fix pattern as written in the rule. Needed so that
   * when we identify that a node has come from the fix pattern, we can use the
   * original text from here instead of printing from scratch. *)
  fix_pattern:string ->
  (* The AST to print *)
  AST_generic.any ->
  (string, string) result
