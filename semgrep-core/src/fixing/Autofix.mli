(* Attempts to render a fix. If successful, returns the text that should replace
 * the matched range in the target file. If unsuccessful, returns None. *)
val render_fix :
  Lang.t ->
  (* Needed so that we can replace metavariables in the fix pattern with their
   * corresponding AST nodes from the target file *)
  Metavariable.bindings ->
  (* The fix pattern itself, as written in the rule *)
  fix_pattern:string ->
  (* The contents of the target file. This is needed so that we can use the
   * original text from the target file when printing metavariables.
   *
   * Lazy because in practice, computing this involves a read from disk which
   * can be expensive, and it may not be needed if there is a failure before
   * printing or if no metavariables appear in the fix pattern. *)
  target_contents:string Lazy.t ->
  string option
