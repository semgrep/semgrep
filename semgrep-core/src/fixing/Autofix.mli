val render_fix :
  Lang.t ->
  Metavariable.bindings ->
  fix_pattern:string ->
  target_contents:string Lazy.t ->
  string option
