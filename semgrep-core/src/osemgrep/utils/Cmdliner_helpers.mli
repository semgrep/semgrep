(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
*)
val negatable_flag :
  ?default:bool ->
  ?env:Cmdliner.Cmd.Env.info ->
  neg_options:string list ->
  doc:string ->
  string list ->
  bool Cmdliner.Term.t
