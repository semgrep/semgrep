
(* Print the set of tokens in a .ml file *)
val test_tokens_ml : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_ml foo.ml will call the
 * test_parse_ml function.
*)
val actions : unit -> Common.cmdline_actions
