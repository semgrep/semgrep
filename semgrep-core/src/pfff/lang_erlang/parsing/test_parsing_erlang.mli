

(* Print the set of tokens in a .py file *)
val test_tokens_erlang : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_erlang foo.py will call the
 * test_parse_erlang function.
*)
val actions : unit -> Common.cmdline_actions
