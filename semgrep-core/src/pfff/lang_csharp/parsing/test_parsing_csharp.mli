

(* Print the set of tokens in a .py file *)
val test_tokens_csharp : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_csharp foo.py will call the
 * test_parse_csharp function.
*)
val actions : unit -> Common.cmdline_actions
