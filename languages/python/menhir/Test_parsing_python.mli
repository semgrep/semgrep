(* Print the set of tokens in a .py file *)
val test_tokens_python : string (* filename *) -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_python foo.py will call the
 * test_parse_python function.
 *)
val actions : unit -> Arg_helpers.cmdline_actions
