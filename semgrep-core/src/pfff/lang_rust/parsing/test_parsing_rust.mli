

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_rust foo.py will call the
 * test_parse_rust function.
*)
val actions : unit -> Common.cmdline_actions
