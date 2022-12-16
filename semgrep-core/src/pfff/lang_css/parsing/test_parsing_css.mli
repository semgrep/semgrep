
(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_css foo.css will call the
 * test_parse_css function.
*)
val actions : unit -> Common.cmdline_actions
