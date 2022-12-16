
(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_web foo.html will call the
 * test_parse_web function.
*)
val actions : unit -> Common.cmdline_actions
