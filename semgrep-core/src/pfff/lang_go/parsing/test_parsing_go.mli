(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_go foo.go will call the
 * test_parse_go function.
*)
val actions : unit -> Common.cmdline_actions
