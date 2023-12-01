(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_js foo.js will call the
 * test_parse_js function.
 *)
val actions : unit -> Arg_.cmdline_actions
