
(* This runs the JS parser on a set of files or directories and displays
 * any parsing errors. It also stores the current parsing result
 * on those files or directories in pfff/tmp/<filename_based_on_concatenation_
 * of_files_and_dirs> and compares it with any previous run, providing some
 * form of regression testing.
*)
val test_parse_js  : Common.path list -> unit

(* Print the set of tokens in a JS file *)
val test_tokens_js : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_js foo.js will call the
 * test_parse_js function.
*)
val actions : unit -> Common.cmdline_actions
