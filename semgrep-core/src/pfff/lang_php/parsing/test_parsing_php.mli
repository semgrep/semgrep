(*s: test_parsing_php.mli *)

(* This runs the PHP parser on a set of files or directories and displays
 * any parsing errors. It also stores the current parsing result
 * on those files or directories in pfff/tmp/<filename_based_on_concatenation_
 * of_files_and_dirs> and compares it with any previous run, providing some
 * form of regression testing.
*)
val test_parse_php  : Common.path list -> unit
(*x: test_parsing_php.mli *)
(* Print the set of tokens in a PHP file *)
val test_tokens_php : Common.filename -> unit

(* Print the AST of a PHP file using a JSON format *)
(* val test_json_php   : Common.filename -> unit *)
(*
(* Print any scalar in a PHP file by internally using a visitor *)
val test_visit_php  : Common.filename -> unit
*)
(*x: test_parsing_php.mli *)

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_php foo.php' will call the
 * test_parse_php function.
*)
val actions : unit -> Common.cmdline_actions
(*e: test_parsing_php.mli *)
