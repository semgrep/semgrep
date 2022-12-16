
(* Print the set of tokens in a .lisp file *)
val test_tokens_lisp : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_lisp foo.lisp will call the
 * test_parse_lisp function.
*)
val actions : unit -> Common.cmdline_actions
