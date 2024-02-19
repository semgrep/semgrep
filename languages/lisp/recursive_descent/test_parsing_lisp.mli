(* Print the set of tokens in a .lisp file *)
val test_tokens_lisp : Fpath.t -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_lisp foo.lisp will call the
 * test_parse_lisp function.
 *)
val actions : unit -> Arg_.cmdline_actions
