
(* Print the set of tokens in a .lisp file *)
val test_tokens_hs : Common.filename -> unit

(* This makes accessible the different test_xxx functions above from
 * the command line, e.g. '$ pfff -parse_hs foo.lisp will call the
 * test_parse_hs function.
*)
val actions : unit -> Common.cmdline_actions
