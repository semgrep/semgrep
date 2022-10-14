val help_page_bottom : Cmdliner.Manpage.block list

(* Wrapper that catches exceptions and turns them into an exit code. *)
val safe_run : ('a -> Exit_code.t) -> 'a -> Exit_code.t
