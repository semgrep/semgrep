val help_page_bottom : Cmdliner.Manpage.block list

(* small wrapper around Cmdliner.Cmd.eval_value *)
val eval_value :
  argv:string array -> 'a Cmdliner.Cmd.t -> ('a, Exit_code.t) result
