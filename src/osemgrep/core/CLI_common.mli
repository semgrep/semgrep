(*
   Shared utilities to help with command-line parsing and handling
   (relies on the cmdliner library)

   The o_ below stands for option (as in command-line argument option).
*)

val help_page_bottom : Cmdliner.Manpage.block list

(* small wrapper around Cmdliner.Cmd.eval_value *)
val eval_value : argv:string array -> 'a Cmdliner.Cmd.t -> 'a

(* handles logging arguments (--quiet/--verbose/--debug) *)
val o_logging : Logs.level option Cmdliner.Term.t

(* small wrapper around Logs_helper.setup_logging and Logging_helpers.setup *)
val setup_logging : force_color:bool -> level:Logs.level option -> unit

(* for --profile *)
val o_profile : bool Cmdliner.Term.t
