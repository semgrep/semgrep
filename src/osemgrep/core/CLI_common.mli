(*
   Shared utilities to help with command-line parsing and handling
   (relies on the cmdliner library)
*)

val help_page_bottom : Cmdliner.Manpage.block list

(* small wrapper around Cmdliner.Cmd.eval_value *)
val eval_value : argv:string array -> 'a Cmdliner.Cmd.t -> 'a

(* handles logging arguments (--quiet/--verbose/--debug) *)
val logging_term : Logs.level option Cmdliner.Term.t

(* TODO: parser+printer for file path so we can write things like:

        Arg.value (Arg.opt (Arg.some CLI_common.fpath) None info)

      instead of

        Arg.value (Arg.opt (Arg.some Arg.string) None info)
        (* + having to convert the string to an fpath by hand *)

      The main benefit would be to clarify error messages by having Fpath.t
      instead of string.

   val fpath : Fpath.t Cmdliner.conv????
*)
