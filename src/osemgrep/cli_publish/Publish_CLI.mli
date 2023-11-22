(*
   'semgrep publish' command-line parsing.
*)

type visibility_state = Org_private | Unlisted | Public [@@deriving show]

val string_of_visibility : visibility_state -> string

(*
   The result of parsing a 'semgrep publish' command.
*)
type conf = {
  (* Main configuration options *)
  common : CLI_common.conf;
  (* May be the path of a rule, or a folder which contains rules. *)
  upload_target : string;
  visibility : visibility_state;
  registry_id : string option;
}
[@@deriving show]

(*
   Usage: parse_argv [| "semgrep-publish"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf
