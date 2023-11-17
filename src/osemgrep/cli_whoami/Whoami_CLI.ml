module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep whoami' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { name_only : bool }

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)

let o_name_only : bool Term.t =
  let info =
    Arg.info [ "name-only" ] ~doc:{|Output only name of logged-in user.|}
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)
let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine name_only = { name_only } in

  Term.(const combine $ o_name_only)

let doc = "Show current identity of logged in user"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Show current identity of logged in user";
    `Pre "semgrep whoami";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep whoami" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
