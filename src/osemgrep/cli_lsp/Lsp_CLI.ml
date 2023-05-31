open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep lsp' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { logging_level : Logs.level option } [@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  let combine logging_level = { logging_level } in
  Term.(const combine $ CLI_common.o_logging)

let doc = "Language server mode!!"

let man : Manpage.block list =
  [ `S Manpage.s_description; `P "Language server mode!!" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep lsp" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
