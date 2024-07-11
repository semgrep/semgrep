module Out = Semgrep_output_v1_t
module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term
module H = Cmdliner_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep install-semgrep-pro' command-line parsing.

   Translated from install.py
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = { common : CLI_common.conf; custom_binary : string option }
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

let o_custom_binary : string option Term.t =
  let info =
    Arg.info [ "custom-binary" ]
      ~doc:
        {|Supply a binary to use as semgrep-core-proprietary, rather than downloading it. You are responsible for ensuring compatibility.|}
  in
  Arg.value (Arg.opt Arg.(some string) None info)

(*************************************************************************)
(* Turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  let combine common custom_binary = { common; custom_binary } in
  Term.(const combine $ CLI_common.o_common $ o_custom_binary)

let doc = "Install the Semgrep Pro Engine"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "The binary is installed in the same directory that semgrep-core is \
       installed in.";
    `P
      {|Must be logged in and have access to Semgrep Pro Engine
Visit https://semgrep.dev/products/pro-engine/ for more information
|};
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep install-semgrep-pro" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  (* mostly a copy of Scan_CLI.parse_argv with different doc and man *)
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
