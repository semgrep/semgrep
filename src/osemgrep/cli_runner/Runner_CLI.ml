open Cmdliner
module H = Cmdliner_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep runner' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type conf = {
  common : CLI_common.conf;
  client : bool;
} [@@deriving show]

let default : conf =
  {
    common = {
        profile = false;
        logging_level = Some Logs.Warning;
        (* or set to Experimental by default when we release osemgrep? *)
        maturity = None;
      };
    client = false;
  }
(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let o_client : bool Term.t =
  H.negatable_flag [ "client" ] ~neg_options:[ "server" ]
    ~default:default.client
    ~doc:
      {|Run the runner client. --server will disable this flag.|}

let cmdline_term : conf Term.t =
  let combine client_ common =
      {
        client = client_;
        common;
      } in
  Term.(const combine $ o_client $ CLI_common.o_common)

let doc = "Runner server mode!!"

let man : Manpage.block list =
  [ `S Manpage.s_description; `P "Runner server mode!!" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep runner" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
