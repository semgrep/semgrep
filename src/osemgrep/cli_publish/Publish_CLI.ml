module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_
module Show = Show_CLI

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep publish' command-line arguments processing.

   Translated partially from publish.py
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type visibility_state = Org_private | Unlisted | Public [@@deriving show]

(*
   The result of parsing a 'semgrep publish' command.
*)
type conf = {
  (* Main configuration options *)
  common : CLI_common.conf;
  upload_target : string;
  visibility : visibility_state;
  registry_id : string option;
}
[@@deriving show]

let default : conf =
  {
    common =
      (* TODO? move to CLI_common.ml? *)
      {
        profile = false;
        logging_level = Some Logs.Warning;
        maturity = Maturity.Default;
      };
    upload_target = "<required>";
    visibility = Org_private;
    registry_id = None;
  }

(*************************************************************************)
(* Helpers *)
(*************************************************************************)
let visibility_state_converter : visibility_state Arg.conv =
  let parser s =
    let fail =
      `Error
        (Common.spf "expected 'org_private', 'unlisted', or 'public' (got %s)" s)
    in
    let s = String.lowercase_ascii s in
    match s with
    | "org_private" -> `Ok Org_private
    | "unlisted" -> `Ok Unlisted
    | "public" -> `Ok Public
    | _ -> fail
  in
  let printer ppf x = pp_visibility_state ppf x in
  (parser, printer)

let string_of_visibility = function
  | Org_private -> "org_private"
  | Unlisted -> "unlisted"
  | Public -> "public"

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)
(* The o_ below stands for option (as in command-line argument option) *)

let o_visibility : visibility_state Term.t =
  let default = default.visibility in
  let info =
    Arg.info [ "visibility" ]
      ~doc:
        {|Sets visibility of the uploaded rules.
If 'org_private', rules will be private to your org (default)
If 'unlisted', rules will be listed in your org, but not listed to non-org members
If 'public', rules will be published to the Semgrep Registry (requires --registry-id)
|}
  in
  Arg.value (Arg.opt visibility_state_converter default info)

let o_registry_id : string option Term.t =
  let info =
    Arg.info [ "registry_id" ]
      ~doc:
        "If --visibility is set to public, this is the path the rule will have \
         in the registry (example: python.flask.my-new-rule)"
  in
  Arg.value (Arg.opt Arg.(some string) None info)

let upload_target =
  let info = Arg.info [] ~doc:"Target rule(s) to upload" in
  Arg.required (Arg.pos 0 Arg.(some string) None info)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine common registry_id upload_target visibility =
    { common; upload_target; visibility; registry_id }
  in
  (* Term defines 'const' but also the '$' operator *)
  Term.(
    (* !the o_xxx must be in alphabetic orders to match the parameters of
     * combine above! *)
    const combine $ CLI_common.o_common $ o_registry_id $ upload_target
    $ o_visibility)

let doc = "upload rule to semgrep.dev"

(* TODO: document the exit codes as defined in Exit_code.mli *)
let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Must be logged in to use; see `semgrep login`";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep publish" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
