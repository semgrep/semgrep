module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd
module H = Cmdliner_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep validate' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* The result of parsing a 'semgrep validate' command. This is also used in
 * Scan_CLI.ml to transform legacy commands such as 'semgrep --validate <dir>'
 * into the new 'semgrep validate <dir>'
 *)
type conf = {
  (* Right now we can use either scan --validate --config to get the list of
   * rules for validate, or positional args to pass after 'semgrep validate'
   * (e.g., semgrep validate foo.yaml, or semgrep validate semgrep-rules/ocaml/)
   * TODO? useful to allow '--validate --config p/python'? or to allow
   * also to validate patterns as in '--validate -e foo() -l python'?
   * Should we change this rules_source to just Dir or File like in Test_CLI.ml?
   *)
  rules_source : Rules_source.t;
  pro : bool;
  (* TODO? really needed? *)
  core_runner_conf : Core_runner.conf;
  common : CLI_common.conf;
}
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)
(* coupling: similar to Scan_CLI.o_pro but currently has a different meaning
 * alt: move those options to CLI_common.ml at some point
 *)
let o_pro : bool Term.t =
  let info =
    Arg.info [ "pro" ]
      ~doc:
        (" support pro languages (currently Apex and Elixir)"
       ^ CLI_common.blurb_pro)
  in
  Arg.value (Arg.flag info)

(* ------------------------------------------------------------------ *)
(* Positional arguments *)
(* ------------------------------------------------------------------ *)

(* TODO: we accept just one elt here, so why not use just Arg.pos? *)
let o_args : string list Term.t =
  let info =
    Arg.info [] ~docv:"STRINGS" ~doc:{|Directory or file containing rules.|}
  in
  Arg.value (Arg.pos_all Arg.string [] info)

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  (* !The parameters must be in alphabetic orders to match the order
   * of the corresponding '$ o_xx $' further below! *)
  let combine args common pro =
    let rules_source =
      match args with
      | [] -> Error.abort "Nothing to validate, pass a directory or rule file"
      | xs -> Rules_source.Configs xs
    in
    let core_runner_conf = Core_runner.default_conf in
    { rules_source; pro; core_runner_conf; common }
  in
  Term.(const combine $ o_args $ CLI_common.o_common $ o_pro)

let doc =
  "validating the rules (EXPERIMENTAL improvements over scan --validate)"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "See \
       https://semgrep.dev/docs/writing-rules/testing-rules#validating-rules";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep validate" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
