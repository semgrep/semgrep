open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep interactive' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* a subset of Scan_CLI.conf *)
type conf = {
  lang : Lang.t; (* use Xlang.t at some point? or even Xlang option? *)
  target_roots : Fpath.t list;
  targeting_conf : Find_targets.conf;
  core_runner_conf : Core_runner.conf;
  (* nosem? *)
  logging_level : Logs.level option;
}
[@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  let combine exclude include_ lang target_roots logging_level =
    let lang =
      match lang with
      (* TODO? we could omit the language like for -e and try all languages?*)
      | None -> Error.abort "-l is required for the interactive subcommand"
      | Some s ->
          (* may raise unsupported language exn *)
          Lang.of_string s
    in
    let target_roots = File.Path.of_strings target_roots in
    let include_ =
      match include_ with
      | [] -> None
      | nonempty -> Some nonempty
    in
    {
      lang;
      target_roots;
      (* LATER: accept all CLI args *)
      targeting_conf =
        { Scan_CLI.default.targeting_conf with include_; exclude };
      core_runner_conf = Scan_CLI.default.core_runner_conf;
      logging_level;
    }
  in
  Term.(
    const combine $ Scan_CLI.o_exclude $ Scan_CLI.o_include $ Scan_CLI.o_lang
    $ Scan_CLI.o_target_roots $ CLI_common.logging_term)

let doc = "Interactive mode!!"

let man : Manpage.block list =
  [ `S Manpage.s_description; `P "Interactive mode!!" ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep interactive" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
