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
  (* LATER: nosem *)
  core_runner_conf : Core_runner.conf;
  logging_level : Logs.level option;
  profile : bool;
}
[@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let cmdline_term : conf Term.t =
  (* those parameters must be in alphabetic order, like in the 'const combine'
   * further below, so it's easy to add new options.
   *)
  let combine ast_caching exclude include_ lang logging_level profile
      target_roots =
    let lang =
      match lang with
      (* TODO? we could omit the language like for -e and try all languages?*)
      | None -> Error.abort "-l is required for the interactive subcommand"
      | Some s ->
          (* may raise unsupported language exn *)
          Lang.of_string s
    in
    let target_roots = File.Path.of_strings target_roots in
    (* like in Scan_CLI.combine *)
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
      core_runner_conf = { Scan_CLI.default.core_runner_conf with ast_caching };
      logging_level;
      profile;
    }
  in
  Term.(
    const combine $ Scan_CLI.o_ast_caching $ Scan_CLI.o_exclude
    $ Scan_CLI.o_include $ Scan_CLI.o_lang $ CLI_common.o_logging
    $ CLI_common.o_profile $ Scan_CLI.o_target_roots)

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
