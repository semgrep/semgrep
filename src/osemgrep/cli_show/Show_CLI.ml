module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep show' command-line arguments processing.
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type debug_settings = { output_dir : Fpath.t option; root : Fpath.t }
[@@deriving show]

(*
   The result of parsing a 'semgrep show' command.
   This is also used in Scan_CLI.ml to transform legacy
   commands such as 'semgrep scan --show-supported-languages' into the
   new 'semgrep show supported-languages'
*)
type conf = {
  common : CLI_common.conf;
  (* mix of --dump-ast/--dump-rule/... *)
  show_kind : show_kind;
  json : bool;
}

(* coupling: if you add a command you probably need to modify [combine]
 * below and also the doc in [man] further below
 *)
and show_kind =
  | Version
  (* 'semgrep show supported-languages'
   * accessible also as `semgrep scan --show-supported-languages
   *)
  | SupportedLanguages
  (* a.k.a whoami *)
  | Identity
  | Deployment
  (* 'semgrep show dump-pattern'
   * accessible also as 'semgrep scan --dump-ast -e <pattern>'
   * alt: we could accept XLang.t to dump extended patterns *)
  | DumpPattern of string * Lang.t
  (* 'semgrep show dump-ast
   * accessible also as 'semgrep scan --lang <lang> --dump-ast <target>
   * alt: we could accept multiple Files via multiple target_roots *)
  | DumpCST of Fpath.t * Lang.t
  | DumpAST of Fpath.t * Lang.t
  | DumpConfig of Rules_config.config_string
  | DumpRuleV2 of Fpath.t
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-engine-path
   * LATER: get rid of it? *)
  | DumpEnginePath of bool (* pro = true *)
  (* 'semgrep show ???'
   * accessible also as 'semgrep scan --dump-command-for-core' (or just '-d')
   * LATER: get rid of it *)
  | DumpCommandForCore
  | Debug of debug_settings
[@@deriving show]

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* Flags *)
(* ------------------------------------------------------------------ *)

let o_json : bool Term.t =
  let info = Arg.info [ "json" ] ~doc:{|Output results in JSON format.|} in
  Arg.value (Arg.flag info)

(*************************************************************************)
(* Subcommands *)
(*************************************************************************)

let version_cmd =
  let doc = "Print the Semgrep version" in
  let info = Cmd.info "version" ~doc in
  let term =
    Term.(
      const (fun common json -> { common; json; show_kind = Version })
      $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let supported_languages_cmd =
  let doc =
    "Print a list of languages that are currently supported by Semgrep"
  in
  let info = Cmd.info "supported-languages" ~doc in
  let term =
    Term.(
      const (fun common json ->
          { common; json; show_kind = SupportedLanguages })
      $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let identity_cmd =
  let doc = "Print the current logged-in token identity" in
  let info = Cmd.info "identity" ~doc in
  let term =
    Term.(
      const (fun common json -> { common; json; show_kind = Identity })
      $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let deployment_cmd =
  let doc = "Print the current logged-in deployment" in
  let info = Cmd.info "deployment" ~doc in
  let term =
    Term.(
      const (fun common json -> { common; json; show_kind = Deployment })
      $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let dump_pattern_cmd =
  let doc = "Dump the abstract syntax tree of the pattern string" in
  let lang_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"LANG")
  in
  let pattern_arg =
    Arg.(required & pos 1 (some string) None & info [] ~docv:"PATTERN")
  in
  let info = Cmd.info "dump-pattern" ~doc in
  let term =
    Term.(
      const (fun lang pattern common json ->
          {
            common;
            json;
            show_kind = DumpPattern (pattern, Lang.of_string lang);
          })
      $ lang_arg $ pattern_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let dump_cst_cmd =
  let doc = "Dump the concrete syntax tree of the file (tree sitter only)" in
  let lang_arg =
    Arg.(value & pos ~rev:true 1 (some string) None & info [] ~docv:"LANG")
  in
  let file_arg =
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"FILE")
  in
  let info = Cmd.info "dump-cst" ~doc in
  let term =
    Term.(
      const (fun lang_opt file common json ->
          let path = Fpath.v file in
          let lang =
            match lang_opt with
            | Some lang_str -> Lang.of_string lang_str
            | None -> Lang.lang_of_filename_exn path
          in
          { common; json; show_kind = DumpCST (path, lang) })
      $ lang_arg $ file_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let dump_ast_cmd =
  let doc = "Dump the abstract syntax tree of the file" in
  let lang_arg =
    Arg.(value & pos ~rev:true 1 (some string) None & info [] ~docv:"LANG")
  in
  let file_arg =
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"FILE")
  in
  let info = Cmd.info "dump-ast" ~doc in
  let term =
    Term.(
      const (fun lang_opt file common json ->
          let path = Fpath.v file in
          let lang =
            match lang_opt with
            | Some lang_str -> Lang.of_string lang_str
            | None -> Lang.lang_of_filename_exn path
          in
          { common; json; show_kind = DumpAST (path, lang) })
      $ lang_arg $ file_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let dump_config_cmd =
  let doc =
    "Dump the internal representation of the result of --config=<STRING>"
  in
  let config_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"CONFIG")
  in
  let info = Cmd.info "dump-config" ~doc in
  let term =
    Term.(
      const (fun config common json ->
          { common; json; show_kind = DumpConfig config })
      $ config_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let dump_rule_v2_cmd =
  let doc =
    "Dump the internal representation of a rule using the new (v2) syntax"
  in
  let file_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE")
  in
  let info = Cmd.info "dump-rule-v2" ~doc in
  let term =
    Term.(
      const (fun file common json ->
          { common; json; show_kind = DumpRuleV2 (Fpath.v file) })
      $ file_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

let debug_cmd =
  let doc = "Open an interactive debugging view" in
  let dir_arg =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"OUTPUT_DIR")
  in
  let root_arg = Arg.(value & pos 1 string "." & info [] ~docv:"ROOT") in
  let info = Cmd.info "debug" ~doc in
  let term =
    Term.(
      const (fun dir root common json ->
          let debug_settings =
            { output_dir = Option.map Fpath.v dir; root = Fpath.v root }
          in
          { common; json; show_kind = Debug debug_settings })
      $ dir_arg $ root_arg $ CLI_common.o_common $ o_json)
  in
  Cmd.v info term

(*************************************************************************)
(* Main command *)
(*************************************************************************)

let doc = "Show various types of information"

let man =
  [
    `S Cmdliner.Manpage.s_description; `P "Display various types of information";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info = Cmd.info "semgrep show" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let default =
    Term.(
      const (fun args ->
          match args with
          | [] ->
              Error.abort
                (Common.spf
                   "'semgrep show' expects a subcommand. Try 'semgrep show \
                    --help'.")
          | _ :: _ as unknown_args ->
              Error.abort
                (Common.spf "show command not supported: %s"
                   (String.concat " " unknown_args)))
      $ Arg.(value & pos_all string [] (info [])))
  in
  let group =
    Cmd.group cmdline_info ~default
      [
        version_cmd;
        supported_languages_cmd;
        identity_cmd;
        deployment_cmd;
        dump_pattern_cmd;
        dump_cst_cmd;
        dump_ast_cmd;
        dump_config_cmd;
        dump_rule_v2_cmd;
        debug_cmd;
      ]
  in
  CLI_common.eval_value ~argv group
