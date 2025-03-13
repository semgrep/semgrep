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

type debug_settings = {
  output_dir : Fpath.t;
  targeting_conf : Find_targets.conf;
  rules_source : Rules_source.t;
}
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
  (* pro-only commands *)
  | Debug of debug_settings
  | DumpLockfile of Fpath.t (* lockfile *) * Fpath.t option (* manifest *)
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

(* UGLY: this is a duplicate of Scan_CLI.o_config. Copied since Scan_CLI depends
   on us for now. (not clear why --- should be the other way around, if anything.)
   Or this should be in Common_CLI. *)
let o_config : string list Term.t =
  let info =
    Arg.info [ "c"; "f"; "config" ]
      ~env:(Cmd.Env.info "SEMGREP_RULES")
      ~doc:
        {|YAML configuration file, directory of YAML files ending in
.yml|.yaml, URL of a configuration file, or Semgrep registry entry name.

Use --config auto to automatically obtain rules tailored to this project;
your project URL will be used to log in to the Semgrep registry.

To run multiple rule files simultaneously, use --config before every YAML,
URL, or Semgrep registry entry name.
For example `semgrep --config p/python --config myrules/myrule.yaml`

See https://semgrep.dev/docs/writing-rules/rule-syntax for information on
configuration file format.
|}
  in
  Arg.value (Arg.opt_all Arg.string [] info)

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

let debug_cmd caps =
  let doc = "Open an interactive debugging view" in
  let output_dir_info =
    Arg.info [ "output-dir" ] ~docv:"DIR"
      ~doc:
        "Directory to save the explorer output to. If not specified, uses a \
         temporary directory."
  in
  let output_dir_arg = Arg.(value & opt (some string) None & output_dir_info) in
  let roots_arg =
    Arg.(
      value & pos_all string [ "." ]
      & info [] ~docv:"TARGET_ROOTS"
          ~doc:"Files or directories to analyze. Defaults to current directory.")
  in
  let info = Cmd.info "debug" ~doc in
  let term =
    Term.(
      const (fun output_dir roots common json config ->
          let output_dir =
            Option.map Fpath.v output_dir
            |> Option.value ~default:(CapTmp.get_temp_dir_name caps#tmp)
          in
          let debug_settings =
            {
              output_dir;
              targeting_conf =
                {
                  Find_targets.default_conf with
                  explicit_targets =
                    Find_targets.Explicit_targets.of_list
                      (List_.map Fpath.v roots);
                };
              rules_source = Rules_source.Configs config;
            }
          in
          { common; json; show_kind = Debug debug_settings })
      $ output_dir_arg $ roots_arg $ CLI_common.o_common $ o_json $ o_config)
  in
  Cmd.v info term

let dump_lockfile_cmd =
  let doc =
    "Dump the extracted dependencies from a lockfile (and manifest) (pro only)"
  in
  (* TODO? we could add a --ecosystem for explicitely specifying
   * the ecosystem instead of inferring it from the name of the lockfile
   * in Lockfile.kind_of_filename_exn and Manifest.kind_of_filename_exn
   *)
  let manifest_arg =
    Arg.(value & pos 1 (some string) None & info [] ~docv:"MANIFEST")
  in
  let file_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"LOCKFILE")
  in
  let info = Cmd.info "dump-lockfile" ~doc in
  let term =
    Term.(
      const (fun file manifest_opt common ->
          let file = Fpath.v file in
          let manifest_opt = Option.map Fpath.v manifest_opt in
          {
            common;
            json = false;
            show_kind = DumpLockfile (file, manifest_opt);
          })
      $ file_arg $ manifest_arg $ CLI_common.o_common)
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

let parse_argv (caps : < Cap.tmp ; .. >) (argv : string array) : conf =
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
        debug_cmd caps;
        dump_lockfile_cmd;
      ]
  in
  CLI_common.eval_value ~argv group
