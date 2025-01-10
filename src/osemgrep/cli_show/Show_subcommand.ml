open Common
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There was no 'pysemgrep show' subcommand. Dumps were run via
 * 'semgrep scan --dump-ast ...' but it is better to have a separate
 * subcommand. Note that the legacy 'semgrep scan --dump-xxx' are
 * redirected to this file after having built a compatible Show_CLI.conf
 *
 * LATER: get rid of Core_CLI.dump_pattern and Core_CLI.dump_ast functions
 *
 * Note that we're using CapConsole.out() here, to print on stdout (Logs.app()
 * is printing on stderr, but for a show command it's probably better to
 * print on stdout).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* we need the network for the 'semgrep show identity/deployment' *)
type caps = < Cap.stdout ; Cap.network ; Cap.tmp >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* copy paste of Core_CLI.json_of_v *)
let json_of_v (v : OCaml.v) =
  let rec aux v =
    match v with
    | OCaml.VUnit -> J.String "()"
    | OCaml.VBool v1 -> if v1 then J.String "true" else J.String "false"
    | OCaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | OCaml.VChar v1 -> J.String (spf "'%c'" v1)
    | OCaml.VString v1 -> J.String v1
    | OCaml.VInt i -> J.Int (Int64.to_int i)
    | OCaml.VTuple xs -> J.Array (List_.map aux xs)
    | OCaml.VDict xs -> J.Object (List_.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (List_.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%Ld" s i64)
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (List_.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(* mostly a copy paste of Core_CLI.dump_v_to_format *)
let dump_v_to_format ~json (v : OCaml.v) =
  if json then J.string_of_json (json_of_v v) else OCaml.string_of_v v

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (caps : < caps ; .. >) (conf : Show_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Show_CLI.show_conf conf));
  let print = CapConsole.print caps#stdout in
  match conf.show_kind with
  | Version ->
      print Version.version;
      (* TODO? opportunity to perform version-check? *)
      Exit_code.ok ~__LOC__
  | Identity ->
      Whoami.print (caps :> < Cap.network ; Cap.stdout >) Whoami.Identity
  | Deployment ->
      Whoami.print (caps :> < Cap.network ; Cap.stdout >) Whoami.Deployment
  | SupportedLanguages ->
      print (spf "supported languages are: %s" Analyzer.supported_analyzers);
      Exit_code.ok ~__LOC__ (* dumpers *)
  (* TODO? error management? improve error message for parse errors?
   * or let CLI.safe_run do the right thing?
   *)
  | DumpPattern (str, lang) -> (
      (* mostly a copy paste of Core_CLI.dump_pattern *)
      (* TODO: maybe enable the "semgrep.parsing" src here *)
      match Parse_pattern.parse_pattern lang str with
      | Ok any ->
          let v = Meta_AST.vof_any any in
          let s = dump_v_to_format ~json:conf.json v in
          print s;
          Exit_code.ok ~__LOC__
      | Error s ->
          Logs.app (fun m -> m "Parse error: %s" s);
          Exit_code.invalid_pattern ~__LOC__)
  | DumpCST (file, lang) ->
      Test_parsing.dump_tree_sitter_cst lang file;
      Exit_code.ok ~__LOC__
  | DumpAST (file, lang) -> (
      (* mostly a copy paste of Core_CLI.dump_ast *)
      let Parsing_result2.
            {
              ast;
              errors;
              tolerated_errors;
              skipped_tokens;
              inserted_tokens;
              stat = _;
            } =
        (* alt: call Parse_target.just_parse_with_lang()
         * but usually we also want the naming/typing info.
         * we could add a flag --naming, but simpler to just call
         * parse_and_resolve_name by default
         * LATER? could also have a --pro where we use the advanced
         * naming/typing of Deep_scan by analyzing the files around too?
         *)
        Parse_target.parse_and_resolve_name lang file
      in
      let v = Meta_AST.vof_any (AST_generic.Pr ast) in
      (* 80 columns is too little *)
      UFormat.set_margin 120;
      let s = dump_v_to_format ~json:conf.json v in
      print s;
      match (errors @ tolerated_errors, skipped_tokens @ inserted_tokens) with
      | [], [] -> Exit_code.ok ~__LOC__
      | _, _ ->
          Logs.err (fun m ->
              m "errors=%s\ntolerated errors=%s\nskipped=%s\ninserted=%s"
                (Parsing_result2.format_errors errors)
                (Parsing_result2.format_errors tolerated_errors)
                (skipped_tokens
                |> List_.map Tok.show_location
                |> String.concat ", ")
                (inserted_tokens
                |> List_.map Tok.show_location
                |> String.concat ", "));
          Exit_code.invalid_code ~__LOC__)
  | DumpConfig config_str ->
      let settings = Semgrep_settings.load () in
      let token_opt = settings.api_token in
      let in_docker = !Semgrep_envvars.v.in_docker in
      let config = Rules_config.parse_config_string ~in_docker config_str in
      let rules_and_errors, errors =
        Rule_fetching.rules_from_dashdash_config
          ~rewrite_rule_ids:true (* command-line default *)
          ~token_opt
          (caps :> < Cap.network ; Cap.tmp >)
          config
      in

      if errors <> [] then
        raise
          (Error.Semgrep_error
             ( Common.spf "invalid configuration string found: %s" config_str,
               Some (Exit_code.missing_config ~__LOC__) ));

      rules_and_errors
      |> List.iter (fun x -> print (Rule_fetching.show_rules_and_origin x));
      Exit_code.ok ~__LOC__
  | DumpRuleV2 file ->
      (* TODO: use validation ocaml code to enforce the
       * CHECK: in rule_schema_v2.atd.
       * For example, check that at least one and only one field is set in formula.
       * Reclaim some of the jsonschema power. Maybe define combinators to express
       * that in rule_schema_v2_adapter.ml?
       *)
      let rules = Parse_rules_with_atd.parse_rules_v2 file in
      print (Rule_schema_v2_t.show_rules rules);
      Exit_code.ok ~__LOC__
  | DumpEnginePath _pro -> failwith "TODO: dump-engine-path not implemented yet"
  | DumpCommandForCore ->
      failwith "TODO: dump-command-for-core not implemented yet"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Show_CLI.parse_argv argv in
  run_conf caps conf
