open Common
open Fpath_.Operators
module E = Error
module Env = Semgrep_envvars
module FT = File_type
module C = Rules_config
module R = Rule
module XP = Xpattern
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Fetching rules from the local filesystem or from the network (registry).

   Note that you should not hadd some [@@profiling] on functions suffixed
   with _async because the profiling info will be useless; the body of
   the function is actually called from elsewhere.

   TODO:
    - lots of stuff ...

   osemgrep-only:
    - can pass -e without -l (try all possible languages)
   osemgrep-pro-only:
    - TODO use a registry cache to speedup things

   Partially translated from config_resolver.py
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* python: was called ConfigFile, and called a 'config' in text output.
 * TODO? maybe we don't need this intermediate type anymore; just return
 * a pair, which would remove the need for partition_rules_and_errors.
 *)
type rules_and_origin = {
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
  origin : origin; (* used by Validate_subcommand *)
}

(* TODO? more complex origin? Remote of Uri.t | Embedded of Fpath.t ?
 * or just put the Semgrep_dashdash_config.config_kind it comes from?
 * This type is used only for rewrite_rule_ids.
 *)
and origin =
  | CLI_argument
  | Local_file of Fpath.t
  | Registry
  | App
  | Untrusted_remote of Uri.t
[@@deriving show]

(*****************************************************************************)
(* Rewrite rule ids *)
(*****************************************************************************)

let prefix_for_fpath_opt (fpath : Fpath.t) : string option =
  assert (Fpath.is_file_path fpath);
  let* rel_path =
    if Fpath.is_rel fpath then Some fpath
      (* python: paths had no common prefix; not possible to relativize *)
    else Fpath.rem_prefix (Fpath.v (Sys.getcwd ())) fpath
  in
  (* LATER: we should use Fpath.normalize first, but pysemgrep
   * doesn't as shown by
     tests/default/e2e/test_check.py::test_basic_rule__relative
   * so we reproduce the same behavior, leading sometimes to some
   * weird rule id like "rules....rules.test" when passing
   * rules/../rules/test.yaml to --config.
   * TODO? pass legacy flag and improve the behavior when not legacy?
   *)
  match List.rev (Fpath.segs rel_path) with
  | [] -> raise Impossible
  | [ _file ] -> None
  | _file :: dirs ->
      let prefix =
        dirs |> List.rev |> List_.map (fun s -> s ^ ".") |> String.concat ""
      in
      Some prefix

let mk_rewrite_rule_ids (origin : origin) : Rule_ID.t -> Rule_ID.t =
 fun (rule_id : Rule_ID.t) ->
  (*
   Check the validity of the rule ID and prepend the path to rule file if
   the rewrite_rule_ids option is set.
*)
  let opt_prefix =
    match origin with
    | Local_file fpath -> prefix_for_fpath_opt fpath
    | _ -> None
  in
  match opt_prefix with
  | None -> rule_id
  | Some prefix ->
      Rule_ID.sanitize_string prefix ^ Rule_ID.to_string rule_id
      |> Rule_ID.of_string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let partition_rules_and_errors (xs : rules_and_origin list) :
    Rule.rules * Rule.invalid_rule_error list =
  let (rules : Rule.rules) = xs |> List.concat_map (fun x -> x.rules) in
  let (errors : Rule.invalid_rule_error list) =
    xs |> List.concat_map (fun x -> x.errors)
  in
  (rules, errors)

let fetch_content_from_url_async ?(token_opt = None) caps (url : Uri.t) :
    string Lwt.t =
  (* TOPORT? _nice_semgrep_url() *)
  Logs.debug (fun m -> m "trying to download from %s" (Uri.to_string url));
  let content =
    let headers =
      match token_opt with
      | None -> None
      | Some token -> Some [ Auth.auth_header_of_token token ]
    in
    let%lwt res = Http_helpers.get_async ?headers caps#network url in
    match res with
    | Ok (body, _) -> Lwt.return body
    | Error (msg, _) ->
        (* was raise Semgrep_error, but equivalent to abort now *)
        Error.abort
          (spf "Failed to download config from %s: %s" (Uri.to_string url) msg)
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  content

let fetch_content_from_registry_url_async ~token_opt caps url =
  Metrics_.g.is_using_registry <- true;
  fetch_content_from_url_async ~token_opt caps url

let fetch_content_from_registry_url ~token_opt caps url =
  Lwt_platform.run (fetch_content_from_registry_url_async ~token_opt caps url)
[@@profiling]

(*****************************************************************************)
(* Registry and yaml aware jsonnet *)
(*****************************************************************************)

let parse_yaml_for_jsonnet (file : Fpath.t) : AST_jsonnet.program =
  Logs.debug (fun m -> m "loading yaml file %s, converting to jsonnet" !!file);
  (* TODO? or use Yaml_to_generic.parse_yaml_file which seems
   * to be used to parse semgrep rules?
   *)
  let gen = Yaml_to_generic.program file in
  (* python: we were simply using a yaml parser and then
   * dumping it back as JSON and then parsing the JSON (which is
   * valid jsonnet). What we do here is a bit more complicated but
   * the advantage is that we get proper error location then!
   *)
  AST_generic_to_jsonnet.program gen

let mk_import_callback (caps : < Cap.network ; Cap.tmp ; .. >) base str =
  match str with
  | s when s =~ ".*\\.y[a]?ml$" ->
      (* On the fly conversion from yaml to jsonnet. We can do
       * 'local x = import "foo.yml";'!
       *)
      let final_path = Fpath.v base / str in
      Some (parse_yaml_for_jsonnet final_path)
  | s ->
      let url_opt =
        try
          let in_docker = !Semgrep_envvars.v.in_docker in
          let kind = Rules_config.parse_config_string ~in_docker s in
          match kind with
          | C.A _ -> failwith "TODO: app_config in jsonnet not handled"
          | C.R rkind ->
              let url = Semgrep_Registry.url_of_registry_config_kind rkind in
              Some url
          | C.URL url -> Some url
          (* TODO? allow to import any config_str? even a directory?
           * factorize with rules_from_dashdash_config?
           *)
          | C.Dir _
          | C.File _ ->
              None
        with
        | E.Semgrep_error _ -> None
      in
      url_opt
      |> Option.map (fun url ->
             (* similar to load_rules_from_url() but here we
              * must return some AST_jsonnet, not rules (yet).
              *
              * TODO: ask for JSON in headers which improves performance
              * because Yaml rule parsing is slower than Json rule parsing.
              *
              * TODO: fix token_opt parameter. Currently we don't pass it.
              * import_callback either needs an additional parameter, or
              * parse_rule should take an import_callback as a parameter.
              *)
             let content =
               fetch_content_from_registry_url ~token_opt:None caps url
             in
             (* TODO: this assumes every URLs are for yaml, but maybe we could
              * also import URLs to jsonnet files or gist! or look at the
              * header mimetype when downloading the URL to decide how to
              * convert it further?
              *)
             CapTmp.with_tmp_file caps#tmp ~str:content ~ext:"yaml" (fun file ->
                 (* LATER: adjust locations so refer to registry URL *)
                 parse_yaml_for_jsonnet file))
[@@profiling]

(* Performs modification to metadata for non-registry/app originating rules.
 * This is so that we can have trusted data in metadata subsequent to this
 * point and rely on the values in certain fields later.
 *
 * Currently this is important for:
 *  - Secrets
 *    * 'semgrep.dev'.rule.origin checked for validators
 *)
let modify_registry_provided_metadata (origin : origin) (rule : Rule.t) =
  match origin with
  | Registry
  | App ->
      rule
  | CLI_argument
  | Local_file _
  | Untrusted_remote _ ->
      let replace obj key v =
        match (obj : JSON.t) with
        | Object members ->
            JSON.Object
              (List_.map
                 (function
                   | key', _ when key = key' -> (key, v)
                   | x -> x)
                 members)
        | x -> x
      in
      (* SECURITY: Set metadata from non-registry secrets rules so that
       * validators are not run. The default requirement is that the rule be
       * served from the pro origin. Without this, local rules could use
       * validators which may exfiltrate data from source code.
       *)
      let updated_metdata =
        let* metadata = rule.metadata in
        let* registry_data = JSON.member "semgrep.dev" metadata in
        let* rule_data = JSON.member "rule" registry_data in
        match rule.product with
        | `Secrets ->
            replace rule_data "origin" (JSON.String "local")
            |> replace registry_data "rule"
            |> replace metadata "semgrep.dev"
            |> fun x -> Some (Some x)
        | _ -> None
      in
      { rule with metadata = updated_metdata ||| rule.metadata }

(* similar to Parse_rule.parse_file but with special import callbacks
 * for a registry-aware jsonnet.
 *)
let parse_rule ~rewrite_rule_ids ~origin caps (file : Fpath.t) :
    (Rule.rules * Rule.invalid_rule_error list, Rule.error) Result.t =
  let rule_id_rewriter =
    if rewrite_rule_ids then Some (mk_rewrite_rule_ids origin) else None
  in
  try
    let rules, errors =
      match FT.file_type_of_file file with
      | FT.Config FT.Jsonnet ->
          Logs.warn (fun m ->
              m
                "Support for Jsonnet rules is experimental and currently meant \
                 for internal use only. The syntax may change or be removed at \
                 any point.");
          let ast = Parse_jsonnet.parse_program file in
          let core =
            Desugar_jsonnet.desugar_program
              ~import_callback:(mk_import_callback caps) file ast
          in
          let value_ = Eval_jsonnet.eval_program core in
          let gen = Manifest_jsonnet_to_AST_generic.manifest_value value_ in
          (* TODO: put to true at some point *)
          Parse_rule.parse_generic_ast ~rewrite_rule_ids:rule_id_rewriter
            ~error_recovery:false file gen
      | _ ->
          Parse_rule.parse_and_filter_invalid_rules
            ~rewrite_rule_ids:rule_id_rewriter file
    in
    Ok (List_.map (modify_registry_provided_metadata origin) rules, errors)
  with
  | Rule.Error err -> Error err
  | Parsing_error.Other_error (s, t) ->
      Error { rule_id = None; kind = Rule.InvalidYaml (s, t) }

(*****************************************************************************)
(* Loading rules *)
(*****************************************************************************)

(* Note that we don't sanity check Parse_rule.is_valid_rule_filename(),
 * so if you explicitely pass a file that does not have the right
 * extension, we will still process it
 * (could be useful for .jsonnet, which is not recognized yet as a
 *  Parse_rule.is_valid_rule_filename, but we still need ojsonnet to
 *  be done).
 *)
let load_rules_from_file ~rewrite_rule_ids ~origin caps (file : Fpath.t) :
    (rules_and_origin, Rule.error) Result.t =
  Logs.debug (fun m -> m "loading local config from %s" !!file);
  if Sys.file_exists !!file then
    match parse_rule ~rewrite_rule_ids ~origin caps file with
    | Ok (rules, errors) ->
        Logs.debug (fun m -> m "Done loading local config from %s" !!file);
        Ok { rules; errors; origin = Local_file file }
    | Error err -> Error err
  else
    (* This should never happen because Semgrep_dashdash_config only builds
     * a File case if the file actually exists.
     *)
    Error.abort (spf "file %s does not exist anymore" !!file)

let load_rules_from_url_async ~origin ?token_opt ?(ext = "yaml") caps url :
    (rules_and_origin, Rule.error) Result.t Lwt.t =
  let%lwt content = fetch_content_from_url_async ?token_opt caps url in
  let ext, content =
    if ext = "policy" then
      (* project rule_config, from config_resolver.py in _make_config_request *)
      try
        match Yojson.Basic.from_string content with
        | `Assoc e -> (
            match List.assoc "rule_config" e with
            | `String e -> ("json", e)
            | _else -> (ext, content))
        | _else -> (ext, content)
      with
      | _failure -> (ext, content)
    else (ext, content)
  in
  CapTmp.with_tmp_file caps#tmp ~str:content ~ext (fun file ->
      load_rules_from_file ~rewrite_rule_ids:false ~origin caps file)
  |> Lwt.return

let load_rules_from_url ~origin ?token_opt ?(ext = "yaml") caps url :
    (rules_and_origin, Rule.error) Result.t =
  Lwt_platform.run (load_rules_from_url_async ~origin ?token_opt ~ext caps url)
[@@profiling]

(* TODO: merge caps and token_opt and caps_opt? *)
let rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt caps kind :
    (rules_and_origin list * Rule.error list) Lwt.t =
  match kind with
  | C.File path ->
      Lwt.return
        (Result_.partition_result
           (load_rules_from_file ~rewrite_rule_ids ~origin:(Local_file path)
              caps)
           [ path ])
  | C.Dir dir ->
      (* We used to skip dot files under [dir], but keeping rules/.semgrep.yml,
       * but not path/.github/foo.yml, but keeping src/.semgrep/bad_pattern.yml
       * but not ./.pre-commit-config.yaml, ... This was mainly because
       * we used to fetch rules from ~/.semgrep/ implicitely when --config
       * was not given, but this feature was removed, so now we can KISS.
       *)
      List_files.list dir
      |> List.filter Rule_file.is_valid_rule_filename
      |> List_.map (fun file ->
             load_rules_from_file ~rewrite_rule_ids ~origin:(Local_file file)
               caps file)
      |> Result_.partition_result Fun.id
      |> Lwt.return
  | C.URL url ->
      (* TODO: Re-enable passing in our token to trusted remote urls.
         * This is currently disabled because we don't want to pass our token
         * to untrusted endpoints. There should be a relatively painless way
         * to do this, but this can be addressed in a follow-up PR.
      *)
      let%lwt rules =
        load_rules_from_url_async ~origin:(Untrusted_remote url) ~token_opt:None
          caps url
      in
      [ rules ] |> Result_.partition_result Fun.id |> Lwt.return
  | C.R rkind ->
      let url = Semgrep_Registry.url_of_registry_config_kind rkind in
      let%lwt content =
        fetch_content_from_registry_url_async ~token_opt caps url
      in
      CapTmp.with_tmp_file caps#tmp ~str:content ~ext:"yaml" (fun file ->
          [ load_rules_from_file ~rewrite_rule_ids ~origin:Registry caps file ])
      |> Result_.partition_result Fun.id
      |> Lwt.return
  | C.A Policy ->
      let token =
        match token_opt with
        | None ->
            Error.abort
              (spf
                 "Cannot to download rules from policy without authorization \
                  token")
        | Some token -> token
      in
      let caps' = Auth.cap_token_and_network token caps in
      let uri = Semgrep_App.url_for_policy caps' in
      let caps'' = Auth.cap_token_and_network_and_tmp token caps in
      let%lwt rules_and_errors =
        load_rules_from_url_async ~token_opt ~ext:"policy" ~origin:Registry
          caps'' uri
      in
      Metrics_.g.is_using_app <- true;
      [ rules_and_errors ] |> Result_.partition_result Fun.id |> Lwt.return
  | C.A SupplyChain ->
      Metrics_.g.is_using_app <- true;
      failwith "TODO: SupplyChain not handled yet"

let rules_from_dashdash_config ~rewrite_rule_ids ~token_opt caps kind :
    rules_and_origin list * Rule.error list =
  Lwt_platform.run
    (rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt caps kind)
[@@profiling]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rules_from_patterns (patterns, xlang_opt, fix) : rules_and_origin list =
  let fk = Tok.unsafe_fake_tok "" in
  let rules_and_origin_for_xlang xlang =
    let xpats =
      List_.map
        (fun (positive, pat) ->
          let xpat = Parse_rule.parse_xpattern xlang (pat, fk) in
          (* force the parsing of the pattern to get the parse error if any *)
          (match xpat.XP.pat with
          | XP.Sem (lpat, _) -> Lazy.force lpat |> ignore
          | XP.Spacegrep _
          | XP.Aliengrep _
          | XP.Regexp _ ->
              ());
          (positive, xpat))
        patterns
    in
    let of_xpat (positive, xpat) =
      if positive then Rule.f (Rule.P xpat)
      else Rule.f (Rule.Not (Tok.unsafe_fake_tok "", Rule.f (Rule.P xpat)))
    in
    let formula =
      match xpats with
      | [ (positive, xpat) ] -> of_xpat (positive, xpat)
      | _ ->
          Rule.And (Tok.unsafe_fake_tok "", List_.map of_xpat xpats) |> Rule.f
    in
    let rule = Rule.rule_of_formula xlang formula in
    let rule = { rule with id = (Constants.rule_id_for_dash_e, fk); fix } in
    { rules = [ rule ]; errors = []; origin = CLI_argument }
  in
  match xlang_opt with
  | Some xlang ->
      (* TODO? capture also parse errors here? and transform the pattern
         * parse error in invalid_rule_error to return in rules_and_origin? *)
      [ rules_and_origin_for_xlang xlang ]
  (* osemgrep-only: better: can use -e without -l! we try all languages *)
  | None ->
      (* We need uniq_by because Lang.assoc contain multiple times the
         * same value, for instance we have ("cpp", Cpp); ("c++", Cpp) in
         * Lang.assoc
         * TODO? use Xlang.assoc instead?
      *)
      let all_langs =
        Lang.assoc
        |> List_.map (fun (_k, l) -> l)
        |> List_.uniq_by ( =*= )
        (* TODO: we currently get a segfault with the Dart parser
           * (for example on a pattern like ': string (* filename *)'), so we
           * skip Dart for now (which anyway is not really supported).
        *)
        |> List_.exclude (fun x -> x =*= Lang.Dart)
      in
      all_langs
      |> List_.map_filter (fun l ->
             try
               let xlang = Xlang.of_lang l in
               let r = rules_and_origin_for_xlang xlang in
               Logs.debug (fun m ->
                   m "language %s valid for the pattern" (Lang.show l));
               Some r
             with
             | R.Error _
             | Failure _ ->
                 None)

(* python: mix of resolver_config.get_config() and get_rules() *)
let rules_from_rules_source_async ~token_opt ~rewrite_rule_ids ~strict caps
    (src : Rules_source.t) : rules_and_origin list Lwt.t =
  let%lwt rules_and_origins, errors =
    match src with
    | Configs xs ->
        let%lwt pairs_list =
          xs
          |> Lwt_list.map_p (fun str ->
                 let in_docker = !Semgrep_envvars.v.in_docker in
                 let config = Rules_config.parse_config_string ~in_docker str in
                 rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt
                   caps config)
        in
        let rules_and_origins_nested, errors_nested =
          Common2.unzip pairs_list
        in
        let rules_and_origins, errors =
          (List.flatten rules_and_origins_nested, List.flatten errors_nested)
        in

        (* error handling: *)
        if errors <> [] then
          raise
            (Error.Semgrep_error
               ( Common.spf
                   "invalid configuration file found (%d configs were invalid)"
                   (List.length errors),
                 Some (Exit_code.missing_config ~__LOC__) ));
        (* NOTE: We should default to config auto if no config was passed in an earlier step,
           but if we reach this step without a config, we emit the error below.
        *)
        if rules_and_origins =*= [] then
          raise
            (Error.Semgrep_error
               ( "No config given. Run with `--config auto` or see \
                  https://semgrep.dev/docs/running-rules/ for instructions on \
                  running with a specific config",
                 Some (Exit_code.missing_config ~__LOC__) ));

        Lwt.return (rules_and_origins, errors)
    (* better: '-e foo -l regex' was not handled in pysemgrep
       *  (got a weird 'invalid pattern clause' error)
       * better: '-e foo -l generic' was not handled in semgrep-core
    *)
    | Pattern (pat, xlang_opt, fix) ->
        Lwt.return (rules_from_patterns ([ (true, pat) ], xlang_opt, fix), [])
  in

  (* error handling: *)
  if errors <> [] && strict then
    raise
      (Error.Semgrep_error
         ( Common.spf "Ran with --strict and got %s while loading configs"
             (String_.unit_str (List.length errors) "error"),
           Some (Exit_code.missing_config ~__LOC__) ));

  (* errors should be empty here, because patterns cannot yet return errors *)
  Lwt.return rules_and_origins

(* You should probably avoid using directly this function and prefer
 * to use the _async variant above mixed with a spinner as in
 * Scan_subcommand.rules_from_rules_source()
 *)
let rules_from_rules_source ~token_opt ~rewrite_rule_ids ~strict caps
    (src : Rules_source.t) : rules_and_origin list =
  Lwt_platform.run
    (rules_from_rules_source_async ~token_opt ~rewrite_rule_ids ~strict caps src)
[@@profiling]
