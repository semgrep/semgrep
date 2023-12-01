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

   TODO:
    - lots of stuff ...

   osemgrep-only:
    - can pass -e without -l (try all possible languages)
    - use a registry cache to speedup things

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
   * doesn't as shown by tests/e2e/test_check.py::test_basic_rule__relative
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

let fetch_content_from_url_async ?(token_opt = None) (url : Uri.t) :
    string Lwt.t =
  (* TOPORT? _nice_semgrep_url() *)
  Logs.debug (fun m -> m "trying to download from %s" (Uri.to_string url));
  let content =
    let headers =
      match token_opt with
      | None -> None
      | Some token -> Some [ Auth.auth_header_of_token token ]
    in
    let%lwt res = Http_helpers.get_async ?headers url in
    match res with
    | Ok (body, _) -> Lwt.return body
    | Error (msg, _) ->
        (* was raise Semgrep_error, but equivalent to abort now *)
        Error.abort
          (spf "Failed to download config from %s: %s" (Uri.to_string url) msg)
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  content

(*****************************************************************************)
(* Registry caching *)
(*****************************************************************************)

(* We cache rules from the registry at the string content level.
 * alt: we could cache directly the parsed rules, but Rule.t now contains
 * closures (we now parse patterns lazily) which complicates things
 * (the Marshall module does not like functional values).
 * Anyway parsing a rule is now fast; what takes time is downloading
 * the rules from the network, which we optimize here.
 *
 * We also use a 24h cache for rules accessed from the registry. This
 * speedups things quite a lot for users without a great Internet connection.
 * In any case, the registry is rarely modified so users do not need to have
 * access to the very latest registry. Lagging 24 hours behind is fine
 * (and you can still use --no-registry-caching if really you want the latest).
 * This is similar to the network version_check that we also cache for 24
 * hours.
 * alt: We could also fetch in parallel the rules from the registry and
 * start the engine with the possibly old rules, and as we go check if
 * the rules changed and rerun the engine if needed. This is more complicated
 * though and would maybe require to switch to OCaml 5.0. Not worth it for now.
 *)
type _registry_cached_value =
  ( string (* the YAML rules, as an unparsed string *),
    float (* timestamp *) * Uri.t )
  Cache_disk.cached_value_on_disk

(* better: faster fetching by using a cache *)
let fetch_content_from_registry_url_async ~token_opt ~registry_caching url =
  Metrics_.g.is_using_registry <- true;
  if not registry_caching then fetch_content_from_url_async ~token_opt url
  else
    let cache_dir = !Env.v.user_dot_semgrep_dir / "cache" / "registry" in
    let cache_methods =
      {
        Cache_disk.cache_file_for_input =
          (fun url ->
            (* Better to obfuscate the cache files, like in Unison, to
             * discourage people to play with it. Also simple way to escape
             * special chars in a URL.
             * hopefully there will be no collision.
             *)
            let md5 = Digest.string (Uri.to_string url) in
            (* TODO: this also assumes yaml registry content. We need an
             * extension because Parse_rule.parse_file behaves differently
             * depending on the extension.
             *)
            cache_dir // Fpath.(v (Digest.to_hex md5) + "yaml"));
        cache_extra_for_input = (fun url -> (Unix.time (), url));
        check_extra =
          (fun (time2, url2) ->
            (* 24h hours caching *)
            Uri.equal url url2 && Unix.time () -. time2 <= 3600. *. 24.);
        input_to_string = Uri.to_string;
      }
    in
    Cache_disk.cache_lwt fetch_content_from_url_async cache_methods url

(*****************************************************************************)
(* Registry and yaml aware jsonnet *)
(*****************************************************************************)

let parse_yaml_for_jsonnet (file : string) : AST_jsonnet.program =
  Logs.debug (fun m -> m "loading yaml file %s, converting to jsonnet" file);
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

let import_callback ~registry_caching base str =
  match str with
  | s when s =~ ".*\\.y[a]?ml$" ->
      (* On the fly conversion from yaml to jsonnet. We can do
       * 'local x = import "foo.yml";'!
       *)
      let final_path = Filename.concat base str in
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
               Lwt_platform.run
                 (fetch_content_from_registry_url_async ~token_opt:None
                    ~registry_caching url)
             in
             (* TODO: this assumes every URLs are for yaml, but maybe we could
              * also import URLs to jsonnet files or gist! or look at the
              * header mimetype when downloading the URL to decide how to
              * convert it further?
              *)
             Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
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
 * We also pass a ~registry_caching so our registry-aware jsonnet is also
 * registry-cache aware.
 *)
let parse_rule ~rewrite_rule_ids ~origin ~registry_caching (file : Fpath.t) :
    Rule.rules * Rule.invalid_rule_error list =
  let rule_id_rewriter =
    if rewrite_rule_ids then Some (mk_rewrite_rule_ids origin) else None
  in
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
            ~import_callback:(import_callback ~registry_caching)
            file ast
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
  (List_.map (modify_registry_provided_metadata origin) rules, errors)

(*****************************************************************************)
(* Loading rules *)
(*****************************************************************************)

(* Note that we don't sanity check Parse_rule.is_valid_rule_filename(),
 * so if you explicitely pass a file that does not have the right
 * extension, we will still process it
 * (could be useful for .jsonnet, which is not recognized yet as a
 *  Parse_rule.is_valid_rule_filename, but we still need ojsonnet to
 *  be done).
 * We pass a ~registry_caching parameter here because the rule file can
 * be a jsonnet file importing rules from the registry.
 *)
let load_rules_from_file ~rewrite_rule_ids ~origin ~registry_caching
    (file : Fpath.t) : rules_and_origin =
  Logs.debug (fun m -> m "loading local config from %s" !!file);
  if Sys.file_exists !!file then (
    let rules, errors =
      parse_rule ~rewrite_rule_ids ~origin ~registry_caching file
    in
    Logs.debug (fun m -> m "Done loading local config from %s" !!file);
    { rules; errors; origin = Local_file file })
  else
    (* This should never happen because Semgrep_dashdash_config only builds
     * a File case if the file actually exists.
     *)
    Error.abort (spf "file %s does not exist anymore" !!file)

let load_rules_from_url_async ~origin ?token_opt ?(ext = "yaml") url :
    rules_and_origin Lwt.t =
  let%lwt content = fetch_content_from_url_async ?token_opt url in
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
  let rules =
    Common2.with_tmp_file ~str:content ~ext (fun file ->
        let file = Fpath.v file in
        load_rules_from_file ~rewrite_rule_ids:false ~origin
          ~registry_caching:false file)
  in
  Lwt.return rules

let load_rules_from_url ~origin ?token_opt ?(ext = "yaml") url :
    rules_and_origin =
  Lwt_platform.run (load_rules_from_url_async ~origin ?token_opt ~ext url)

let rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt
    ~registry_caching kind : rules_and_origin list Lwt.t =
  match kind with
  | C.File path ->
      Lwt.return
        [
          load_rules_from_file ~rewrite_rule_ids ~registry_caching
            ~origin:(Local_file path) path;
        ]
  | C.Dir dir ->
      List_files.list dir
      (* TOPORT:
         and not _is_hidden_config(l.relative_to(loc))
         ...
         def _is_hidden_config(loc: Path) -> bool:
         """
         Want to keep rules/.semgrep.yml but not path/.github/foo.yml
         Also want to keep src/.semgrep/bad_pattern.yml but not ./.pre-commit-config.yaml
         """
         return any(
           part != os.curdir
           and part != os.pardir
           and part.startswith(".")
           and DEFAULT_SEMGREP_CONFIG_NAME not in part
           for part in loc.parts
         )
      *)
      |> List.filter Parse_rule.is_valid_rule_filename
      |> List_.map (fun file ->
             load_rules_from_file ~rewrite_rule_ids ~origin:(Local_file file)
               ~registry_caching file)
      |> Lwt.return
  | C.URL url ->
      (* TODO: Re-enable passing in our token to trusted remote urls.
         * This is currently disabled because we don't want to pass our token
         * to untrusted endpoints. There should be a relatively painless way
         * to do this, but this can be addressed in a follow-up PR.
      *)
      let%lwt rules =
        load_rules_from_url_async ~origin:(Untrusted_remote url) ~token_opt:None
          url
      in
      Lwt.return [ rules ]
  | C.R rkind ->
      let url = Semgrep_Registry.url_of_registry_config_kind rkind in
      let%lwt content =
        fetch_content_from_registry_url_async ~token_opt ~registry_caching url
      in
      (* TODO: this also assumes every registry URL is for yaml *)
      let rules =
        Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
            let file = Fpath.v file in
            [
              load_rules_from_file ~rewrite_rule_ids ~origin:Registry
                ~registry_caching file;
            ])
      in
      Lwt.return rules
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
      let uri = Semgrep_App.url_for_policy ~token in
      let%lwt rules =
        load_rules_from_url_async ~token_opt ~ext:"policy" ~origin:Registry uri
      in
      Metrics_.g.is_using_app <- true;
      Lwt.return [ rules ]
  | C.A SupplyChain ->
      Metrics_.g.is_using_app <- true;
      failwith "TODO: SupplyChain not handled yet"

let rules_from_dashdash_config ~rewrite_rule_ids ~token_opt ~registry_caching
    kind : rules_and_origin list =
  Lwt_platform.run
    (rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt
       ~registry_caching kind)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rules_from_pattern pattern : rules_and_origin list =
  let pat, xlang_opt, fix = pattern in
  let fk = Tok.unsafe_fake_tok "" in
  let rules_and_origin_for_xlang xlang =
    let xpat = Parse_rule.parse_xpattern xlang (pat, fk) in
    (* force the parsing of the pattern to get the parse error if any *)
    (match xpat.XP.pat with
    | XP.Sem (lpat, _) -> Lazy.force lpat |> ignore
    | XP.Spacegrep _
    | XP.Aliengrep _
    | XP.Regexp _ ->
        ());
    let rule = Rule.rule_of_xpattern xlang xpat in
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

let rules_from_rules_source_async ~token_opt ~rewrite_rule_ids ~registry_caching
    (src : Rules_source.t) : rules_and_origin list Lwt.t =
  match src with
  | Configs xs ->
      xs
      |> Lwt_list.map_p (fun str ->
             let in_docker = !Semgrep_envvars.v.in_docker in
             let config = Rules_config.parse_config_string ~in_docker str in
             rules_from_dashdash_config_async ~rewrite_rule_ids ~token_opt
               ~registry_caching config)
      |> Lwt.map List.concat
  (* better: '-e foo -l regex' was not handled in pysemgrep
   *  (got a weird 'invalid pattern clause' error)
   * better: '-e foo -l generic' was not handled in semgrep-core
   *)
  | Pattern (pat, xlang_opt, fix) ->
      Lwt.return (rules_from_pattern (pat, xlang_opt, fix))
[@@profiling]

(* TODO We can probably delete this. *)
(* python: mix of resolver_config.get_config() and get_rules() *)
let rules_from_rules_source ~token_opt ~rewrite_rule_ids ~registry_caching
    (src : Rules_source.t) : rules_and_origin list =
  Lwt_platform.run
    (rules_from_rules_source_async ~token_opt ~rewrite_rule_ids
       ~registry_caching src)
