open Common
module E = Error
module FT = File_type
module C = Semgrep_dashdash_config

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Partially translated from config_resolver.py

   TODO:
    - handle the registry-aware jsonnet format (LONG)
    - lots of stuff ...
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* input *)
type rules_source =
  (* -e/-l/--replacement. In theory we could even parse the string to get
   * a XPattern.t *)
  | Pattern of string * Xlang.t * string option (* replacement *)
  (* --config. In theory we could even parse the string to get
   * some Semgrep_dashdash_config.config_kind list *)
  | Configs of string list
(* TODO? | ProjectUrl of Uri.t? or just use Configs for it? *)
[@@deriving show]

(* output *)
(* python: was called ConfigFile, and called a 'config' in text output *)
type rules_and_origin = {
  origin : origin;
  (* TODO? put a config_id: string option? or config prefix? or
   * compute it later based on the origin?
   *)
  rules : Rule.rules;
  errors : Rule.invalid_rule_error list;
}

(* TODO? more complex origin? Remote of Uri.t | Local of filename | Inline? *)
and origin = Common.filename option (* None for remote files *)
[@@deriving show]

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

(*****************************************************************************)
(* Registry and yaml aware jsonnet *)
(*****************************************************************************)

let parse_yaml_for_jsonnet (file : Common.filename) : AST_jsonnet.program =
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

let import_callback base str =
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
          let kind = Semgrep_dashdash_config.config_kind_of_config_str s in
          match kind with
          | C.R rkind ->
              let url = Semgrep_dashdash_config.url_of_registry_kind rkind in
              Some url
          (* TODO: this assumes every URLs are for yaml, but maybe we could
           * also import URLs to jsonnet files or gist! or look at the
           * header mimetype when downloading the URL to decide how to
           * convert it further?
           *)
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
             (* TODO? factorize with load_rules_from_url? but here we
              * must return some AST_jsonnet, not rules (yet).
              *)
             Logs.debug (fun m ->
                 m "trying to download from %s" (Uri.to_string url));
             let content =
               try Network.get url with
               | Timeout _ as exn -> Exception.catch_and_reraise exn
               | exn ->
                   (* was raise Semgrep_error, but equivalent to abort now *)
                   Error.abort
                     (spf "Failed to download config from %s: %s"
                        (Uri.to_string url) (Common.exn_to_s exn))
             in
             Logs.debug (fun m ->
                 m "finished downloading from %s" (Uri.to_string url));
             Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
                 (* LATER: adjust locations so refer to registry URL *)
                 parse_yaml_for_jsonnet file))

(* similar to Parse_rule.parse_file but with special import callbacks
 * for a registry-aware jsonnet.
 *)
let parse_rule (file : filename) : Rule.rules * Rule.invalid_rule_error list =
  match FT.file_type_of_file file with
  | FT.Config FT.Jsonnet ->
      Logs.err (fun m ->
          m
            "Support for Jsonnet rules is experimental and currently meant for \
             internal use only. The syntax may change or be removed at any \
             point.");
      let ast = Parse_jsonnet.parse_program file in
      let core = Desugar_jsonnet.desugar_program ~import_callback file ast in
      let value_ = Eval_jsonnet.eval_program core in
      let gen = Manifest_jsonnet_to_AST_generic.manifest_value value_ in
      (* TODO: put to true at some point *)
      Parse_rule.parse_generic_ast ~error_recovery:false file gen
  | _else_ -> Parse_rule.parse_and_filter_invalid_rules file

(*****************************************************************************)
(* Loading rules *)
(*****************************************************************************)

(* Note that we don't sanity check Parse_rule.is_valid_rule_filename,
 * so if you explicitely pass a file that does not have the right
 * extension, we will still process it
 * (could be useful for .jsonnet, which is not recognized yet as a
 *  Parse_rule.is_valid_rule_filename, but we still need ojsonnet to
 *  be done).
 *)
let load_rules_from_file file : rules_and_origin =
  Logs.debug (fun m -> m "loading local config from %s" file);
  if Sys.file_exists file then (
    let rules, errors = parse_rule file in
    Logs.debug (fun m -> m "Done loading local config from %s" file);
    { origin = Some file; rules; errors })
  else
    (* This should never happen because Semgrep_dashdash_config only builds
     * a File case if the file actually exists.
     *)
    Error.abort (spf "file %s does not exist anymore" file)

let load_rules_from_url url : rules_and_origin =
  (* TOPORT? _nice_semgrep_url() *)
  Logs.debug (fun m -> m "trying to download from %s" (Uri.to_string url));
  let content =
    try Network.get url with
    | Timeout _ as exn -> Exception.catch_and_reraise exn
    | exn ->
        (* was raise Semgrep_error, but equivalent to abort now *)
        Error.abort
          (spf "Failed to download config from %s: %s" (Uri.to_string url)
             (Common.exn_to_s exn))
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  Common2.with_tmp_file ~str:content ~ext:"yaml" (fun file ->
      let res = load_rules_from_file file in
      { res with origin = None })

let rules_from_dashdash_config (kind : Semgrep_dashdash_config.config_kind) :
    rules_and_origin list =
  match kind with
  | C.File file -> [ load_rules_from_file file ]
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
      |> Common.map load_rules_from_file
  | C.URL url -> [ load_rules_from_url url ]
  | C.R rkind ->
      let url = Semgrep_dashdash_config.url_of_registry_kind rkind in
      [ load_rules_from_url url ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: rewrite rule_id of the rules using x.path origin? *)
let rules_from_rules_source (source : rules_source) : rules_and_origin list =
  match source with
  | Configs xs ->
      xs
      |> List.concat_map (fun str ->
             let kind = Semgrep_dashdash_config.config_kind_of_config_str str in
             rules_from_dashdash_config kind)
  | Pattern (pat, xlang, fix) ->
      let fk = Parse_info.unsafe_fake_info "" in
      (* better: '-e foo -l regex' not handled in original semgrep,
       * got a weird 'invalid pattern clause' error.
       * better: '-e foo -l generic' not handled in semgrep-core
       * TODO? some try and abort because we can get parse errors?
       *)
      let xpat = Parse_rule.parse_xpattern xlang (pat, fk) in
      let rule = Rule.rule_of_xpattern xlang xpat in
      let rule = { rule with id = (Constants.cli_rule_id, fk); fix } in
      (* TODO? transform the pattern parse error in invalid_rule_error? *)
      [ { origin = None; rules = [ rule ]; errors = [] } ]
  [@@profiling]
