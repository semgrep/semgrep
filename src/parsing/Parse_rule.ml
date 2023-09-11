(* Yoann Padioleau, Emma Jin
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open File.Operators
open Parsing_common
module J = JSON
module FT = File_type
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module G = AST_generic
module Set = Set_
module MV = Metavariable
module PF = Parse_formula

let logger = Logging.get_logger [ __MODULE__ ]

(* TODO? make it a flag? *)
let use_ojsonnet = true

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema for a rule in rule_schema_v1.yaml.
 *
 * history: we used to parse a semgrep rule by simply using the basic API of
 * the OCaml 'yaml' library. This API allows converting a yaml file into
 * a simple and compact JSON.t value.
 * However, this JSON.t value did not contain any location information, which
 * made it hard to report errors in a YAML rule. This is why we switched
 * to the low-level API of the 'yaml' library that returns a stream
 * of tokens with location information. We actually used first that low-level
 * API to return the generic AST of a yaml file, to add support for
 * YAML in semgrep (allowing semgrep rules on any YAML files).
 * See the Yaml_to_generic.parse_rule function. We then (ab)used this function
 * to also parse a semgrep rule (which is a yaml file) in this file.
 *)

(*****************************************************************************)
(* Parsers for core fields (languages:, severity:) *)
(*****************************************************************************)

let parse_language ~id ((s, t) as _lang) : Lang.t =
  match Lang.of_string_opt s with
  | None -> Rule.raise_error (Some id) (InvalidRule (InvalidLanguage s, id, t))
  | Some l -> l

(*
   This list specifies target selection and possible pattern parsers.
   Constraints:
   - target selection is either generic or a list of programming languages
     that can be detected by file extensions and other quick lookups.
   - target analysis is done by one of the generic analyzers (regex,
     spacegrep, aliengrep, ...) or by trying to parse the semgrep pattern
     as one of several programming languages.
   This decouples target selection from pattern parsing.

   TODO: note that there's a few places in this file where we use
   Xlang.of_string which allows "spacegrep" and "aliengrep" so
   this might lead to inconsistencies as here we allow just "generic".
*)
let parse_languages ~id (options : Rule_options_t.t) langs : Rule.languages =
  let opt_target_selector, (target_analyzer : Xlang.t) =
    match langs with
    | [ (("none" | "regex"), _t) ] -> (None, LRegex)
    | [ ("generic", _t) ] -> (
        (* The generic mode now uses one of two possible engines.
           For now, we keep the name "generic" for both and use an option
           to choose one engine or the other. *)
        match options.generic_engine with
        | `Spacegrep -> (None, LSpacegrep)
        | `Aliengrep -> (None, LAliengrep))
    | xs -> (
        let rule_id, _ = id in
        let langs = xs |> Common.map (parse_language ~id:rule_id) in
        match langs with
        | [] ->
            Rule.raise_error (Some rule_id)
              (InvalidRule
                 (InvalidOther "we need at least one language", fst id, snd id))
        | x :: xs -> (Some langs, L (x, xs)))
  in
  { target_selector = opt_target_selector; target_analyzer }

let parse_severity ~id (s, t) : Rule.severity =
  match s with
  | "ERROR" -> Error
  | "WARNING" -> Warning
  | "INFO" -> Info
  | "INVENTORY" -> Inventory
  | "EXPERIMENT" -> Experiment
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s),
             id,
             t ))

(*****************************************************************************)
(* Parsers for extra (metavar-xxx:, fix:, etc.) *)
(*****************************************************************************)

let parse_fix_regex (env : env) (key : key) fields =
  let fix_regex_dict = yaml_to_dict env key fields in
  let (regex : string R.wrap) =
    take fix_regex_dict env parse_string_wrap "regex"
  in
  let (replacement : string) =
    take fix_regex_dict env parse_string "replacement"
  in
  let (count_opt : int option) =
    take_opt fix_regex_dict env parse_int "count"
  in
  (parse_regexp env regex, count_opt, replacement)

let parse_equivalences env key value =
  let parse_equivalence env equiv =
    match equiv.G.e with
    | G.Container
        ( Dict,
          ( _,
            [
              {
                e =
                  Container
                    ( Tuple,
                      ( _,
                        [
                          { e = L (String (_, ("equivalence", t), _)); _ };
                          value;
                        ],
                        _ ) );
                _;
              };
            ],
            _ ) ) ->
        parse_string env ("equivalence", t) value
    | _ ->
        error_at_expr env.id equiv
          "Expected `equivalence: $X` for each equivalences list item"
  in
  parse_list env key parse_equivalence value

let parse_paths env key value =
  let paths_dict = yaml_to_dict env key value in
  (* TODO: should imitate parse_string_wrap_list *)
  let parse_glob_list env (key : key) e =
    let extract_string env = function
      | { G.e = G.L (String (_, (value, _), _)); _ } -> (
          try (value, Glob.Parse.parse_string value) with
          | Glob.Lexer.Syntax_error _ ->
              error_at_key env.id key ("Invalid glob for " ^ fst key))
      | _ ->
          error_at_key env.id key
            ("Expected all values in the list to be globs for " ^ fst key)
    in
    parse_list env key extract_string e
  in
  let inc_opt, exc_opt =
    ( take_opt paths_dict env parse_glob_list "include",
      take_opt paths_dict env parse_glob_list "exclude" )
  in
  (* alt: we could use report_unparsed_fields(), but better to raise an error for now
     to be compatible with pysemgrep *)
  if Hashtbl.length paths_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)";
  { R.require = optlist_to_list inc_opt; exclude = optlist_to_list exc_opt }

(*****************************************************************************)
(* Check the validity of the Aliengrep options *)
(*****************************************************************************)

let parse_options rule_id (key : key) value =
  let s = J.string_of_json (generic_to_json rule_id key value) in
  let options =
    Common.save_excursion Atdgen_runtime.Util.Json.unknown_field_handler
      (fun _src_loc field_name ->
        (* for forward compatibility, better to not raise an exn and just
         * ignore the new fields.
         * TODO: we should use a warning/logging infra to report
         * this in the JSON to the semgrep wrapper and user.
         *)
        (*raise (InvalidYamlException (spf "unknown option: %s" field_name))*)
        pr2 (spf "WARNING: unknown option: %s" field_name))
      (fun () -> Rule_options_j.t_of_string s)
  in
  (options, Some key)

(*****************************************************************************)
(* Parsers for taint *)
(*****************************************************************************)

let requires_expr_to_precondition env key e =
  let invalid_requires () =
    error_at_key env.id key
      "Invalid `requires' expression, it must be a Python Boolean expression \
       over labels (any valid Python identifier) using operators `not', `or' \
       and `and'."
  in
  let rec expr_to_precondition e =
    match e.G.e with
    | G.L (G.Bool (v, _)) -> R.PBool v
    | G.N (G.Id ((str, _), _)) when Metavariable.is_metavar_name str ->
        error_at_key env.id key
          ("Invalid `requires' expression, metavariables cannot be used as \
            labels: " ^ str)
    | G.N (G.Id ((str, _), _)) -> R.PLabel str
    | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg e1 ], _)) ->
        PNot (expr_to_precondition e1)
    | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
        match (op, args_to_precondition args) with
        | G.And, xs -> R.PAnd xs
        | G.Or, xs -> R.POr xs
        | __else__ -> invalid_requires ())
    | __else__ -> invalid_requires ()
  and args_to_precondition args =
    match args with
    | [] -> []
    | G.Arg e :: args' -> expr_to_precondition e :: args_to_precondition args'
    | _ :: _args' -> invalid_requires ()
  in
  expr_to_precondition e

let parse_taint_requires env key x =
  let s = parse_string env key x in
  let e = parse_python_expression env key s in
  let range = AST_generic_helpers.range_of_any_opt (E e) in
  { R.precondition = requires_expr_to_precondition env key e; range }

(* TODO: can add a case where these take in only a single string *)
let parse_taint_source ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_source =
  let parse_from_dict dict f =
    let source_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:false
    in
    let source_control =
      take_opt dict env parse_bool "control" |> Option.value ~default:false
    in
    let label =
      take_opt dict env parse_string "label"
      |> Option.value ~default:R.default_source_label
    in
    let source_requires = take_opt dict env parse_taint_requires "requires" in
    let source_formula = f env dict in
    {
      R.source_formula;
      source_by_side_effect;
      source_control;
      label;
      source_requires;
    }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict PF.parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let source_formula = R.P (PF.parse_rule_xpattern env value) in
        {
          source_formula;
          source_by_side_effect = false;
          source_control = false;
          label = R.default_source_label;
          source_requires = None;
        }
    | Right dict -> parse_from_dict dict PF.parse_formula_from_dict

let parse_taint_propagator ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_propagator =
  let f =
    if is_old then PF.parse_formula_old_from_dict
    else PF.parse_formula_from_dict
  in
  let parse_from_dict dict f =
    let propagator_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:true
    in
    let from = take dict env parse_string_wrap "from" in
    let to_ = take dict env parse_string_wrap "to" in
    let propagator_requires =
      take_opt dict env parse_taint_requires "requires"
    in
    let propagator_label = take_opt dict env parse_string "label" in
    let propagator_replace_labels =
      take_opt dict env
        (fun env key v ->
          parse_listi env key (fun env v -> parse_string env key v) v)
        "replace-labels"
    in
    let propagator_formula = f env dict in
    {
      R.propagator_formula;
      propagator_by_side_effect;
      from;
      to_;
      propagator_requires;
      propagator_replace_labels;
      propagator_label;
    }
  in
  let dict = yaml_to_dict env key value in
  parse_from_dict dict f

let parse_taint_sanitizer ~(is_old : bool) env (key : key) (value : G.expr) =
  let parse_from_dict dict f =
    let sanitizer_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Option.value ~default:false
    in
    let not_conflicting =
      take_opt dict env parse_bool
        (if is_old then "not_conflicting" else "not-conflicting")
      |> Option.value ~default:false
    in
    let sanitizer_formula = f env dict in
    { sanitizer_formula; sanitizer_by_side_effect; R.not_conflicting }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict PF.parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let sanitizer_formula = R.P (PF.parse_rule_xpattern env value) in
        {
          sanitizer_formula;
          sanitizer_by_side_effect = false;
          R.not_conflicting = false;
        }
    | Right dict -> parse_from_dict dict PF.parse_formula_from_dict

let parse_taint_sink ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_sink =
  let sink_id = String.concat ":" env.path in
  let parse_from_dict dict f =
    let sink_requires = take_opt dict env parse_taint_requires "requires" in
    let sink_formula = f env dict in
    { R.sink_id; sink_formula; sink_requires }
  in
  if is_old then
    let dict = yaml_to_dict env key value in
    parse_from_dict dict PF.parse_formula_old_from_dict
  else
    match parse_str_or_dict env value with
    | Left value ->
        let sink_formula = R.P (PF.parse_rule_xpattern env value) in
        { sink_id; sink_formula; sink_requires = None }
    | Right dict -> parse_from_dict dict PF.parse_formula_from_dict

let parse_taint_pattern env key (value : G.expr) =
  let dict = yaml_to_dict env key value in
  let parse_specs parse_spec env key x =
    ( snd key,
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x )
  in
  let sources, propagators_opt, sanitizers_opt, sinks =
    ( take dict env (parse_specs (parse_taint_source ~is_old:false)) "sources",
      take_opt dict env
        (parse_specs (parse_taint_propagator ~is_old:false))
        "propagators",
      take_opt dict env
        (parse_specs (parse_taint_sanitizer ~is_old:false))
        "sanitizers",
      take dict env (parse_specs (parse_taint_sink ~is_old:false)) "sinks" )
  in
  `Taint
    {
      R.sources;
      propagators =
        (* optlist_to_list *)
        (match propagators_opt with
        | None -> []
        | Some (_, xs) -> xs);
      sanitizers = sanitizers_opt;
      sinks;
    }

(*****************************************************************************)
(* Parsers for extract mode *)
(*****************************************************************************)

let parse_extract_dest ~id lang : Xlang.t =
  match lang with
  | ("none" | "regex"), _ -> LRegex
  | ("generic" | "spacegrep"), _ -> LSpacegrep
  | "aliengrep", _ -> LAliengrep
  | lang -> L (parse_language ~id lang, [])

let parse_extract_reduction ~id (s, t) =
  match s with
  | "concat" -> R.Concat
  | "separate" -> R.Separate
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf "Bad extract reduction: %s (expected concat or separate)" s),
             id,
             t ))

let parse_extract_transform ~id (s, t) =
  match s with
  | "no_transform" -> R.NoTransform
  | "unquote_string" -> R.Unquote
  | "concat_json_string_array" -> R.ConcatJsonArray
  | s ->
      Rule.raise_error (Some id)
        (InvalidRule
           ( InvalidOther
               (spf
                  "Bad extract transform: %s (expected unquote_string or \
                   concat_json_string_array)"
                  s),
             id,
             t ))

let parse_rules_to_run_with_extract env key value =
  let ruleids_dict = yaml_to_dict env key value in
  let inc_opt, exc_opt =
    ( take_opt ruleids_dict env
        (parse_string_wrap_list Rule_ID.of_string)
        "include",
      take_opt ruleids_dict env
        (parse_string_wrap_list Rule_ID.of_string)
        "exclude" )
  in
  (* alt: we could use report_unparsed_fields(), but better to raise an error for now
     to be compatible with pysemgrep *)
  if Hashtbl.length ruleids_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)";
  {
    R.required_rules = optlist_to_list inc_opt;
    excluded_rules = optlist_to_list exc_opt;
  }

(*****************************************************************************)
(* Parsers used by step mode as well as general rules *)
(*****************************************************************************)

let parse_search_fields env rule_dict =
  let formula =
    take_opt rule_dict env (fun env _ expr -> PF.parse_formula env expr) "match"
  in
  match formula with
  | Some formula -> `Search formula
  | None -> `Search (PF.parse_formula_old_from_dict env rule_dict)

let parse_taint_fields env rule_dict =
  let parse_specs parse_spec env key x =
    ( snd key,
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x )
  in
  match Hashtbl.find_opt rule_dict.h "taint" with
  | Some (key, value) -> parse_taint_pattern env key value
  | __else__ ->
      let sources, propagators_opt, sanitizers_opt, sinks =
        ( take rule_dict env
            (parse_specs (parse_taint_source ~is_old:true))
            "pattern-sources",
          take_opt rule_dict env
            (parse_specs (parse_taint_propagator ~is_old:true))
            "pattern-propagators",
          take_opt rule_dict env
            (parse_specs (parse_taint_sanitizer ~is_old:true))
            "pattern-sanitizers",
          take rule_dict env
            (parse_specs (parse_taint_sink ~is_old:true))
            "pattern-sinks" )
      in
      `Taint
        {
          sources;
          propagators =
            (* optlist_to_list *)
            (match propagators_opt with
            | None -> []
            | Some (_, xs) -> xs);
          sanitizers = sanitizers_opt;
          sinks;
        }

(*****************************************************************************)
(* Parsers for step mode *)
(*****************************************************************************)

let parse_step_fields env key (value : G.expr) : R.step =
  let rd = yaml_to_dict env key value in
  let languages = take_no_env rd parse_string_wrap_list_no_env "languages" in
  (* No id, so error at the steps key
     TODO error earlier *)
  let rule_options =
    (* TODO: this is annoying and refers to the global options which may be
       incorrect anyway -> support an 'options' field next to 'languages'
       in the step object? *)
    Option.value env.options ~default:Rule_options.default_config
  in
  let step_id_str, tok = key in
  let id =
    (Rule_ID.of_string (* TODO: is this really a rule ID? *) step_id_str, tok)
  in
  let step_languages = parse_languages ~id rule_options languages in
  let env = { env with languages = step_languages } in
  let step_paths = take_opt rd env parse_paths "paths" in
  let mode_opt = take_opt rd env parse_string_wrap "mode" in
  let has_taint_key = Option.is_some (Hashtbl.find_opt rd.h "taint") in
  let step_mode =
    match (mode_opt, has_taint_key) with
    | None, false
    | Some ("search", _), false -> (
        match parse_search_fields env rd with
        | `Search formula -> `Search formula
        | _else_ -> raise Common.Impossible)
    | _, true
    | Some ("taint", _), _ -> (
        match parse_taint_fields env rd with
        | `Taint formula -> `Taint formula
        | _else_ -> raise Common.Impossible)
    | Some key, _ ->
        error_at_key env.id key
          (spf
             "Unexpected value for mode, should be 'search' or 'taint', not %s"
             (fst key))
  in
  { step_languages; step_paths; step_mode }

let parse_steps env key (value : G.expr) : R.step list =
  let parse_step step = parse_step_fields env key step in
  match value.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map parse_step xs
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

(*****************************************************************************)
(* Parsers for secrets mode *)
(*****************************************************************************)
let parse_secrets_fields env rule_dict : R.secrets =
  let secrets : R.formula list =
    take rule_dict env
      (fun env key expr ->
        parse_list env key
          (fun env dict_pair ->
            yaml_to_dict env key dict_pair |> PF.parse_formula_old_from_dict env)
          expr)
      "postprocessor-patterns"
  in
  let req = take rule_dict env yaml_to_dict "request" in
  let res = take rule_dict env yaml_to_dict "response" in
  let url = take req env parse_string "url" in
  let meth = take req env method_ "method" in
  let headers : Rule.header list =
    take req env yaml_to_dict "headers" |> fun { h; _ } ->
    Hashtbl.fold
      (fun name value lst ->
        { Rule.name; value = parse_string env (fst value) (snd value) } :: lst)
      h []
  in
  let body = take_opt req env parse_string "body" in
  let auth = take_opt req env parse_auth "auth" in
  let return_code = take res env parse_int "return_code" in
  let regex = take_opt res env parse_string "pattern-regex" in
  {
    secrets;
    request = { url; meth; headers; body; auth };
    response = { return_code; regex };
  }

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  (* We do this because we should only assume that we have a search mode rule
     if there is not a `taint` key present in the rule dict.
  *)
  let has_taint_key = Option.is_some (Hashtbl.find_opt rule_dict.h "taint") in
  match (mode_opt, has_taint_key) with
  | None, false
  | Some ("search", _), false ->
      parse_search_fields env rule_dict
  | _, true
  | Some ("taint", _), _ ->
      parse_taint_fields env rule_dict
  | Some ("extract", _), _ ->
      let formula = PF.parse_formula_old_from_dict env rule_dict in
      let dst_lang =
        take rule_dict env parse_string_wrap "dest-language"
        |> parse_extract_dest ~id:env.id
      in
      (* TODO: determine fmt---string with interpolated metavars? *)
      let extract = take rule_dict env parse_string "extract" in
      let extract_rule_ids =
        take_opt rule_dict env parse_rules_to_run_with_extract "dest-rules"
      in
      let transform =
        take_opt rule_dict env parse_string_wrap "transform"
        |> Option.map (parse_extract_transform ~id:env.id)
        |> Option.value ~default:R.NoTransform
      in
      let reduce =
        take_opt rule_dict env parse_string_wrap "reduce"
        |> Option.map (parse_extract_reduction ~id:env.id)
        |> Option.value ~default:R.Separate
      in
      `Extract
        { formula; dst_lang; extract_rule_ids; extract; reduce; transform }
  (* TODO: change this mode name to something more descriptive + not intentionally
   * ambigous sometime later.
   *)
  | Some ("semgrep_internal_postprocessor", _), _ ->
      `Secrets (parse_secrets_fields env rule_dict)
  (* TODO? should we use "mode: steps" instead? *)
  | Some ("step", _), _ ->
      let steps = take rule_dict env parse_steps "steps" in
      `Steps steps
  | Some key, _ ->
      error_at_key env.id key
        (spf
           "Unexpected value for mode, should be 'search', 'taint', 'extract', \
            or 'step', not %s"
           (fst key))

(* sanity check there are no remaining fields in rd *)
let report_unparsed_fields rd =
  (* those were not "consumed" *)
  Hashtbl.remove rd.h "pattern";
  Hashtbl.remove rd.h "patterns";
  match Common.hash_to_list rd.h with
  | [] -> ()
  | xs ->
      (* less: we could return an error, but better to be fault-tolerant
       * to futur extensions to the rule format
       *)
      xs
      |> List.iter (fun (k, _v) ->
             logger#warning "skipping unknown field: %s" k)

let parse_version key value =
  let str, tok = parse_string_wrap_no_env key value in
  match Version_info.of_string str with
  | Some version -> (version, tok)
  | None ->
      yaml_error_at_key key
        ("Expected a version of the form X.Y.Z for " ^ fst key)

let incompatible_version ?min_version ?max_version rule_id tok =
  Rule.raise_error (Some rule_id)
    (InvalidRule
       ( IncompatibleRule (Version_info.version, (min_version, max_version)),
         rule_id,
         tok ))

let check_version_compatibility rule_id ~min_version ~max_version =
  (match min_version with
  | None -> ()
  | Some (mini, tok) ->
      if not (Version_info.compare mini Version_info.version <= 0) then
        incompatible_version ?min_version:(Some mini) rule_id tok);
  match max_version with
  | None -> ()
  | Some (maxi, tok) ->
      if not (Version_info.compare Version_info.version maxi <= 0) then
        incompatible_version ?max_version:(Some maxi) rule_id tok

let parse_one_rule (t : G.tok) (i : int) (rule : G.expr) : Rule.t =
  let rd = yaml_to_dict_no_env ("rules", t) rule in
  (* We need a rule ID early to produce useful error messages. *)
  let ((rule_id, _) as id) =
    let rule_id_str, tok = take_no_env rd parse_string_wrap_no_env "id" in
    (Rule_ID.of_string rule_id_str, tok)
  in
  (* We need to check for version compatibility before attempting to interpret
     the rule. *)
  let min_version = take_opt_no_env rd parse_version "min-version" in
  let max_version = take_opt_no_env rd parse_version "max-version" in
  check_version_compatibility rule_id ~min_version ~max_version;

  let languages = take_no_env rd parse_string_wrap_list_no_env "languages" in
  let options_opt, options_key =
    match take_opt_no_env rd (parse_options rule_id) "options" with
    | None -> (None, None)
    | Some (options, options_key) -> (Some options, options_key)
  in
  let options = Option.value options_opt ~default:Rule_options.default_config in
  let languages = parse_languages ~id options languages in
  let env =
    {
      id = rule_id;
      languages;
      in_metavariable_pattern = false;
      path = [ string_of_int i; "rules" ];
      options_key;
      options = options_opt;
    }
  in
  let mode_opt = take_opt rd env parse_string_wrap "mode" in
  let mode = parse_mode env mode_opt rd in
  let message, severity =
    match mode with
    | `Extract _ -> ("", ("INFO", Tok.unsafe_fake_tok ""))
    | _ ->
        ( take rd env parse_string "message",
          take rd env parse_string_wrap "severity" )
  in
  let metadata_opt = take_opt_no_env rd (generic_to_json rule_id) "metadata" in
  let fix_opt = take_opt rd env parse_string "fix" in
  let fix_regex_opt = take_opt rd env parse_fix_regex "fix-regex" in
  let paths_opt = take_opt rd env parse_paths "paths" in
  let equivs_opt = take_opt rd env parse_equivalences "equivalences" in
  report_unparsed_fields rd;
  {
    R.id;
    min_version = Option.map fst min_version;
    max_version = Option.map fst max_version;
    message;
    languages;
    severity = parse_severity ~id:env.id severity;
    mode;
    (* optional fields *)
    metadata = metadata_opt;
    fix = fix_opt;
    fix_regexp = fix_regex_opt;
    paths = paths_opt;
    equivalences = equivs_opt;
    options = options_opt;
  }

let parse_generic_ast ?(error_recovery = false) (file : Fpath.t)
    (ast : AST_generic.program) : Rule.rules * Rule.invalid_rule_error list =
  let t, rules =
    match ast with
    | [ { G.s = G.ExprStmt (e, _); _ } ] -> (
        match e.G.e with
        | Container
            ( Dict,
              ( _,
                [
                  {
                    e =
                      Container
                        ( Tuple,
                          ( _,
                            [
                              { e = L (String (_, ("rules", _), _)); _ }; rules;
                            ],
                            _ ) );
                    _;
                  };
                ],
                _ ) ) -> (
            match rules.G.e with
            | G.Container (G.Array, (l, rules, _r)) -> (l, rules)
            | _ ->
                yaml_error_at_expr rules
                  "expected a list of rules following `rules:`")
        (* it's also ok to not have the toplevel rules:, anyway we never
         * used another toplevel key
         *)
        | G.Container (G.Array, (l, rules, _r)) -> (l, rules)
        | _ ->
            let loc = Tok.first_loc_of_file !!file in
            yaml_error (Tok.tok_of_loc loc)
              "missing rules entry as top-level key")
    | [] ->
        (* an empty rules file returns an empty list of rules *)
        (Tok.(tok_of_loc (first_loc_of_file !!file)), [])
    | _ -> assert false
    (* yaml_to_generic should always return a ExprStmt *)
  in
  let xs =
    rules
    |> Common.mapi (fun i rule ->
           if error_recovery then (
             try Left (parse_one_rule t i rule) with
             | Rule.Error { kind = InvalidRule ((kind, ruleid, _) as err); _ }
               ->
                 let s = Rule.string_of_invalid_rule_error_kind kind in
                 logger#warning "skipping rule %s, error = %s"
                   (ruleid :> string)
                   s;
                 Right err)
           else Left (parse_one_rule t i rule))
  in
  Common.partition_either (fun x -> x) xs

(* We can't call just Yaml_to_generic.program below because when we parse
 * YAML Semgrep rules, we preprocess unicode characters differently.
 * We also need to translate Parse_info.Other_error exn in
 * (Rule.Err (Rule.InvalidYaml)) exn.
 * Note that we can't generate a Rule.Err in Yaml_to_generic directly
 * because we don't want parsing/other/ to depend on core/.
 *)
let parse_yaml_rule_file file =
  let str = Common.read_file file in
  try Yaml_to_generic.parse_yaml_file file str with
  | Parsing_error.Other_error (s, t) ->
      Rule.raise_error None (InvalidYaml (s, t))

let parse_file ?error_recovery file =
  let ast =
    match FT.file_type_of_file file with
    | FT.Config FT.Json ->
        (* in a parsing-rule context, we don't want the parsed strings by
         * Parse_json.parse_program to remain escaped. For example with this
         * JSON rule:
         * { "rules": [ {
         *       "id": "x",
         *       "message": "",
         *       "languages": ["python"],
         *       "severity": "WARNING",
         *       "pattern": "\"hello\""
         *     }
         *   ]
         * }
         * we want the pattern in the generic AST of the rule to contain the
         * string '"hello"', without the antislash, otherwise
         * Parse_python.parse_any will fail parsing it.
         *
         * Note that we didn't have this problem before when we were using
         * Yojson to parse a JSON rule, because Yojson correctly unescaped
         * and returned the "final string".
         *
         * Note that this is handled correctly by Yaml_to_generic.parse_rule
         * below.
         *)
        Json_to_generic.program ~unescape_strings:true
          (Parse_json.parse_program !!file)
    | FT.Config FT.Jsonnet ->
        if use_ojsonnet then
          let ast = Parse_jsonnet.parse_program file in
          (* Note that here we do not support registry-aware import;
           * those are defined in osemgrep/.../Rule_fetching.ml where
           * we use Network.get functions. Thus, semgrep-core -dump_rule
           * will not work with registry-aware import either.
           * Use osemgrep --dump-config instead.
           *)
          let core = Desugar_jsonnet.desugar_program file ast in
          let value_ = Eval_jsonnet.eval_program core in
          Manifest_jsonnet_to_AST_generic.manifest_value value_
        else
          Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
              let cmd = spf "jsonnet -J vendor %s -o %s" !!file tmpfile in
              let n = Sys.command cmd in
              if n <> 0 then failwith (spf "error executing %s" cmd);
              let ast = Parse_json.parse_program tmpfile in
              Json_to_generic.program ~unescape_strings:true ast)
    | FT.Config FT.Yaml -> parse_yaml_rule_file ~is_target:true !!file
    | _else_ ->
        logger#error "wrong rule format, only JSON/YAML/JSONNET are valid";
        logger#info "trying to parse %s as YAML" !!file;
        parse_yaml_rule_file ~is_target:true !!file
  in
  parse_generic_ast ?error_recovery file ast

(*****************************************************************************)
(* Main Entry point *)
(*****************************************************************************)

let parse_and_filter_invalid_rules file = parse_file ~error_recovery:true file
  [@@profiling]

let parse_xpattern xlang (str, tok) =
  let env =
    {
      id = Rule_ID.of_string "anon-pattern";
      languages = Rule.languages_of_xlang xlang;
      in_metavariable_pattern = false;
      path = [];
      options_key = None;
      options = None;
    }
  in
  PF.parse_rule_xpattern env (str, tok)

(*****************************************************************************)
(* Useful for tests *)
(*****************************************************************************)

let parse file =
  let xs, skipped = parse_file ~error_recovery:false file in
  assert (skipped =*= []);
  xs

(*****************************************************************************)
(* Valid rule filename checks *)
(*****************************************************************************)
(* Those functions could be in a separate file *)

(* alt: could define
 * type yaml_kind = YamlRule | YamlTest | YamlFixed | YamlOther
 *)
let is_test_yaml_file filepath =
  (* .test.yaml files are YAML target files rather than config files! *)
  let filepath = !!filepath in
  Filename.check_suffix filepath ".test.yaml"
  || Filename.check_suffix filepath ".test.yml"
  || Filename.check_suffix filepath ".test.fixed.yaml"
  || Filename.check_suffix filepath ".test.fixed.yml"

let is_valid_rule_filename filename =
  match File_type.file_type_of_file filename with
  (* ".yml" or ".yaml" *)
  | FT.Config FT.Yaml -> not (is_test_yaml_file filename)
  (* old: we were allowing Jsonnet before, but better to skip
   * them for now to avoid adding a jsonnet dependency in our docker/CI
   * FT.Config (FT.Json FT.Jsonnet) when not unit_testing -> true
   *)
  | _else_ -> false
