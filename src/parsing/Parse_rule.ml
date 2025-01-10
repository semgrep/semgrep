(* Yoann Padioleau, Emma Jin
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
open Fpath_.Operators
module J = JSON
module FT = File_type
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module G = AST_generic
module Set = Set_
module MV = Metavariable
open Parse_rule_helpers
module H = Parse_rule_helpers

(* alt: use a separate Logs src "semgrep.parsing.rule" *)
module Log = Log_parsing.Log

let ( >>= ) = Result.bind

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema for a rule in rule_schema_v1.yaml (and
 * also now in rule_schema_v2.atd for the v2 syntax).
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

let parse_language ~id ((s, t) as _lang) : (Lang.t, Rule_error.t) result =
  match Lang.of_string_opt s with
  | None ->
      Error
        (Rule_error.mk_error ~rule_id:id
           (InvalidRule (InvalidLanguage s, id, t)))
  | Some lang -> (
      match Parsing_plugin.check_if_missing lang with
      | Ok () -> Ok lang
      | Error msg ->
          Error
            (Rule_error.mk_error ~rule_id:id
               (InvalidRule (MissingPlugin msg, id, t))))

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
   Analyzer.of_string which allows "spacegrep" and "aliengrep" so
   this might lead to inconsistencies as here we allow just "generic".
*)
let parse_languages ~id (options : Rule_options_t.t) langs :
    (Target_selector.t option * Analyzer.t, Rule_error.t) result =
  match langs with
  | [ (("none" | "regex"), _t) ] -> Ok (None, LRegex)
  | [ ("generic", _t) ] -> (
      (* The generic mode now uses one of two possible engines.
         For now, we keep the name "generic" for both and use an option
         to choose one engine or the other. *)
      match options.generic_engine with
      | `Spacegrep -> Ok (None, LSpacegrep)
      | `Aliengrep -> Ok (None, LAliengrep))
  | xs -> (
      let rule_id, _ = id in
      let/ langs =
        xs |> List_.map (parse_language ~id:rule_id) |> Base.Result.all
      in
      match langs with
      | [] -> error rule_id (snd id) "we need at least one language"
      | x :: xs -> Ok (Some langs, Analyzer.L (x, xs)))

let parse_severity ~id (s, t) : (Rule.severity, Rule_error.t) result =
  match s with
  (* LATER: should put a deprecated notice at some point for all of those
   * coupling: update the error below when we fully switch to Low/Medium/...
   *)
  | "ERROR" -> Ok `Error
  | "WARNING" -> Ok `Warning
  | "INVENTORY" -> Ok `Inventory
  | "EXPERIMENT" -> Ok `Experiment
  (* since Semgrep 1.72.0 *)
  | "CRITICAL" -> Ok `Critical
  | "HIGH" -> Ok `High
  | "MEDIUM" -> Ok `Medium
  | "LOW" -> Ok `Low
  (* generic placeholder *)
  | "INFO" -> Ok `Info
  | s -> error id t (spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s)

(*****************************************************************************)
(* Parsers for extra (metavar-xxx:, fix:, etc.) *)
(*****************************************************************************)

let parse_fix_regex (env : env) (key : key) fields =
  let/ fix_regex_dict = parse_dict env key fields in
  let/ regexp = take_key fix_regex_dict env parse_string_wrap "regex" in
  let/ regexp = parse_regexp env regexp in
  (* TODO? should we String.trim for consistency with fix: ? *)
  let/ replacement = take_key fix_regex_dict env parse_string "replacement" in
  let/ count = take_opt fix_regex_dict env parse_int_strict "count" in
  Ok Rule.{ regexp; count; replacement }

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
  let/ paths_dict = parse_dict env key value in
  (* TODO: should imitate parse_string_wrap_list *)
  let parse_glob_list env (key : key) e =
    let extract_string env = function
      | { G.e = G.L (String (_, (value, _), _)); _ } -> (
          try Ok (value, Glob.Parse.parse_string value) with
          | Glob.Lexer.Syntax_error _ ->
              error_at_key env.id key ("Invalid glob for " ^ fst key))
      | _ ->
          error_at_key env.id key
            ("Expected all values in the list to be globs for " ^ fst key)
    in
    parse_list env key extract_string e
  in
  let/ inc_opt = take_opt paths_dict env parse_glob_list "include" in
  let/ exc_opt = take_opt paths_dict env parse_glob_list "exclude" in
  (* alt: we could use H.warn_if_remaining_unparsed_fields() but better to
   * raise an error for now to be compatible with pysemgrep.
   *)
  if Hashtbl.length paths_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)"
  else
    Ok
      {
        R.require = List_.optlist_to_list inc_opt;
        exclude = List_.optlist_to_list exc_opt;
      }

let parse_options rule_id (key : key) value =
  let/ s = generic_to_json rule_id key value |> Result.map J.string_of_json in
  let options =
    Common.save_excursion Atdgen_runtime.Util.Json.unknown_field_handler
      (fun _src_loc field_name ->
        (* for forward compatibility, better to not raise an exn and just
         * ignore the new fields.
         * old: raise (InvalidYamlException (spf "unknown opt: %s" field_name))
         *)
        (* nosemgrep: no-logs-in-library *)
        Logs.warn (fun m -> m "unknown rule option: %s" field_name))
      (fun () -> Rule_options_j.t_of_string s)
  in
  Ok (options, Some key)

(*****************************************************************************)
(* Parsers for taint *)
(*****************************************************************************)

let parse_by_side_effect env (key : key) x =
  match x.G.e with
  | G.L (String (_, ("true", _), _)) -> Ok R.Yes
  | G.L (String (_, ("false", _), _)) -> Ok R.No
  | G.L (String (_, ("only", _), _)) -> Ok R.Only
  | G.L (Bool (b, _)) -> Ok (if b then R.Yes else R.No)
  | _x -> error_at_key env.id key (spf "parse_by_side_effect for %s" (fst key))

let requires_expr_to_precondition env key e =
  let invalid_requires () =
    error_at_key env.id key
      "Invalid `requires' expression, it must be a Python Boolean expression \
       over labels (any valid Python identifier) using operators `not', `or' \
       and `and'."
  in
  let rec expr_to_precondition e =
    match e.G.e with
    | G.L (G.Bool (v, _)) -> Ok (R.PBool v)
    | G.N (G.Id ((str, _), _)) when Mvar.is_metavar_name str ->
        error_at_key env.id key
          ("Invalid `requires' expression, metavariables cannot be used as \
            labels: " ^ str)
    | G.N (G.Id ((str, _), _)) -> Ok (R.PLabel str)
    | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg e1 ], _)) ->
        let/ precond = expr_to_precondition e1 in
        Ok (R.PNot precond)
    | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
        let/ precond = args_to_precondition args in
        match (op, precond) with
        | G.And, xs -> Ok (R.PAnd xs)
        | G.Or, xs -> Ok (R.POr xs)
        | __else__ -> invalid_requires ())
    | __else__ -> invalid_requires ()
  and args_to_precondition args =
    match args with
    | [] -> Ok []
    | G.Arg e :: args' ->
        let/ x = expr_to_precondition e in
        let/ xs = args_to_precondition args' in
        Ok (x :: xs)
    | _ :: _args' -> invalid_requires ()
  in
  expr_to_precondition e

let parse_taint_requires env key x =
  let/ s = parse_string env key x in
  let/ e = parse_python_expression env key s in
  let range = AST_generic_helpers.range_of_any_opt (E e) in
  let/ precondition = requires_expr_to_precondition env key e in
  Ok { R.precondition; range }

(* TODO: can add a case where these take in only a single string *)
let parse_taint_source ~(is_old : bool) env (key : key) (value : G.expr) :
    (Rule.taint_source, Rule_error.t) result =
  let source_id = "source:" ^ String.concat ":" env.path in
  let parse_from_dict dict f =
    let/ source_exact =
      take_opt dict env parse_bool "exact"
      |> Result.map (Option.value ~default:false)
    in
    let/ source_by_side_effect =
      take_opt dict env parse_by_side_effect "by-side-effect"
      |> Result.map (Option.value ~default:R.No)
    in
    let/ source_control =
      take_opt dict env parse_bool "control"
      |> Result.map (Option.value ~default:false)
    in
    let/ label =
      take_opt dict env parse_string "label"
      |> Result.map (Option.value ~default:R.default_source_label)
    in
    let/ source_requires = take_opt dict env parse_taint_requires "requires" in
    let/ source_formula = f env dict in
    Ok
      {
        R.source_id;
        source_formula;
        source_exact;
        source_by_side_effect;
        source_control;
        label;
        source_requires;
      }
  in
  if is_old then
    let/ dict = parse_dict env key value in
    parse_from_dict dict Parse_rule_formula.parse_formula_old_from_dict
  else
    let/ source = parse_str_or_dict env value in
    match source with
    | Left value ->
        let/ formula = Parse_rule_formula.parse_rule_xpattern env value in
        let source_formula = R.f (R.P formula) in
        Ok
          {
            Rule.source_id;
            source_formula;
            source_exact = false;
            source_by_side_effect = R.No;
            source_control = false;
            label = R.default_source_label;
            source_requires = None;
          }
    | Right dict ->
        parse_from_dict dict Parse_rule_formula.parse_formula_from_dict

let parse_taint_propagator ~(is_old : bool) env (key : key) (value : G.expr) :
    (Rule.taint_propagator, Rule_error.t) result =
  let propagator_id = "propagator:" ^ String.concat ":" env.path in
  let f =
    if is_old then Parse_rule_formula.parse_formula_old_from_dict
    else Parse_rule_formula.parse_formula_from_dict
  in
  let parse_from_dict dict f =
    let/ propagator_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Result.map (Option.value ~default:true)
    in
    let/ from = take_key dict env parse_string_wrap "from" in
    let/ to_ = take_key dict env parse_string_wrap "to" in
    let/ propagator_requires =
      take_opt dict env parse_taint_requires "requires"
    in
    let/ propagator_label = take_opt dict env parse_string "label" in
    let/ propagator_replace_labels =
      take_opt dict env
        (fun env key v ->
          parse_listi env key (fun env v -> parse_string env key v) v)
        "replace-labels"
    in
    let/ propagator_formula = f env dict in
    Ok
      {
        R.propagator_id;
        propagator_formula;
        propagator_by_side_effect;
        from;
        to_;
        propagator_requires;
        propagator_replace_labels;
        propagator_label;
      }
  in
  let/ dict = parse_dict env key value in
  parse_from_dict dict f

let parse_taint_sanitizer ~(is_old : bool) env (key : key) (value : G.expr) =
  let sanitizer_id = "sanitizer:" ^ String.concat ":" env.path in
  let parse_from_dict dict f =
    let/ sanitizer_exact =
      take_opt dict env parse_bool "exact"
      |> Result.map (Option.value ~default:false)
    in
    let/ sanitizer_by_side_effect =
      take_opt dict env parse_bool "by-side-effect"
      |> Result.map (Option.value ~default:false)
    in
    let/ not_conflicting =
      take_opt dict env parse_bool
        (if is_old then "not_conflicting" else "not-conflicting")
      |> Result.map (Option.value ~default:false)
    in
    let/ sanitizer_formula = f env dict in
    Ok
      Rule.
        {
          sanitizer_id;
          sanitizer_formula;
          sanitizer_exact;
          sanitizer_by_side_effect;
          not_conflicting;
        }
  in
  if is_old then
    let/ dict = parse_dict env key value in
    parse_from_dict dict Parse_rule_formula.parse_formula_old_from_dict
  else
    let/ sanitizer = parse_str_or_dict env value in
    match sanitizer with
    | Left value ->
        let/ xpattern = Parse_rule_formula.parse_rule_xpattern env value in
        let sanitizer_formula = R.P xpattern |> R.f in
        Ok
          {
            sanitizer_id;
            sanitizer_formula;
            sanitizer_exact = false;
            sanitizer_by_side_effect = false;
            R.not_conflicting = false;
          }
    | Right dict ->
        parse_from_dict dict Parse_rule_formula.parse_formula_from_dict

let parse_taint_sink ~(is_old : bool) env (key : key) (value : G.expr) :
    (Rule.taint_sink, Rule_error.t) result =
  let sink_id = "sink:" ^ String.concat ":" env.path in
  let parse_from_dict dict f =
    let/ sink_requires = take_opt dict env parse_taint_requires "requires" in
    let/ sink_at_exit =
      take_opt dict env parse_bool "at-exit"
      |> Result.map (Option.value ~default:false)
    in
    let/ sink_exact =
      take_opt dict env parse_bool "exact"
      |> Result.map (Option.value ~default:true)
    in
    let/ sink_formula = f env dict in
    let sink_has_focus = Rule.is_formula_with_focus sink_formula in
    Ok
      Rule.
        {
          sink_id;
          sink_formula;
          sink_exact;
          sink_requires;
          sink_at_exit;
          sink_has_focus;
        }
  in
  if is_old then
    let/ dict = parse_dict env key value in
    parse_from_dict dict Parse_rule_formula.parse_formula_old_from_dict
  else
    let/ sink = parse_str_or_dict env value in
    match sink with
    | Left value ->
        let/ xpattern = Parse_rule_formula.parse_rule_xpattern env value in
        let sink_formula = R.P xpattern |> R.f in
        let sink_has_focus = Rule.is_formula_with_focus sink_formula in
        Ok
          Rule.
            {
              sink_id;
              sink_formula;
              sink_exact = true;
              sink_requires = None;
              sink_at_exit = false;
              sink_has_focus;
            }
    | Right dict ->
        parse_from_dict dict Parse_rule_formula.parse_formula_from_dict

let parse_taint_pattern env key (value : G.expr) =
  let/ dict = parse_dict env key value in
  let parse_specs parse_spec env key x =
    let/ items =
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x
    in
    Ok (snd key, items)
  in
  let/ sources =
    take_key dict env (parse_specs (parse_taint_source ~is_old:false)) "sources"
  in
  let/ propagators_opt =
    take_opt dict env
      (parse_specs (parse_taint_propagator ~is_old:false))
      "propagators"
  in
  let/ sanitizers_opt =
    take_opt dict env
      (parse_specs (parse_taint_sanitizer ~is_old:false))
      "sanitizers"
  in
  let/ sinks =
    take_key dict env (parse_specs (parse_taint_sink ~is_old:false)) "sinks"
  in
  Ok
    (`Taint
      Rule.
        {
          sources;
          propagators =
            (* optlist_to_list *)
            (match propagators_opt with
            | None -> []
            | Some (_, xs) -> xs);
          sanitizers = sanitizers_opt;
          sinks;
        })

(*****************************************************************************)
(* Parsers for extract mode *)
(*****************************************************************************)

(* TODO: factorize code with parse_languages *)
let parse_extract_dest ~id lang : (Analyzer.t, Rule_error.t) result =
  match lang with
  | ("none" | "regex"), _ -> Ok LRegex
  | ("generic" | "spacegrep"), _ -> Ok LSpacegrep
  | "aliengrep", _ -> Ok LAliengrep
  | lang ->
      let/ lang = parse_language ~id lang in
      Ok (Analyzer.L (lang, []))

let parse_extract_reduction ~id (s, t) =
  match s with
  | "concat" -> Ok R.Concat
  | "separate" -> Ok R.Separate
  | s ->
      error id t
        (spf "Bad extract reduction: %s (expected concat or separate)" s)

let parse_extract_transform ~id (s, t) =
  match s with
  | "no_transform" -> Ok R.NoTransform
  | "unquote_string" -> Ok R.Unquote
  | "concat_json_string_array" -> Ok R.ConcatJsonArray
  | s ->
      error id t
        (spf
           "Bad extract transform: %s (expected unquote_string or \
            concat_json_string_array)"
           s)

let parse_rules_to_run_with_extract env key value =
  let/ ruleids_dict = parse_dict env key value in
  let parse_rule_ids ruleids_dict env key =
    take_opt ruleids_dict env
      (parse_string_wrap_list (fun env key value ->
           Rule_ID.of_string_opt value
           |> Option.to_result
                ~none:
                  (Rule_error.mk_error ~rule_id:env.id
                     (InvalidRule
                        ( InvalidOther
                            ("Expected a valid rule ID. Instead got " ^ value),
                          env.id,
                          (* TODO: this isn't the best key, but we don't have
                             access to a better one. Seems like the helpers
                             could be refactored to compose much better. *)
                          snd key )))))
      key
  in
  let/ inc_opt = parse_rule_ids ruleids_dict env "include" in
  let/ exc_opt = parse_rule_ids ruleids_dict env "exclude" in
  (* to be compatible with pysemgrep *)
  if Hashtbl.length ruleids_dict.h > 0 then
    error_at_key env.id key
      "Additional properties are not allowed (only 'include' and 'exclude' are \
       supported)"
  else
    Ok
      Rule.
        {
          required_rules = List_.optlist_to_list inc_opt;
          excluded_rules = List_.optlist_to_list exc_opt;
        }

(*****************************************************************************)
(* Parsers used by step mode as well as general rules *)
(*****************************************************************************)

let parse_search_fields env rule_dict =
  let/ formula =
    take_opt rule_dict env
      (fun env _ expr -> Parse_rule_formula.parse_formula env expr)
      "match"
  in
  match formula with
  | Some formula -> Ok (`Search formula)
  | None ->
      let/ formula =
        Parse_rule_formula.parse_formula_old_from_dict env rule_dict
      in
      Ok (`Search formula)

let parse_taint_fields env rule_dict =
  let parse_specs parse_spec env key x =
    let/ items =
      parse_listi env key
        (fun env -> parse_spec env (fst key ^ "list item", snd key))
        x
    in
    Ok (snd key, items)
  in
  match H.dict_take_opt rule_dict "taint" with
  | Some (key, value) -> parse_taint_pattern env key value
  | __else__ ->
      let/ sources =
        take_key rule_dict env
          (parse_specs (parse_taint_source ~is_old:true))
          "pattern-sources"
      in
      let/ propagators_opt =
        take_opt rule_dict env
          (parse_specs (parse_taint_propagator ~is_old:true))
          "pattern-propagators"
      in
      let/ sanitizers_opt =
        take_opt rule_dict env
          (parse_specs (parse_taint_sanitizer ~is_old:true))
          "pattern-sanitizers"
      in
      let/ sinks =
        take_key rule_dict env
          (parse_specs (parse_taint_sink ~is_old:true))
          "pattern-sinks"
      in
      Ok
        (`Taint
          Rule.
            {
              sources;
              propagators =
                (* optlist_to_list *)
                (match propagators_opt with
                | None -> []
                | Some (_, xs) -> xs);
              sanitizers = sanitizers_opt;
              sinks;
            })

(*****************************************************************************)
(* Parsers for step mode *)
(*****************************************************************************)

let parse_step_fields env key (value : G.expr) : (R.step, Rule_error.t) result =
  let/ rd = parse_dict env key value in
  let/ languages = take_no_env rd parse_string_wrap_list_no_env "languages" in
  (* No id, so error at the steps key
     TODO error earlier *)
  let rule_options =
    (* TODO: this is annoying and refers to the global options which may be
       incorrect anyway -> support an 'options' field next to 'languages'
       in the step object? *)
    Option.value env.options ~default:Rule_options.default
  in
  let step_id_str, tok = key in
  let id =
    ( Rule_ID.of_string_exn (* TODO: is this really a rule ID? *) step_id_str,
      tok )
  in
  let/ step_selector, step_analyzer =
    parse_languages ~id rule_options languages
  in
  let env = { env with target_analyzer = step_analyzer } in
  let/ step_paths = take_opt rd env parse_paths "paths" in

  (* TODO: factorize with parse_mode *)
  let/ mode_opt = take_opt rd env parse_string_wrap "mode" in
  let has_taint_key = Option.is_some (Hashtbl.find_opt rd.h "taint") in
  let/ step_mode =
    match (mode_opt, has_taint_key) with
    | None, false
    | Some ("search", _), false ->
        parse_search_fields env rd
    | _, true
    | Some ("taint", _), _ ->
        parse_taint_fields env rd
    | Some key, _ ->
        error_at_key env.id key
          (spf
             "Unexpected value for mode, should be 'search' or 'taint', not %s"
             (fst key))
  in
  Ok Rule.{ step_selector; step_analyzer; step_paths; step_mode }

let parse_steps env key (value : G.expr) : (R.step list, Rule_error.t) result =
  let parse_step step = parse_step_fields env key step in
  match value.G.e with
  | G.Container (Array, (_, xs, _)) ->
      List_.map parse_step xs |> Base.Result.all
  | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)

(*****************************************************************************)
(* Parsers for secrets mode *)
(*****************************************************************************)

let parse_validity env key x : (Rule.validation_state, Rule_error.t) result =
  match x.G.e with
  | G.L (String (_, ("valid", _), _)) -> Ok `Confirmed_valid
  | G.L (String (_, ("invalid", _), _)) -> Ok `Confirmed_invalid
  | _x -> error_at_key env.id key (spf "parse_validity for %s" (fst key))

let parse_http_request env key value : (Rule.request, Rule_error.t) result =
  let/ req = parse_dict env key value in
  let/ url = take_key req env parse_string "url" in
  let/ meth = take_key req env parse_http_method "method" in
  let/ headers = take_key req env parse_dict "headers" in
  let/ headers =
    headers |> fun { h; _ } ->
    Hashtbl.fold
      (fun name value lst ->
        let/ lst = lst in
        let/ value = parse_string env (fst value) (snd value) in
        Ok ({ Rule.name; value } :: lst))
      h (Ok [])
  in
  let/ body = take_opt req env parse_string "body" in
  let/ auth = take_opt req env parse_auth "auth" in
  Ok Rule.{ url; meth; headers; body; auth }

let parse_http_matcher_clause key env value :
    (Rule.http_match_clause, Rule_error.t) result =
  let/ clause = parse_dict env key value in
  let/ status_code = take_opt clause env parse_int "status-code" in
  let/ headers =
    take_opt clause env
      (fun env key ->
        parse_list env key (fun env x ->
            let/ hd = parse_dict env key x in
            let/ name = take_key hd env parse_string "name" in
            let/ value = take_key hd env parse_string "value" in
            Ok Rule.{ name; value }))
      "headers"
  in
  let/ content =
    match take_opt clause env parse_dict "content" with
    | Ok (Some content) ->
        let/ formula =
          Parse_rule_formula.parse_formula_old_from_dict env content
        in
        let/ language =
          take_opt content env parse_string "language"
          |> Result.map
               (Option.map
                  (Analyzer.of_string ~rule_id:(Rule_ID.to_string env.id)))
          |> Result.map (Option.value ~default:Analyzer.LAliengrep)
        in
        Ok (Some (formula, language))
    | Ok None -> Ok None
    | Error e -> Error e
  in
  match (status_code, headers, content) with
  | None, None, None ->
      error_at_key env.id key
        "A matcher must have at least one of status-code, headers, or content"
  | _ ->
      Ok
        Rule.
          { status_code; headers = Option.value ~default:[] headers; content }

let parse_http_matcher key env value : (Rule.http_matcher, Rule_error.t) result
    =
  let/ matcher = parse_dict env key value in
  let/ match_conditions =
    take_key matcher env
      (fun env key -> parse_list env key (parse_http_matcher_clause key))
      "match"
  in
  let/ result = take_key matcher env parse_dict "result" in
  let/ validity = take_key result env parse_validity "validity" in
  let/ message = take_opt result env parse_string "message" in
  let/ severity =
    match take_opt result env parse_string_wrap "severity" with
    | Ok (Some x) ->
        let/ sev = parse_severity ~id:env.id x in
        Ok (Some sev)
    | Ok None -> Ok None
    | Error e -> Error e
  in
  let/ metadata = take_opt_no_env result (generic_to_json env.id) "metadata" in
  Ok Rule.{ match_conditions; validity; message; severity; metadata }

let parse_http_response env key value :
    (Rule.http_matcher list, Rule_error.t) result =
  parse_list env key (parse_http_matcher key) value

let parse_http_validator env key value : (Rule.validator, Rule_error.t) result =
  let/ validator_dict = parse_dict env key value in
  let/ request = take_key validator_dict env parse_http_request "request" in
  let/ response = take_key validator_dict env parse_http_response "response" in
  Ok (Rule.HTTP { request; response })

let parse_aws_request env key value : (Rule.aws_request, Rule_error.t) result =
  let/ request_dict = parse_dict env key value in
  let/ secret_access_key =
    take_key request_dict env parse_string "secret_access_key"
  in
  let/ access_key_id = take_key request_dict env parse_string "access_key_id" in
  let/ region = take_key request_dict env parse_string "region" in
  let/ session_token = take_opt request_dict env parse_string "session_token" in
  Ok Rule.{ secret_access_key; access_key_id; region; session_token }

let parse_aws_validator env key value : (Rule.validator, Rule_error.t) result =
  let/ validator_dict = parse_dict env key value in
  let/ request = take_key validator_dict env parse_aws_request "request" in
  let/ response = take_key validator_dict env parse_http_response "response" in
  Ok (Rule.AWS { request; response })

let parse_validator key env value =
  let/ dict = parse_dict env key value in
  match List_.find_some_opt (Hashtbl.find_opt dict.h) [ "http"; "aws" ] with
  | Some (("http", _), value) -> parse_http_validator env key value
  | Some (("aws", _), value) -> parse_aws_validator env key value
  | Some _
  | None ->
      (* The [Some _] case here should be impossible *)
      error_at_key env.id key
        ("No recognized validator, must be one of ['http', 'aws'] at " ^ fst key)

let parse_validators env key value =
  parse_list env key (parse_validator key) value

(*****************************************************************************)
(* Parsers for Supply chain *)
(*****************************************************************************)

let parse_ecosystem env key value =
  match value.G.e with
  | G.L (String (_, (_ecosystem, _), _)) ->
      Ok `Npm
      (* | _ -> error_at_key env.id key ("Unknown ecosystem: " ^ ecosystem)) *)
  | _ -> error_at_key env.id key "Non-string data for ecosystem?"

let parse_dependency_pattern key env value :
    (SCA_pattern.t, Rule_error.t) result =
  let/ rd = parse_dict env key value in
  let/ ecosystem = take_key rd env parse_ecosystem "namespace" in
  let/ package_name = take_key rd env parse_string "package" in
  let/ version_str = take_key rd env parse_string "version" in
  let/ version_constraints =
    try Ok (Parse_SCA_version.parse_constraints version_str) with
    | Parse_SCA_version.Error error_str ->
        error_at_key env.id key
          (spf "bad version constraint format for %s, error = %s" version_str
             error_str)
  in
  Ok SCA_pattern.{ ecosystem; package_name; version_constraints }

let parse_dependency_formula env key value :
    (R.sca_dependency_formula, Rule_error.t) result =
  let/ rd = parse_dict env key value in
  if Hashtbl.mem rd.h "depends-on-either" then
    take_key rd env
      (fun env key -> parse_list env key (parse_dependency_pattern key))
      "depends-on-either"
  else
    let/ dependency_pattern = parse_dependency_pattern key env value in
    Ok [ dependency_pattern ]

(*****************************************************************************)
(* Parse the whole thing  *)
(*****************************************************************************)

(* dispatch depending on the "mode" of the rule *)
let parse_mode env mode_opt dep_fml_opt (rule_dict : dict) :
    (R.mode, Rule_error.t) result =
  (* We do this because we should only assume that we have a search mode rule
     if there is not a `taint` key present in the rule dict.
  *)
  let has_taint_key = Option.is_some (Hashtbl.find_opt rule_dict.h "taint") in
  (* TODO? maybe have also has_extract_key, has_steps_key, has_secrets_key *)
  match (mode_opt, has_taint_key, dep_fml_opt) with
  (* no mode:, no taint:, default to look for match: *)
  | None, false, None
  | Some ("search", _), false, _ ->
      parse_search_fields env rule_dict
  | None, true, _
  | Some ("taint", _), _, _ ->
      parse_taint_fields env rule_dict
  (* TODO: for extract in syntax v2 (see rule_schema_v2.atd)
   * | None, _, true (has_extract_key) ->
   *     parse_extract_fields ...
   *)
  | Some ("extract", _), _, _ ->
      let/ formula =
        Parse_rule_formula.parse_formula_old_from_dict env rule_dict
      in
      let/ dst_lang =
        take_key rule_dict env parse_string_wrap "dest-language"
        |> fun lang_tok -> Result.bind lang_tok (parse_extract_dest ~id:env.id)
      in
      (* TODO: determine fmt---string with interpolated metavars? *)
      let/ extract = take_key rule_dict env parse_string "extract" in
      let/ extract_rule_ids =
        take_opt rule_dict env parse_rules_to_run_with_extract "dest-rules"
      in
      let/ transform =
        take_opt rule_dict env parse_string_wrap "transform" >>= function
        | Some x -> parse_extract_transform ~id:env.id x
        | None -> Ok R.NoTransform
      in
      let/ reduce =
        take_opt rule_dict env parse_string_wrap "reduce" >>= function
        | Some x -> parse_extract_reduction ~id:env.id x
        | None -> Ok R.Separate
      in
      Ok
        (`Extract
          Rule.
            { formula; dst_lang; extract_rule_ids; extract; reduce; transform })
  (* TODO? should we use "mode: steps" instead? *)
  | Some ("step", _), _, _ ->
      let/ steps = take_key rule_dict env parse_steps "steps" in
      Ok (`Steps steps)
  (* SCA Doesn't require patterns. Just trying to be permissive here
     for now. If the SCA rule is a valid search rule then we go ahead
     and parse it as a search rule. Right now the dependency_formula
     is repeated as an optional field in the rule too.*)
  | None, false, Some fml -> (
      match parse_search_fields env rule_dict with
      | Error { kind = InvalidRule _; _ } -> Ok (`SCA fml)
      | x -> x)
  (* unknown mode *)
  | Some key, _, _ ->
      error_at_key env.id key
        (spf
           "Unexpected value for mode, should be 'search', 'taint', 'extract', \
            or 'step', not %s"
           (fst key))

let parse_version key value =
  let/ str, tok = parse_string_wrap_no_env key value in
  match Semver.of_string str with
  | Some version -> Ok (version, tok)
  | None ->
      yaml_error_at_key key
        ("Expected a version of the form X.Y.Z for " ^ fst key)

let incompatible_version ?min_version ?max_version rule_id tok =
  Error
    (Rule_error.mk_error ~rule_id
       (InvalidRule
          ( IncompatibleRule (Version_info.version, (min_version, max_version)),
            rule_id,
            tok )))

let check_version_compatibility rule_id ~min_version ~max_version =
  let/ () =
    match min_version with
    | Some (mini, tok) when not (Semver.compare mini Version_info.version <= 0)
      ->
        incompatible_version ?min_version:(Some mini) rule_id tok
    | Some _
    | None ->
        Ok ()
  in
  let/ () =
    match max_version with
    | Some (maxi, tok) when not (Semver.compare Version_info.version maxi <= 0)
      ->
        incompatible_version ?max_version:(Some maxi) rule_id tok
    | Some _
    | None ->
        Ok ()
  in
  Ok ()

(* TODO: Unify how we differentiate which rules correspond to which
   products. This basically just copies the logic of
   semgrep/cli/src/semgrep/rule.py::Rule.product *)
let parse_product (metadata : J.t option)
    (dep_formula_opt : R.sca_dependency_formula option) :
    Semgrep_output_v1_t.product =
  match dep_formula_opt with
  | Some _ -> `SCA
  | None -> (
      match metadata with
      | Some metadata -> (
          match J.member "product" metadata with
          | Some (J.String "secrets") -> `Secrets
          (* TODO: Error if this isn't well formed...*)
          | _ -> `SAST)
      | _ -> `SAST)

let parse_one_rule ~rewrite_rule_ids (i : int) (rule : G.expr) :
    (Rule.t, Rule_error.t) result =
  (* TODO: explain the function arguments of parse_dict_no_env *)
  let/ rd = parse_dict_no_env "rules" rule in
  (* We need a rule ID early to produce useful error messages. *)
  let/ rule_id, tok = take_no_env rd parse_rule_id_no_env "id" in
  let rule_id : Rule_ID.t =
    match rewrite_rule_ids with
    | None -> rule_id
    | Some f -> f rule_id
  in
  let id = (rule_id, tok) in
  (* We need to check for version compatibility before attempting to interpret
     the rule. *)
  let/ min_version = take_opt_no_env rd parse_version "min-version" in
  let/ max_version = take_opt_no_env rd parse_version "max-version" in
  let/ () = check_version_compatibility rule_id ~min_version ~max_version in

  let/ languages_opt =
    take_opt_no_env rd parse_string_wrap_list_no_env "languages"
  in
  let/ languages =
    match languages_opt with
    | Some languages -> Ok languages
    (* TODO: join-mode does not have languages and is not recognized right now
     * by semgrep-core
     * TODO? steps-mode or rules using just pattern-regex could also skip
     * the languages section? (and use target selector instead)
     *)
    | None -> H.error rule_id tok "missing languages"
  in
  let/ options_opt, options_key =
    let/ options = take_opt_no_env rd (parse_options rule_id) "options" in
    match options with
    | None -> Ok (None, None)
    | Some (options, options_key) -> Ok (Some options, options_key)
  in
  let options = Option.value options_opt ~default:Rule_options.default in
  let/ target_selector, target_analyzer =
    parse_languages ~id options languages
  in
  let env =
    {
      id = rule_id;
      target_analyzer;
      in_metavariable_pattern = false;
      path = [ string_of_int i; "rules" ];
      options_key;
      options = options_opt;
    }
  in
  let/ mode_opt = take_opt rd env parse_string_wrap "mode" in
  let/ dep_formula_opt =
    take_opt rd env parse_dependency_formula "r2c-internal-project-depends-on"
  in
  (* this parses the search formula, or taint spec, or extract mode, etc. *)
  let/ mode = parse_mode env mode_opt dep_formula_opt rd in
  let/ metadata_opt = take_opt_no_env rd (generic_to_json rule_id) "metadata" in
  let product = parse_product metadata_opt dep_formula_opt in
  let/ message, severity =
    match mode with
    | `Extract _ -> Ok ("", `Info)
    | _ ->
        let/ message = take_key rd env parse_string "message" in
        let/ severity = take_key rd env parse_string_wrap "severity" in
        let/ severity = parse_severity ~id:env.id severity in
        Ok (message, severity)
  in
  let/ fix_opt = take_opt rd env parse_string "fix" in
  let/ fix_regex_opt = take_opt rd env parse_fix_regex "fix-regex" in
  let/ paths_opt = take_opt rd env parse_paths "paths" in
  let/ equivs_opt = take_opt rd env parse_equivalences "equivalences" in
  let/ validators_opt = take_opt rd env parse_validators "validators" in
  H.warn_if_remaining_unparsed_fields rule_id rd;
  Ok
    {
      R.id;
      min_version = Option.map fst min_version;
      max_version = Option.map fst max_version;
      message;
      target_selector;
      target_analyzer;
      severity;
      mode;
      product;
      (* optional fields *)
      metadata = metadata_opt;
      fix = fix_opt |> Option.map String.trim;
      fix_regexp = fix_regex_opt;
      paths = paths_opt;
      equivalences = equivs_opt;
      options = options_opt;
      validators = validators_opt;
      dependency_formula = dep_formula_opt;
    }

let parse_generic_ast ?(error_recovery = false) ?rewrite_rule_ids
    (file : Fpath.t) (ast : AST_generic.program) :
    (Rule_error.rules_and_invalid, Rule_error.t) result =
  let res =
    let/ rules =
      match ast with
      | [ { G.s = G.ExprStmt (e, _); _ } ] -> (
          let missing_rules_field () =
            let loc = Tok.first_loc_of_file file in
            yaml_error (Tok.tok_of_loc loc)
              "missing rules entry as top-level key"
          in
          match e.e with
          | Container (Dict, _) ->
              let/ root_dict = parse_dict_no_env "rule file" e in
              let/ rules =
                match dict_take_opt root_dict "rules" with
                | None -> missing_rules_field ()
                | Some (_key, rules) -> (
                    match rules.G.e with
                    | G.Container (G.Array, (_tok, rules, _r)) -> Ok rules
                    | _ ->
                        yaml_error_at_expr rules
                          "expected a list of rules following `rules:`")
              in
              let/ () = check_that_dict_is_empty root_dict in
              Ok rules
          (* it's also ok to not have the toplevel rules:, anyway we never
             * used another toplevel key
          *)
          | G.Container (G.Array, (_tok, rules, _r)) -> Ok rules
          | _ -> missing_rules_field ())
      | [] ->
          (* an empty rules file returns an empty list of rules *)
          Ok []
      | _ -> assert false
      (* yaml_to_generic should always return a ExprStmt *)
    in
    let/ xs =
      rules
      |> List_.mapi (fun i rule ->
             match parse_one_rule ~rewrite_rule_ids i rule with
             | Ok rule -> Ok (Either.Left rule)
             | Error { kind = InvalidRule ((kind, ruleid, _) as err); _ }
               when error_recovery || Rule_error.is_skippable_error kind ->
                 let s = Rule_error.string_of_invalid_rule_kind kind in
                 Log.warn (fun m ->
                     m "skipping rule %s, error = %s" (Rule_ID.to_string ruleid)
                       s);
                 Ok (Either.Right err)
             | Error err -> Error err)
      |> Base.Result.all
    in
    Ok (Either_.partition (fun x -> x) xs)
  in
  match res with
  | Error (err : Rule_error.t) -> Error (Rule_error.augment_with_file file err)
  | other -> other

(* We can't call just Yaml_to_generic.program below because when we parse
 * YAML Semgrep rules, we preprocess unicode characters differently.
 * We also need to translate Parse_info.Other_error exn in
 * (Rule.Err (Rule.InvalidYaml)) exn.
 * Note that we can't generate a Rule.Err in Yaml_to_generic directly
 * because we don't want parsing/other/ to depend on core/.
 *)
let parse_yaml_rule_file ~is_target (file : Fpath.t) =
  let str = UFile.read_file file in
  try Ok (Yaml_to_generic.parse_yaml_file ~is_target file str) with
  | Parsing_error.Other_error (s, t) ->
      Error (Rule_error.mk_error (InvalidYaml (s, t)))

let parse_file ?error_recovery ?rewrite_rule_ids file :
    (Rule.rules * Rule_error.invalid_rule list, Rule_error.t) result =
  let/ ast =
    (* coupling: Rule_file.is_valid_rule_filename *)
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
        Ok
          (Json_to_generic.program ~unescape_strings:true
             (Parse_json.parse_program file))
    | FT.Config FT.Jsonnet ->
        (* old: via external jsonnet program
           Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
               let cmd = spf "jsonnet -J vendor %s -o %s" !!file tmpfile in
               let n = Sys.command cmd in
               if n <> 0 then failwith (spf "error executing %s" cmd);
               let ast = Parse_json.parse_program tmpfile in
               Json_to_generic.program ~unescape_strings:true ast)
        *)
        let ast = Parse_jsonnet.parse_program file in
        (* Note that here we do not support registry-aware import;
         * those are defined in osemgrep/.../Rule_fetching.ml where
         * we use Network.get functions. Thus, semgrep-core -dump_rule
         * will not work with registry-aware import either.
         * Use osemgrep --dump-config instead.
         *)
        let core = Desugar_jsonnet.desugar_program file ast in
        let value_ = Eval_jsonnet.eval_program core in
        Ok (Manifest_jsonnet_to_AST_generic.manifest_value value_)
    | FT.Config FT.Yaml -> parse_yaml_rule_file ~is_target:true file
    | _ ->
        (* TODO: suspicious code duplication. The same error message
           occurs in Translate_rule.ml *)
        Log.err (fun m ->
            m
              "Wrong rule format, only JSON/YAML/JSONNET are valid. Trying to \
               parse %s as YAML"
              !!file);
        parse_yaml_rule_file ~is_target:true file
  in
  parse_generic_ast ?error_recovery ?rewrite_rule_ids file ast

(*****************************************************************************)
(* Main Entry point *)
(*****************************************************************************)

let parse_and_filter_invalid_rules ?rewrite_rule_ids (file : Fpath.t) :
    (Rule.rules * Rule_error.invalid_rule list, Rule_error.t) result =
  let/ rules, errors = parse_file ~error_recovery:true ?rewrite_rule_ids file in
  Log.debug (fun m ->
      m "Parse_rule.parse_and_filter_invalid_rules(%s) = " !!file);
  rules |> List.iter (fun r -> Log.debug (fun m -> m "%s" (Rule.show r)));
  Ok (rules, errors)
[@@profiling]

let parse_xpattern analyzer (str, tok) =
  let env =
    {
      id = Rule_ID.of_string_exn "anon-pattern";
      target_analyzer = analyzer;
      in_metavariable_pattern = false;
      path = [];
      options_key = None;
      options = None;
    }
  in
  Parse_rule_formula.parse_rule_xpattern env (str, tok)

let parse_fake_xpattern analyzer str =
  let fk = Tok.unsafe_fake_tok "" in
  parse_xpattern analyzer (str, fk)

(*****************************************************************************)
(* Useful for tests *)
(*****************************************************************************)

let parse (file : Fpath.t) : (Rule.rules, Rule_error.t) result =
  let/ xs, _skipped = parse_file ~error_recovery:false file in
  (* The skipped rules include Apex rules and other rules that are always
     skippable. *)
  Ok xs
