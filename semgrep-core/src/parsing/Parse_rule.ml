(*s: semgrep/parsing/Parse_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: pad/r2c copyright *)
open Common
module J = JSON
module FT = File_type
module R = Rule
module E = Parse_mini_rule
module H = Parse_mini_rule
module G = AST_generic
module PI = Parse_info
module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema in rule_schema.yaml
 *
 * Parsing generic dictionaries creates a mutable Hashtbl and consumes the
 * fields as they are processed
 *
 * TODO:
 *  - use the new position-aware YAML parser to get position information (for
 *    precise error location) by using an AST_generic expression instead of
 *    JSON.t (at the same time, in the long term we want
 *    to use JSON and jsonnet, so we might get anyway a line location
 *    in a generated file, so maybe better to give error location by
 *    describing the line and what is wrong with it?).
 *  - Move the H.xxx here and get rid of Parse_mini_rule.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env = { id : string; languages : R.xlang; path : string list }

let error s = raise (E.InvalidYamlException s)

let generic_to_json key ast =
  let rec generic_to_json_help = function
    | G.L (Null _) -> J.Null
    | G.L (Bool (b, _)) -> J.Bool b
    | G.L (Float (Some f, _)) -> J.Float f
    | G.L (Int (Some i, _)) -> J.Int i
    | G.L (String (s, _)) -> J.String s
    | G.Container (Array, (_, xs, _)) ->
        J.Array (xs |> List.map generic_to_json_help)
    | G.Container (Dict, (_, xs, _)) ->
        J.Object
          (xs
          |> List.map (fun x ->
                 match x with
                 | G.Tuple (_, [ L (String (k, _)); v ], _) ->
                     (k, generic_to_json_help v)
                 | _ -> error ("Expected key value pair in " ^ key ^ " dict")))
    | _ -> error "Unexpected generic representation of yaml"
  in
  generic_to_json_help ast

let opt_to_list = function None -> [] | Some xs -> xs

(*****************************************************************************)
(* Dict helper methods *)
(*****************************************************************************)

let yaml_to_dict name rule =
  match rule with
  | G.Container (Dict, (_, fields, _)) ->
      let dict = Hashtbl.create 10 in
      fields
      |> List.iter (fun field ->
             match field with
             | G.Tuple (_, [ L (String (key, _)); value ], _) ->
                 Hashtbl.add dict key value
             | _ -> error "Not a valid key value pair");
      dict
  | _ -> error ("each " ^ name ^ " should be a dictionary of fields")

(* Mutates the Hashtbl! *)
let take dict f key =
  match Hashtbl.find_opt dict key with
  | Some value ->
      let value = f key value in
      Hashtbl.remove dict key;
      value
  | None -> error ("Missing required field " ^ key)

(* Mutates the Hashtbl! *)
let take_opt dict f key = Common.map_opt (f key) (Hashtbl.find_opt dict key)

(*****************************************************************************)
(* Sub parsers basic types *)
(*****************************************************************************)

let parse_string key = function
  | G.L (String (value, _)) -> value
  | G.N (Id ((value, _), _)) -> value
  | _ -> error ("Expected a string value for " ^ key)

let parse_list key f = function
  | G.Container (Array, (_, xs, _)) -> List.map f xs
  | _ -> error ("Expected a list for " ^ key)

let parse_string_list key e =
  let extract_string = function
    | G.L (String (value, _)) -> value
    | _ -> error ("Expected all values in the list to be strings for " ^ key)
  in
  parse_list key extract_string e

let parse_listi key f = function
  | G.Container (Array, (_, xs, _)) -> List.mapi f xs
  | _ -> error ("Expected a list for " ^ key)

let parse_bool key = function
  | G.L (String ("true", _)) -> true
  | G.L (String ("false", _)) -> false
  | G.L (Bool (b, _)) -> b
  | x ->
      pr2_gen x;
      error (spf "parse_bool for %s" key)

let parse_int key = function
  | G.L (Int (Some i, _)) -> i
  | G.L (String (s, _)) -> (
      try int_of_string s with Failure _ -> error (spf "parse_int for %s" key))
  | G.L (Float (Some f, _)) ->
      let i = int_of_float f in
      if float_of_int i = f then i
      else (
        pr2_gen f;
        error "not an int")
  | x ->
      pr2_gen x;
      error (spf "parse_int for %s" key)

(*****************************************************************************)
(* Sub parsers extra *)
(*****************************************************************************)

let pcre_error_to_string s exn =
  let message =
    match exn with
    | Pcre.Partial -> "String only matched the pattern partially"
    | BadPartial ->
        "Pattern contains items that cannot be used together with partial \
         matching."
    | BadPattern (msg, pos) -> spf "%s at position %d" msg pos
    | BadUTF8 -> "UTF8 string being matched is invalid"
    | BadUTF8Offset ->
        "Gets raised when a UTF8 string being matched with offset is invalid."
    | MatchLimit ->
        "Maximum allowed number of match attempts with\n\
        \                      backtracking or recursion is reached during \
         matching."
    | RecursionLimit -> "Recursion limit reached"
    | WorkspaceSize -> "Workspace array size reached"
    | InternalError msg -> spf "Internal error: %s" msg
  in
  spf "'%s': %s" s message

let parse_metavar_cond s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error "not an expression"
  with exn -> raise exn

let parse_regexp env s =
  try (s, Pcre.regexp s)
  with Pcre.Error exn ->
    raise (E.InvalidRegexpException (env.id, pcre_error_to_string s exn))

let parse_fix_regex env key fields =
  let fix_regex_dict = yaml_to_dict key fields in
  let regex, replacement, count_opt =
    ( take fix_regex_dict parse_string "regex",
      take fix_regex_dict parse_string "replacement",
      take_opt fix_regex_dict parse_int "count" )
  in
  (parse_regexp env regex, count_opt, replacement)

let parse_equivalences key value =
  let parse_equivalence equiv =
    match equiv with
    | G.Container
        ( Dict,
          (_, [ Tuple (_, [ L (String ("equivalence", _)); value ], _) ], _) )
      ->
        parse_string "equivalence" value
    | _ -> error "Expected `equivalence: $X` for each equivalences list item"
  in
  parse_list key parse_equivalence value

let parse_paths key value =
  let paths_dict = yaml_to_dict key value in
  let inc_opt, exc_opt =
    ( take_opt paths_dict parse_string_list "include",
      take_opt paths_dict parse_string_list "exclude" )
  in
  { R.include_ = opt_to_list inc_opt; exclude = opt_to_list exc_opt }

let parse_options key value =
  let s = J.string_of_json (generic_to_json key value) in
  Common.save_excursion Atdgen_runtime.Util.Json.unknown_field_handler
    (fun _src_loc field_name ->
      (* for forward compatibility, better to not raise an exn and just
       * ignore the new fields.
       * TODO: we should use a warning/logging infra to report
       * this in the JSON to the semgrep wrapper and user.
       *)
      (*raise (E.InvalidYamlException (spf "unknown option: %s" field_name))*)
      pr2 (spf "WARNING: unknown option: %s" field_name))
    (fun () -> Config_semgrep_j.t_of_string s)

(*****************************************************************************)
(* Sub parsers patterns and formulas *)
(*****************************************************************************)

let parse_pattern env e =
  let s =
    match e with
    | G.L (String (value, _)) -> value
    | G.N (Id ((value, _), _)) -> value
    | _ -> error ("Expected a string value for " ^ env.id)
  in
  let start, end_ = Visitor_AST.range_of_any (G.E e) in
  let _s_range =
    (PI.mk_info_of_loc start, PI.mk_info_of_loc end_)
    (* TODO put in *)
  in
  match env.languages with
  | R.L (lang, _) ->
      R.mk_xpat (Sem (H.parse_pattern ~id:env.id ~lang s, lang)) s
  | R.LRegex -> failwith "you should not use real pattern with language = none"
  | R.LGeneric -> (
      let src = Spacegrep.Src_file.of_string s in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> R.mk_xpat (Spacegrep ast) s
      | Error err ->
          raise (H.InvalidPatternException (env.id, s, "generic", err.msg)))

let find_formula_old rule_dict : string * G.expr =
  let find key = (key, Hashtbl.find_opt rule_dict key) in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex",
      find "pattern-comby" )
  with
  | (_, None), (_, None), (_, None), (_, None), (_, None) ->
      error
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, `pattern-comby` to be present"
  | (key, Some value), (_, None), (_, None), (_, None), (_, None)
  | (_, None), (key, Some value), (_, None), (_, None), (_, None)
  | (_, None), (_, None), (key, Some value), (_, None), (_, None)
  | (_, None), (_, None), (_, None), (key, Some value), (_, None)
  | (_, None), (_, None), (_, None), (_, None), (key, Some value) ->
      (key, value)
  | _ ->
      error
        "Expected only one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, or `pattern-comby`"

let rec parse_formula (env : env) (rule_dict : (string, G.expr) Hashtbl.t) :
    R.pformula =
  match Hashtbl.find_opt rule_dict "match" with
  | Some v -> R.New (parse_formula_new env v)
  | None -> R.Old (parse_formula_old env (find_formula_old rule_dict))

and parse_formula_old env ((key, value) : string * G.expr) : R.formula_old =
  let env = { env with path = key :: env.path } in
  let get_pattern str_e = parse_pattern env str_e in
  let get_nested_formula i x =
    let env = { env with path = string_of_int i :: env.path } in
    match x with
    | G.Container
        (Dict, (_, [ Tuple (_, [ L (String (key, _)); value ], _) ], _)) ->
        parse_formula_old env (key, value)
    | _ ->
        pr2_gen x;
        error "Wrong parse_formula fields"
  in
  match (key, value) with
  | "pattern", s -> R.Pat (get_pattern s)
  | "pattern-not", s -> R.PatNot (get_pattern s)
  | "pattern-inside", s -> R.PatInside (get_pattern s)
  | "pattern-not-inside", s -> R.PatNotInside (get_pattern s)
  | "pattern-either", xs -> R.PatEither (parse_listi key get_nested_formula xs)
  | "patterns", xs -> R.Patterns (parse_listi key get_nested_formula xs)
  | "pattern-regex", s ->
      let s = parse_string key s in
      let xpat = R.mk_xpat (Regexp (parse_regexp env s)) s in
      R.Pat xpat
  | "pattern-not-regex", s ->
      let s = parse_string key s in
      let xpat = R.mk_xpat (Regexp (parse_regexp env s)) s in
      R.PatNot xpat
  | "pattern-comby", s ->
      let s = parse_string key s in
      let xpat = R.mk_xpat (Comby s) s in
      R.Pat xpat
  | "metavariable-regex", _
  | "metavariable-pattern", _
  | "metavariable-comparison", _
  | "pattern-where-python", _ ->
      R.PatExtra (parse_extra env key value)
  | _x -> error "unimplemented"

(* let extra = parse_extra env x in
   R.PatExtra extra *)
and parse_formula_new env (x : G.expr) : R.formula =
  match x with
  | G.Container (Dict, (_, [ Tuple (_, [ L (String (key, _)); value ], _) ], _))
    -> (
      match key with
      | "and" -> R.And (parse_list key (parse_formula_new env) value)
      | "or" -> R.Or (parse_list key (parse_formula_new env) value)
      | "not" -> R.Not (parse_formula_new env value)
      | "inside" -> R.Leaf (R.P (parse_pattern env value, Some Inside))
      | "regex" ->
          let s = parse_string key value in
          let xpat = R.mk_xpat (R.Regexp (parse_regexp env s)) s in
          R.Leaf (R.P (xpat, None))
      | "comby" ->
          let s = parse_string key value in
          let xpat = R.mk_xpat (R.Comby s) s in
          R.Leaf (R.P (xpat, None))
      | "where" ->
          let s = parse_string key value in
          R.Leaf (R.MetavarCond (R.CondEval (parse_metavar_cond s)))
      | "metavariable_regex" -> (
          match value with
          | G.Container (Array, (_, [ mvar; re ], _)) ->
              let mvar = parse_string key mvar in
              let re = parse_string key re in
              R.Leaf (R.MetavarCond (R.CondRegexp (mvar, parse_regexp env re)))
          | _ -> error "Expected a metavariable and regex")
      | _ -> error ("Invalid key for formula_new " ^ key))
  | _ -> R.Leaf (R.P (parse_pattern env x, None))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra env key value : Rule.extra =
  match key with
  | "metavariable-regex" ->
      let mv_regex_dict = yaml_to_dict key value in
      let metavar, regexp =
        ( take mv_regex_dict parse_string "metavariable",
          take mv_regex_dict parse_string "regex" )
      in
      R.MetavarRegexp (metavar, parse_regexp env regexp)
  | "metavariable-pattern" ->
      let mv_pattern_dict = yaml_to_dict key value in
      let metavar = take mv_pattern_dict parse_string "metavariable" in
      let env', opt_xlang =
        match take_opt mv_pattern_dict parse_string "language" with
        | Some s ->
            let xlang = R.xlang_of_string ~id:(Some env.id) s in
            let env' =
              {
                id = env.id;
                languages = xlang;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let pformula = parse_formula env' mv_pattern_dict in
      let formula = R.formula_of_pformula pformula in
      R.MetavarPattern (metavar, opt_xlang, formula)
  | "metavariable-comparison" ->
      let mv_comparison_dict = yaml_to_dict key value in
      let metavariable, comparison, strip, base =
        ( take mv_comparison_dict parse_string "metavariable",
          take mv_comparison_dict parse_string "comparison",
          take_opt mv_comparison_dict parse_bool "strip",
          take_opt mv_comparison_dict parse_int "base" )
      in
      let comparison = parse_metavar_cond comparison in
      R.MetavarComparison { R.metavariable; comparison; strip; base }
  | "pattern-where-python" ->
      R.PatWherePython (parse_string "pattern-where-python" value)
  | x ->
      pr2_gen x;
      error "wrong parse_extra fields"

let parse_languages ~id langs =
  match langs with
  | [ ("none" | "regex") ] -> R.LRegex
  | [ "generic" ] -> R.LGeneric
  | xs -> (
      let languages =
        xs
        |> List.map (function s ->
               (match Lang.lang_of_string_opt s with
               | None ->
                   raise
                     (E.InvalidLanguageException
                        (id, spf "unsupported language: %s" s))
               | Some l -> l))
      in
      match languages with
      | [] ->
          raise (E.InvalidRuleException (id, "we need at least one language"))
      | x :: xs -> R.L (x, xs))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse_mode env mode_opt (rule_dict : (string, G.expr) Hashtbl.t) : R.mode =
  match mode_opt with
  | None | Some "search" ->
      let formula = parse_formula env rule_dict in
      R.Search formula
  | Some "taint" ->
      let parse_sub_patterns key patterns =
        let parse_sub_pattern name pattern =
          parse_formula env (yaml_to_dict name pattern)
        in
        parse_list key (parse_sub_pattern (key ^ "list item")) patterns
      in
      let sources, sanitizers_opt, sinks =
        ( take rule_dict parse_sub_patterns "pattern-sources",
          take_opt rule_dict parse_sub_patterns "pattern-sanitizers",
          take rule_dict parse_sub_patterns "pattern-sinks" )
      in
      R.Taint { sources; sanitizers = opt_to_list sanitizers_opt; sinks }
  | Some _ -> error "Unexpected value for mode, should be 'search' or 'taint'"

let parse_generic file formula_ast =
  let rules_block =
    match formula_ast with
    | [
     {
       G.s =
         G.ExprStmt
           ( Container
               ( Dict,
                 (_, [ Tuple (_, [ L (String ("rules", _)); rules ], _) ], _) ),
             _ );
       _;
     };
    ] ->
        rules
    | _ -> error "missing rules entry as top-level key"
  in
  let rules =
    match rules_block with
    | Container (Array, (_, rules, _)) -> rules
    | _ -> error "expected a list of rules following `rules:`"
  in
  rules
  |> List.mapi (fun i rule ->
         let rule_dict = yaml_to_dict "rules" rule in
         let take f key = take rule_dict f key in
         let take_opt f key = take_opt rule_dict f key in
         let id, languages =
           (take parse_string "id", take parse_string_list "languages")
         in
         let languages = parse_languages ~id languages in
         let env = { id; languages; path = [ string_of_int i; "rules" ] } in
         let ( message,
               severity,
               mode_opt,
               metadata_opt,
               fix_opt,
               fix_regex_opt,
               paths_opt,
               equivs_opt,
               options_opt ) =
           ( take parse_string "message",
             take parse_string "severity",
             take_opt parse_string "mode",
             take_opt generic_to_json "metadata",
             take_opt parse_string "fix",
             take_opt (parse_fix_regex env) "fix-regex",
             take_opt parse_paths "paths",
             take_opt parse_equivalences "equivalences",
             take_opt parse_options "options" )
         in
         let mode = parse_mode env mode_opt rule_dict in
         {
           R.id;
           message;
           languages;
           file;
           severity = H.parse_severity ~id severity;
           mode;
           (* optional fields *)
           metadata = metadata_opt;
           fix = fix_opt;
           fix_regexp = fix_regex_opt;
           paths = paths_opt;
           equivalences = equivs_opt;
           options = options_opt;
         })

let parse file =
  let ast =
    match FT.file_type_of_file file with
    | FT.Config FT.Yaml -> Yaml_to_generic.program file
    | FT.Config FT.Json ->
        Json_to_generic.program (Parse_json.parse_program file)
    | FT.Config FT.Jsonnet ->
        Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
            let cmd = spf "jsonnet %s -o %s" file tmpfile in
            let n = Sys.command cmd in
            if n <> 0 then failwith (spf "error executing %s" cmd);
            Json_to_generic.program (Parse_json.parse_program tmpfile))
    | _ ->
        failwith
          (spf "wrong rule format, only JSON/YAML/JSONNET are valid:%s:" file)
  in
  parse_generic file ast

(*e: semgrep/parsing/Parse_rule.ml *)
