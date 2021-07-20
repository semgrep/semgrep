(*s: semgrep/parsing/Parse_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau, Emma Jin
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
module MR = Mini_rule
module G = AST_generic
module PI = Parse_info
module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema in rule_schema.yaml.
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
 * See the Yaml_to_generic.program function. We then abuse this function
 * to also parse a semgrep rule (which is a yaml file) in this file.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  (* id of the current rule (needed by some exns) *)
  id : Rule.rule_id;
  (* languages of the current rule (needed by parse_pattern) *)
  languages : R.xlang;
  (* emma: save the path within the yaml file for each pattern
   * (this will allow us to later report errors in playground basic mode)
   *)
  path : string list;
}

type key = string R.wrap

(* Parsing generic dictionaries creates a mutable Hashtbl and consumes the
 * fields as they are processed.
 * todo? use a Map instead?
 *)
type dict = {
  (* !this is mutated! *)
  h : (string, key * AST_generic.expr) Hashtbl.t;
  (* for error reports on missing fields *)
  first_tok : R.tok;
}

(*****************************************************************************)
(* Error Management *)
(*****************************************************************************)

exception InvalidLanguage of Rule.rule_id * string * Parse_info.t

(* TODO: the Parse_info.t is not precise for now, it corresponds to the
 * start of the pattern *)
exception
  InvalidPattern of
    Rule.rule_id * string * Rule.xlang * string (* exn *) * Parse_info.t

exception InvalidRegexp of Rule.rule_id * string * Parse_info.t

(* general errors *)
exception InvalidYaml of string * Parse_info.t

exception DuplicateYamlKey of string * Parse_info.t

(* less: could be merged with InvalidYaml *)
exception InvalidRule of Rule.rule_id * string * Parse_info.t

let error t s = raise (InvalidYaml (s, t))

let error_at_key (key : key) s = error (snd key) s

let error_at_expr (e : G.expr) s =
  error (Visitor_AST.first_info_of_any (G.E e)) s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Why do we need this generic_to_json function? Why would we want to convert
 * to JSON when we actually did lots of work to convert the YAML/JSON
 * in the generic AST to get proper error location. This is because
 * the 'metadata' field in Rule.ml is JSON.
 *)
let generic_to_json (key : key) ast =
  let rec aux = function
    | G.L (Null _) -> J.Null
    | G.L (Bool (b, _)) -> J.Bool b
    | G.L (Float (Some f, _)) -> J.Float f
    | G.L (Int (Some i, _)) -> J.Int i
    | G.L (String (s, _)) -> J.String s
    | G.Container (Array, (_, xs, _)) -> J.Array (xs |> Ls.map aux)
    | G.Container (Dict, (_, xs, _)) ->
        J.Object
          (xs
          |> Ls.map (fun x ->
                 match x with
                 | G.Tuple (_, [ L (String (k, _)); v ], _) -> (k, aux v)
                 | _ ->
                     error_at_expr x
                       ("Expected key/value pair in " ^ fst key ^ " dictionary"))
          )
    | x -> error_at_expr x "Unexpected generic representation of yaml"
  in
  aux ast

let optlist_to_list = function None -> [] | Some xs -> xs

(*****************************************************************************)
(* Dict helper methods *)
(*****************************************************************************)

let yaml_to_dict (enclosing : string R.wrap) (rule : G.expr) : dict =
  match rule with
  (* note that the l/r are actually populated by yaml_to_generic, even
   * though there is no proper corresponding token
   *)
  | G.Container (Dict, (l, fields, _r)) ->
      let dict = Hashtbl.create 10 in
      fields
      |> List.iter (fun field ->
             match field with
             | G.Tuple (_, [ L (String (key_str, t)); value ], _) ->
                 (* Those are actually silently ignored by many YAML parsers
                  * which just consider the last key/value as the final one.
                  * This was a source of bugs in semgrep rules where people
                  * thought you could enter multiple metavariables under one
                  * metavariable-regex.
                  *)
                 if Hashtbl.mem dict key_str then
                   raise
                     (DuplicateYamlKey
                        (spf "duplicate key '%s' in dictionary" key_str, t));
                 Hashtbl.add dict key_str ((key_str, t), value)
             | x -> error_at_expr x "Not a valid key value pair");
      { h = dict; first_tok = l }
  | x -> error_at_expr x ("each " ^ fst enclosing ^ " should be a dictionary")

(* Mutates the Hashtbl! *)
let (take_opt : dict -> (key -> G.expr -> 'a) -> string -> 'a option) =
 fun dict f key_str ->
  Common.map_opt
    (fun (key, value) ->
      let res = f key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let (take : dict -> (key -> G.expr -> 'a) -> string -> 'a) =
 fun dict f key_str ->
  match take_opt dict f key_str with
  | Some res -> res
  | None -> error dict.first_tok ("Missing required field " ^ key_str)

(*****************************************************************************)
(* Sub parsers basic types *)
(*****************************************************************************)

(* TODO: delete at some point, should use parse_string_wrap instead *)
let parse_string (key : key) = function
  | G.L (String (value, _)) -> value
  | G.N (Id ((value, _), _)) -> value
  | _ -> error_at_key key ("Expected a string value for " ^ fst key)

let parse_string_wrap (key : key) = function
  | G.L (String (value, t)) -> (value, t)
  | G.N (Id ((value, t), _)) -> (value, t)
  | _ -> error_at_key key ("Expected a string value for " ^ fst key)

let parse_list (key : key) f = function
  | G.Container (Array, (_, xs, _)) -> Ls.map f xs
  | _ -> error_at_key key ("Expected a list for " ^ fst key)

(* TODO: delete at some point, should use parse_string_wrap_list *)
let parse_string_list (key : key) e =
  let extract_string = function
    | G.L (String (value, _)) -> value
    | _ ->
        error_at_key key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list key extract_string e

let parse_string_wrap_list (key : key) e =
  let extract_string = function
    | G.L (String (value, t)) -> (value, t)
    | _ ->
        error_at_key key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list key extract_string e

let parse_listi (key : key) f = function
  | G.Container (Array, (_, xs, _)) -> Ls.mapi f xs
  | _ -> error_at_key key ("Expected a list for " ^ fst key)

let parse_bool (key : key) = function
  | G.L (String ("true", _)) -> true
  | G.L (String ("false", _)) -> false
  | G.L (Bool (b, _)) -> b
  | _x -> error_at_key key (spf "parse_bool for %s" (fst key))

let parse_int (key : key) = function
  | G.L (Int (Some i, _)) -> i
  | G.L (String (s, _)) -> (
      try int_of_string s
      with Failure _ -> error_at_key key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) ->
      let i = int_of_float f in
      if float_of_int i = f then i else error_at_key key "not an int"
  | _x -> error_at_key key (spf "parse_int for %s" (fst key))

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

let parse_metavar_cond key s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error_at_key key "not an expression"
  with
  | Timeout -> raise Timeout
  | UnixExit n -> raise (UnixExit n)
  | exn -> error_at_key key ("exn: " ^ Common.exn_to_s exn)

let parse_regexp env (s, t) =
  try (s, Pcre.regexp s)
  with Pcre.Error exn ->
    raise (InvalidRegexp (env.id, pcre_error_to_string s exn, t))

let parse_fix_regex (env : env) (key : key) fields =
  let fix_regex_dict = yaml_to_dict key fields in
  let (regex : string R.wrap) = take fix_regex_dict parse_string_wrap "regex" in
  let (replacement : string) = take fix_regex_dict parse_string "replacement" in
  let (count_opt : int option) = take_opt fix_regex_dict parse_int "count" in
  (parse_regexp env regex, count_opt, replacement)

let parse_equivalences key value =
  let parse_equivalence equiv =
    match equiv with
    | G.Container
        ( Dict,
          (_, [ Tuple (_, [ L (String ("equivalence", t)); value ], _) ], _) )
      ->
        parse_string ("equivalence", t) value
    | x ->
        error_at_expr x
          "Expected `equivalence: $X` for each equivalences list item"
  in
  parse_list key parse_equivalence value

let parse_paths key value =
  let paths_dict = yaml_to_dict key value in
  let inc_opt, exc_opt =
    ( take_opt paths_dict parse_string_list "include",
      take_opt paths_dict parse_string_list "exclude" )
  in
  { R.include_ = optlist_to_list inc_opt; exclude = optlist_to_list exc_opt }

let parse_options (key : key) value =
  let s = J.string_of_json (generic_to_json key value) in
  Common.save_excursion Atdgen_runtime.Util.Json.unknown_field_handler
    (fun _src_loc field_name ->
      (* for forward compatibility, better to not raise an exn and just
       * ignore the new fields.
       * TODO: we should use a warning/logging infra to report
       * this in the JSON to the semgrep wrapper and user.
       *)
      (*raise (InvalidYamlException (spf "unknown option: %s" field_name))*)
      pr2 (spf "WARNING: unknown option: %s" field_name))
    (fun () -> Config_semgrep_j.t_of_string s)

(*****************************************************************************)
(* Sub parsers patterns and formulas *)
(*****************************************************************************)

(* TODO: note that the [pattern] string and token location [t] given to us
 * by the YAML parser do not correspond exactly to the content
 * in the YAML file. If the pattern is on a single line, as in
 *    pattern: foo($X)
 * then everything is fine, but if it's on multiple lines as in
 *    pattern: |
 *       foo($X);
 *       bar($X);
 * The pattern string will contain "foo($X);\nbar($X);\n" without any
 * indentation and the token location [t] will actually be the location
 * of the leading "|", so we need to recompute location by reparsing
 * the YAML file and look at the indentation there.
 *)
let parse_pattern ~id ~lang (pattern, t) =
  try
    (* old? todo? call Normalize_ast.normalize here? *)
    let any = Parse_pattern.parse_pattern lang ~print_errors:false pattern in
    (* TODO: adjust pos with Map_AST.mk_fix_token_locations and
     * Parse_info.adjust_info_wrt_base t
     *)
    any
  with
  | Timeout -> raise Timeout
  | UnixExit n -> raise (UnixExit n)
  (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
  | exn ->
      raise
        (InvalidPattern (id, pattern, Rule.L (lang, []), Common.exn_to_s exn, t))

let parse_xpattern env e =
  let s, t =
    match e with
    | G.L (String (s, t)) -> (s, t)
    | G.N (Id ((s, t), _)) -> (s, t)
    | x -> error_at_expr x ("Expected a string value for " ^ env.id)
  in
  (* emma: This is for later, but note that start and end_ are currently the same
   * (each pattern is only associated with one token). This might be really annoying
   * to change (we need to compute an accurate end_, but the string given to us by
   * the yaml parser has tabs removed). Will include a note to this effect when
   * I make my "add ranges to patterns" PR.
   *)
  let start, end_ = Visitor_AST.range_of_any (G.E e) in
  let _s_range =
    (PI.mk_info_of_loc start, PI.mk_info_of_loc end_)
    (* TODO put in *)
  in
  match env.languages with
  | R.L (lang, _) ->
      R.mk_xpat (Sem (parse_pattern ~id:env.id ~lang (s, t), lang)) (s, t)
  | R.LRegex -> failwith "you should not use real pattern with language = none"
  | R.LGeneric -> (
      let src = Spacegrep.Src_file.of_string s in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> R.mk_xpat (Spacegrep ast) (s, t)
      | Error err ->
          (* TODO: adjust error pos instead of using [t] *)
          raise (InvalidPattern (env.id, s, Rule.LGeneric, err.msg, t)))

let find_formula_old (rule_dict : dict) : key * G.expr =
  let find key_str = Hashtbl.find_opt rule_dict.h key_str in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex",
      find "pattern-comby" )
  with
  | None, None, None, None, None ->
      error rule_dict.first_tok
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, `pattern-comby` to be present"
  | Some (key, value), None, None, None, None
  | None, Some (key, value), None, None, None
  | None, None, Some (key, value), None, None
  | None, None, None, Some (key, value), None
  | None, None, None, None, Some (key, value) ->
      (key, value)
  | _ ->
      error rule_dict.first_tok
        "Expected only one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, or `pattern-comby`"

let rec parse_formula (env : env) (rule_dict : dict) : R.pformula =
  match Hashtbl.find_opt rule_dict.h "match" with
  | Some (_matchkey, v) -> R.New (parse_formula_new env v)
  | None -> R.Old (parse_formula_old env (find_formula_old rule_dict))

and parse_formula_old env ((key, value) : key * G.expr) : R.formula_old =
  let env = { env with path = fst key :: env.path } in
  let get_pattern str_e = parse_xpattern env str_e in
  let get_nested_formula i x =
    let env = { env with path = string_of_int i :: env.path } in
    match x with
    | G.Container (Dict, (_, [ Tuple (_, [ L (String key); value ], _) ], _)) ->
        parse_formula_old env (key, value)
    | x -> error_at_expr x "Wrong parse_formula fields"
  in
  let s, t = key in
  match s with
  | "pattern" -> R.Pat (get_pattern value)
  | "pattern-not" -> R.PatNot (t, get_pattern value)
  | "pattern-inside" -> R.PatInside (get_pattern value)
  | "pattern-not-inside" -> R.PatNotInside (t, get_pattern value)
  | "pattern-either" -> R.PatEither (t, parse_listi key get_nested_formula value)
  | "patterns" -> R.Patterns (t, parse_listi key get_nested_formula value)
  | "pattern-regex" ->
      let x = parse_string_wrap key value in
      let xpat = R.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Pat xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap key value in
      let xpat = R.mk_xpat (Regexp (parse_regexp env x)) x in
      R.PatNot (t, xpat)
  | "pattern-comby" ->
      let x = parse_string_wrap key value in
      let xpat = R.mk_xpat (Comby (fst x)) x in
      R.Pat xpat
  | "metavariable-regex" | "metavariable-pattern" | "metavariable-comparison"
  | "pattern-where-python" ->
      R.PatExtra (t, parse_extra env key value)
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  | _ -> error_at_key key (spf "unexpected key %s" (fst key))

(* let extra = parse_extra env x in
   R.PatExtra extra *)
and parse_formula_new env (x : G.expr) : R.formula =
  match x with
  | G.Container (Dict, (_, [ Tuple (_, [ L (String key); value ], _) ], _)) -> (
      let s, t = key in

      match s with
      | "and" -> R.And (t, parse_list key (parse_formula_new env) value)
      | "or" -> R.Or (t, parse_list key (parse_formula_new env) value)
      | "not" -> R.Not (t, parse_formula_new env value)
      | "inside" -> R.Leaf (R.P (parse_xpattern env value, Some Inside))
      | "regex" ->
          let x = parse_string_wrap key value in
          let xpat = R.mk_xpat (R.Regexp (parse_regexp env x)) x in
          R.Leaf (R.P (xpat, None))
      | "comby" ->
          let x = parse_string_wrap key value in
          let xpat = R.mk_xpat (R.Comby (fst x)) x in
          R.Leaf (R.P (xpat, None))
      | "where" ->
          let s = parse_string key value in
          R.Leaf (R.MetavarCond (t, R.CondEval (parse_metavar_cond key s)))
      | "metavariable_regex" -> (
          match value with
          | G.Container (Array, (_, [ mvar; re ], _)) ->
              let mvar = parse_string key mvar in
              let x = parse_string_wrap key re in
              R.Leaf
                (R.MetavarCond (t, R.CondRegexp (mvar, parse_regexp env x)))
          | x -> error_at_expr x "Expected a metavariable and regex")
      | _ -> error_at_key key ("Invalid key for formula_new " ^ fst key))
  | _ -> R.Leaf (R.P (parse_xpattern env x, None))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) : Rule.extra =
  match fst key with
  | "metavariable-regex" ->
      let mv_regex_dict =
        try yaml_to_dict key value
        with DuplicateYamlKey (msg, t) ->
          raise
            (InvalidYaml
               (msg ^ ". You should use multiple metavariable-regex", t))
      in
      let metavar, regexp =
        ( take mv_regex_dict parse_string "metavariable",
          take mv_regex_dict parse_string_wrap "regex" )
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
      let comparison = parse_metavar_cond key comparison in
      R.MetavarComparison { R.metavariable; comparison; strip; base }
  | "pattern-where-python" -> R.PatWherePython (parse_string key value)
  | _ -> error_at_key key ("wrong parse_extra field: " ^ fst key)

let parse_languages ~id langs =
  match langs with
  | [ (("none" | "regex"), _t) ] -> R.LRegex
  | [ ("generic", _t) ] -> R.LGeneric
  | xs -> (
      let languages =
        xs
        |> Ls.map (function s, t ->
               (match Lang.lang_of_string_opt s with
               | None ->
                   raise
                     (InvalidLanguage
                        (fst id, spf "unsupported language: %s" s, t))
               | Some l -> l))
      in
      match languages with
      | [] ->
          raise (InvalidRule (fst id, "we need at least one language", snd id))
      | x :: xs -> R.L (x, xs))

let parse_severity ~id (s, t) =
  match s with
  | "ERROR" -> R.Error
  | "WARNING" -> R.Warning
  | "INFO" -> R.Info
  | s ->
      raise
        (InvalidRule
           (id, spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s, t))

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  match mode_opt with
  | None | Some ("search", _) ->
      let formula = parse_formula env rule_dict in
      R.Search formula
  | Some ("taint", _) ->
      let parse_sub_patterns key patterns =
        let parse_sub_pattern name pattern =
          parse_formula env (yaml_to_dict name pattern)
        in
        parse_list key
          (parse_sub_pattern (fst key ^ "list item", snd key))
          patterns
      in
      let sources, sanitizers_opt, sinks =
        ( take rule_dict parse_sub_patterns "pattern-sources",
          take_opt rule_dict parse_sub_patterns "pattern-sanitizers",
          take rule_dict parse_sub_patterns "pattern-sinks" )
      in
      R.Taint { sources; sanitizers = optlist_to_list sanitizers_opt; sinks }
  | Some key ->
      error_at_key key
        (spf "Unexpected value for mode, should be 'search' or 'taint', not %s"
           (fst key))

let parse_generic file ast =
  let rules_block =
    match ast with
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
    | _ ->
        let loc = PI.first_loc_of_file file in
        error (PI.mk_info_of_loc loc) "missing rules entry as top-level key"
  in
  let t, rules =
    match rules_block with
    | Container (Array, (l, rules, _r)) -> (l, rules)
    | x -> error_at_expr x "expected a list of rules following `rules:`"
  in
  rules
  |> Ls.mapi (fun i rule ->
         let rd = yaml_to_dict ("rules", t) rule in
         let id, languages =
           ( take rd parse_string_wrap "id",
             take rd parse_string_wrap_list "languages" )
         in
         let languages = parse_languages ~id languages in
         let env =
           { id = fst id; languages; path = [ string_of_int i; "rules" ] }
         in
         let ( message,
               severity,
               mode_opt,
               metadata_opt,
               fix_opt,
               fix_regex_opt,
               paths_opt,
               equivs_opt,
               options_opt ) =
           ( take rd parse_string "message",
             take rd parse_string_wrap "severity",
             take_opt rd parse_string_wrap "mode",
             take_opt rd generic_to_json "metadata",
             take_opt rd parse_string "fix",
             take_opt rd (parse_fix_regex env) "fix-regex",
             take_opt rd parse_paths "paths",
             take_opt rd parse_equivalences "equivalences",
             take_opt rd parse_options "options" )
         in
         let mode = parse_mode env mode_opt rd in
         {
           R.id;
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
