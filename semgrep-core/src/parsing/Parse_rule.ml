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
open Common
module J = JSON
module FT = File_type
module R = Rule
module MR = Mini_rule
module G = AST_generic
module PI = Parse_info
module Set = Set_

let logger = Logging.get_logger [ __MODULE__ ]

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
  languages : Xlang.t;
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

let yaml_error t s = raise (R.InvalidYaml (s, t))

let yaml_error_at_expr (e : G.expr) s =
  yaml_error (Visitor_AST.first_info_of_any (G.E e)) s

let yaml_error_at_key (key : key) s = yaml_error (snd key) s
let error env t s = raise (R.InvalidRule (R.InvalidOther s, env.id, t))
let error_at_key env (key : key) s = error env (snd key) s

let error_at_expr env (e : G.expr) s =
  error env (Visitor_AST.first_info_of_any (G.E e)) s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Why do we need this generic_to_json function? Why would we want to convert
 * to JSON when we actually did lots of work to convert the YAML/JSON
 * in the generic AST to get proper error location. This is because
 * the 'metadata' field in Rule.ml is JSON.
 *)
let generic_to_json env (key : key) ast =
  let rec aux x =
    match x.G.e with
    | G.L (Null _) -> J.Null
    | G.L (Bool (b, _)) -> J.Bool b
    | G.L (Float (Some f, _)) -> J.Float f
    | G.L (Int (Some i, _)) -> J.Int i
    | G.L (String (s, _)) ->
        (* should use the unescaped string *)
        J.String s
    | G.Container (Array, (_, xs, _)) -> J.Array (xs |> Common.map aux)
    | G.Container (Dict, (_, xs, _)) ->
        J.Object
          (xs
          |> Common.map (fun x ->
                 match x.G.e with
                 | G.Container
                     (G.Tuple, (_, [ { e = L (String (k, _)); _ }; v ], _)) ->
                     (* should use the unescaped string *)
                     (k, aux v)
                 | _ ->
                     error_at_expr env x
                       ("Expected key/value pair in " ^ fst key ^ " dictionary"))
          )
    | G.Alias (_alias, e) -> aux e
    | _ ->
        Common.pr2 (G.show_expr_kind x.G.e);
        error_at_expr env x "Unexpected generic representation of yaml"
  in
  aux ast

let read_string_wrap e =
  match e with
  | G.L (String (value, t)) ->
      (* should use the unescaped string *)
      Some (value, t)
  | G.L (Float (Some n, t)) ->
      if Float.is_integer n then Some (string_of_int (Float.to_int n), t)
      else Some (string_of_float n, t)
  | G.N (Id ((value, t), _)) -> Some (value, t)
  | _ -> None

(*****************************************************************************)
(* Dict helper methods *)
(*****************************************************************************)

let yaml_to_dict_helper error_fun_f error_fun_d (enclosing : string R.wrap)
    (rule : G.expr) : dict =
  match rule.G.e with
  (* note that the l/r are actually populated by yaml_to_generic, even
   * though there is no proper corresponding token
   *)
  | G.Container (Dict, (l, fields, _r)) ->
      let dict = Hashtbl.create 10 in
      fields
      |> List.iter (fun field ->
             match field.G.e with
             | G.Container
                 (G.Tuple, (_, [ { e = L (String (key_str, t)); _ }; value ], _))
               ->
                 (* Those are actually silently ignored by many YAML parsers
                  * which just consider the last key/value as the final one.
                  * This was a source of bugs in semgrep rules where people
                  * thought you could enter multiple metavariables under one
                  * metavariable-regex.
                  *)
                 if Hashtbl.mem dict key_str then
                   raise
                     (R.DuplicateYamlKey
                        (spf "duplicate key '%s' in dictionary" key_str, t));
                 Hashtbl.add dict key_str ((key_str, t), value)
             | _ -> error_fun_f field "Not a valid key value pair");
      { h = dict; first_tok = l }
  | _ -> error_fun_d rule ("each " ^ fst enclosing ^ " should be a dictionary")

(* Mutates the Hashtbl! *)
let (take_opt :
      dict -> env -> (env -> key -> G.expr -> 'a) -> string -> 'a option) =
 fun dict env f key_str ->
  Option.map
    (fun (key, value) ->
      let res = f env key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let (take : dict -> env -> (env -> key -> G.expr -> 'a) -> string -> 'a) =
 fun dict env f key_str ->
  match take_opt dict env f key_str with
  | Some res -> res
  | None -> error env dict.first_tok ("Missing required field " ^ key_str)

let yaml_to_dict env =
  yaml_to_dict_helper (error_at_expr env) (error_at_expr env)

(*****************************************************************************)
(* Parsing methods for before env is created *)
(*****************************************************************************)

(* Mutates the Hashtbl! *)
let (take_opt_no_env : dict -> (key -> G.expr -> 'a) -> string -> 'a option) =
 fun dict f key_str ->
  Option.map
    (fun (key, value) ->
      let res = f key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let (take_no_env : dict -> (key -> G.expr -> 'a) -> string -> 'a) =
 fun dict f key_str ->
  match take_opt_no_env dict f key_str with
  | Some res -> res
  | None -> yaml_error dict.first_tok ("Missing required field " ^ key_str)

let yaml_to_dict_no_env =
  yaml_to_dict_helper yaml_error_at_expr yaml_error_at_expr

let parse_string_wrap_no_env (key : key) x =
  match read_string_wrap x.G.e with
  | Some (value, t) -> (value, t)
  | None -> yaml_error_at_key key ("Expected a string value for " ^ fst key)

let parse_list_no_env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map f xs
  | _ -> yaml_error_at_key key ("Expected a list for " ^ fst key)

let parse_string_wrap_list_no_env (key : key) e =
  let extract_string = function
    | { G.e = G.L (String (value, t)); _ } -> (value, t)
    | _ ->
        yaml_error_at_key key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list_no_env key extract_string e

(*****************************************************************************)
(* Sub parsers basic types *)
(*****************************************************************************)

let parse_string_wrap env (key : key) x =
  match read_string_wrap x.G.e with
  | Some (value, t) -> (value, t)
  | None -> error_at_key env key ("Expected a string value for " ^ fst key)

(* TODO: delete at some point, should use parse_string_wrap instead *)
let parse_string env (key : key) x = parse_string_wrap env key x |> fst

let parse_list env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> Common.map (f env) xs
  | _ -> error_at_key env key ("Expected a list for " ^ fst key)

(* TODO: delete at some point, should use parse_string_wrap_list *)
let parse_string_list env (key : key) e =
  let extract_string env = function
    | { G.e = G.L (String (value, _)); _ } -> value
    | _ ->
        error_at_key env key
          ("Expected all values in the list to be strings for " ^ fst key)
  in
  parse_list env key extract_string e

let parse_listi env (key : key) f x =
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List.mapi f xs
  | _ -> error_at_key env key ("Expected a list for " ^ fst key)

let parse_bool env (key : key) x =
  match x.G.e with
  | G.L (String ("true", _)) -> true
  | G.L (String ("false", _)) -> false
  | G.L (Bool (b, _)) -> b
  | _x -> error_at_key env key (spf "parse_bool for %s" (fst key))

let parse_int env (key : key) x =
  match x.G.e with
  | G.L (Int (Some i, _)) -> i
  | G.L (String (s, _)) -> (
      try int_of_string s with
      | Failure _ -> error_at_key env key (spf "parse_int for %s" (fst key)))
  | G.L (Float (Some f, _)) ->
      let i = int_of_float f in
      if float_of_int i = f then i else error_at_key env key "not an int"
  | _x -> error_at_key env key (spf "parse_int for %s" (fst key))

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

let parse_metavar_cond env key s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error_at_key env key "not an expression"
  with
  | Timeout _ as e -> raise e
  | UnixExit n -> raise (UnixExit n)
  | exn -> error_at_key env key ("exn: " ^ Common.exn_to_s exn)

let parse_regexp env (s, t) =
  try (s, SPcre.regexp s) with
  | Pcre.Error exn ->
      raise
        (R.InvalidRule (R.InvalidRegexp (pcre_error_to_string s exn), env.id, t))

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
                      (_, [ { e = L (String ("equivalence", t)); _ }; value ], _)
                    );
                _;
              };
            ],
            _ ) ) ->
        parse_string env ("equivalence", t) value
    | _ ->
        error_at_expr env equiv
          "Expected `equivalence: $X` for each equivalences list item"
  in
  parse_list env key parse_equivalence value

let parse_paths env key value =
  let paths_dict = yaml_to_dict env key value in
  let inc_opt, exc_opt =
    ( take_opt paths_dict env parse_string_list "include",
      take_opt paths_dict env parse_string_list "exclude" )
  in
  { R.include_ = optlist_to_list inc_opt; exclude = optlist_to_list exc_opt }

let parse_options env (key : key) value =
  let s = J.string_of_json (generic_to_json env key value) in
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

(* less: could move in a separate Parse_xpattern.ml *)
let parse_xpattern xlang (str, tok) =
  match xlang with
  | Xlang.L (lang, _) ->
      let pat = Parse_pattern.parse_pattern lang ~print_errors:false str in
      R.mk_xpat (R.Sem (pat, lang)) (str, tok)
  | Xlang.LRegex -> failwith "TODO"
  | Xlang.LGeneric -> (
      let src = Spacegrep.Src_file.of_string str in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> R.mk_xpat (R.Spacegrep ast) (str, tok)
      | Error err -> failwith err.msg)

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
 *
 * TODO: adjust pos with Map_AST.mk_fix_token_locations and
 * Parse_info.adjust_info_wrt_base t
 *)

let parse_xpattern_expr env e =
  let s, t =
    match read_string_wrap e.G.e with
    | Some (s, t) -> (s, t)
    | None -> error_at_expr env e ("Expected a string value for " ^ env.id)
  in

  (* emma: This is for later, but note that start and end_ are currently the
   * same (each pattern is only associated with one token). This might be
   * really annoying to change (we need to compute an accurate end_, but
   * the string given to us by the yaml parser has tabs removed).
   * Will include a note to this effect when I make my
   * "add ranges to patterns" PR.
   *)

  (* let start, end_ = Visitor_AST.range_of_any (G.E e) in
     let _s_range =
       (PI.mk_info_of_loc start, PI.mk_info_of_loc end_)
       (* TODO put in *)
     in *)
  try parse_xpattern env.languages (s, t) with
  | (Timeout _ | UnixExit _) as e -> raise e
  (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
  | exn ->
      raise
        (R.InvalidRule
           ( R.InvalidPattern (s, env.languages, Common.exn_to_s exn, env.path),
             env.id,
             t ))

let find_formula_old env (rule_dict : dict) : key * G.expr =
  let find key_str = Hashtbl.find_opt rule_dict.h key_str in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex",
      find "pattern-comby" )
  with
  | None, None, None, None, None ->
      error env rule_dict.first_tok
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, `pattern-comby` to be present"
  | Some (key, value), None, None, None, None
  | None, Some (key, value), None, None, None
  | None, None, Some (key, value), None, None
  | None, None, None, Some (key, value), None
  | None, None, None, None, Some (key, value) ->
      (key, value)
  | _ ->
      error env rule_dict.first_tok
        "Expected only one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, or `pattern-comby`"

let rec parse_formula_old env ((key, value) : key * G.expr) : R.formula_old =
  let env = { env with path = fst key :: env.path } in
  let get_pattern str_e = parse_xpattern_expr env str_e in
  let get_nested_formula i x =
    let env = { env with path = string_of_int i :: env.path } in
    match x.G.e with
    | G.Container
        ( Dict,
          ( _,
            [
              {
                e =
                  Container (Tuple, (_, [ { e = L (String key); _ }; value ], _));
                _;
              };
            ],
            _ ) ) ->
        parse_formula_old env (key, value)
    | _ -> error_at_expr env x "Wrong parse_formula fields"
  in
  let s, t = key in
  match s with
  | "pattern" -> R.Pat (get_pattern value)
  | "pattern-not" -> R.PatNot (t, get_pattern value)
  | "focus-metavariable" -> R.PatFocus (t, parse_string env key value)
  | "pattern-inside" -> R.PatInside (get_pattern value)
  | "pattern-not-inside" -> R.PatNotInside (t, get_pattern value)
  | "pattern-either" ->
      R.PatEither (t, parse_listi env key get_nested_formula value)
  | "patterns" -> R.Patterns (t, parse_listi env key get_nested_formula value)
  | "pattern-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = R.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Pat xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = R.mk_xpat (Regexp (parse_regexp env x)) x in
      R.PatNot (t, xpat)
  | "pattern-comby" ->
      let x = parse_string_wrap env key value in
      let xpat = R.mk_xpat (Comby (fst x)) x in
      R.Pat xpat
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-pattern"
  | "metavariable-comparison"
  | "pattern-where-python" ->
      R.PatExtra (t, parse_extra env key value)
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> R.PatFilteredInPythonTodo t
  | _ -> error_at_key env key (spf "unexpected key %s" (fst key))

(* let extra = parse_extra env x in
   R.PatExtra extra *)
and parse_formula_new env (x : G.expr) : R.formula =
  match x.G.e with
  | G.Container
      ( Dict,
        ( _,
          [
            {
              e = Container (Tuple, (_, [ { e = L (String key); _ }; value ], _));
              _;
            };
          ],
          _ ) ) -> (
      let s, t = key in

      match s with
      | "and" ->
          let xs = parse_list env key parse_formula_and_new value in
          let fs, conds = Common.partition_either (fun x -> x) xs in
          R.And
            {
              tok = t;
              conjuncts = fs;
              conditions = conds;
              (* TODO: 'focus-metavariable:' *)
              focus = [];
            }
      | "or" -> R.Or (t, parse_list env key parse_formula_new value)
      | "not" -> R.Not (t, parse_formula_new env value)
      | "inside" -> R.P (parse_xpattern_expr env value, Some Inside)
      | "regex" ->
          let x = parse_string_wrap env key value in
          let xpat = R.mk_xpat (R.Regexp (parse_regexp env x)) x in
          R.P (xpat, None)
      | "comby" ->
          let x = parse_string_wrap env key value in
          let xpat = R.mk_xpat (R.Comby (fst x)) x in
          R.P (xpat, None)
      | _ -> error_at_key env key ("Invalid key for formula_new " ^ fst key))
  | _ -> R.P (parse_xpattern_expr env x, None)

and parse_formula_and_new env (x : G.expr) :
    (R.formula, R.tok * R.metavar_cond) Common.either =
  match x.G.e with
  | G.Container
      ( Dict,
        ( _,
          [
            {
              e = Container (Tuple, (_, [ { e = L (String key); _ }; value ], _));
              _;
            };
          ],
          _ ) ) -> (
      let s, t = key in

      match s with
      | "where" ->
          let s = parse_string env key value in
          Right (t, R.CondEval (parse_metavar_cond env key s))
      | "metavariable_regex" -> (
          match value.G.e with
          | G.Container (Array, (_, [ mvar; re ], _)) ->
              let mvar = parse_string env key mvar in
              let x = parse_string_wrap env key re in
              Right (t, R.CondRegexp (mvar, parse_regexp env x))
          | _ -> error_at_expr env value "Expected a metavariable and regex")
      | _ -> Left (parse_formula_new env x))
  | _ -> Left (R.P (parse_xpattern_expr env x, None))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) : Rule.extra =
  match fst key with
  | "metavariable-analysis" ->
      let mv_analysis_dict = yaml_to_dict env key value in
      let metavar = take mv_analysis_dict env parse_string "metavariable" in
      let analyzer = take mv_analysis_dict env parse_string "analyzer" in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "redos" -> R.CondReDoS
        | other -> error_at_key env key ("Unsupported analyzer: " ^ other)
      in
      R.MetavarAnalysis (metavar, kind)
  | "metavariable-regex" ->
      let mv_regex_dict =
        try yaml_to_dict env key value with
        | R.DuplicateYamlKey (msg, t) ->
            error env t (msg ^ ". You should use multiple metavariable-regex")
      in
      let metavar, regexp =
        ( take mv_regex_dict env parse_string "metavariable",
          take mv_regex_dict env parse_string_wrap "regex" )
      in
      R.MetavarRegexp (metavar, parse_regexp env regexp)
  | "metavariable-pattern" ->
      let mv_pattern_dict = yaml_to_dict env key value in
      let metavar = take mv_pattern_dict env parse_string "metavariable" in
      let env', opt_xlang =
        match take_opt mv_pattern_dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~id:(Some env.id) s in
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
      let formula_old =
        parse_formula_old env' (find_formula_old env mv_pattern_dict)
      in
      R.MetavarPattern (metavar, opt_xlang, formula_old)
  | "metavariable-comparison" ->
      let mv_comparison_dict = yaml_to_dict env key value in
      let metavariable, comparison, strip, base =
        ( take mv_comparison_dict env parse_string "metavariable",
          take mv_comparison_dict env parse_string "comparison",
          take_opt mv_comparison_dict env parse_bool "strip",
          take_opt mv_comparison_dict env parse_int "base" )
      in
      let comparison = parse_metavar_cond env key comparison in
      R.MetavarComparison { R.metavariable; comparison; strip; base }
  | "pattern-where-python" -> R.PatWherePython (parse_string env key value)
  | _ -> error_at_key env key ("wrong parse_extra field: " ^ fst key)

let parse_languages ~id langs : Xlang.t =
  match langs with
  | [ (("none" | "regex"), _t) ] -> LRegex
  | [ ("generic", _t) ] -> LGeneric
  | xs -> (
      let languages =
        xs
        |> Common.map (function s, t ->
               (match Lang.lang_of_string_opt s with
               | None -> raise (R.InvalidRule (R.InvalidLanguage s, fst id, t))
               | Some l -> l))
      in
      match languages with
      | [] ->
          raise
            (R.InvalidRule
               (R.InvalidOther "we need at least one language", fst id, snd id))
      | x :: xs -> L (x, xs))

let parse_severity ~id (s, t) =
  match s with
  | "ERROR" -> R.Error
  | "WARNING" -> R.Warning
  | "INFO" -> R.Info
  | "INVENTORY" -> R.Inventory
  | s ->
      raise
        (R.InvalidRule
           ( R.InvalidOther
               (spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s),
             id,
             t ))

(*****************************************************************************)
(* Sub parsers taint *)
(*****************************************************************************)

let parse_formula (env : env) (rule_dict : dict) : R.pformula =
  match Hashtbl.find_opt rule_dict.h "match" with
  | Some (_matchkey, v) -> R.New (parse_formula_new env v)
  | None -> R.Old (parse_formula_old env (find_formula_old env rule_dict))

let parse_sanitizer env (key : key) (value : G.expr) =
  let sanitizer_dict = yaml_to_dict env key value in
  let not_conflicting =
    take_opt sanitizer_dict env parse_bool "not_conflicting"
    |> Option.value ~default:false
  in
  let pformula = parse_formula env sanitizer_dict in
  { R.not_conflicting; pformula }

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  match mode_opt with
  | None
  | Some ("search", _) ->
      let formula = parse_formula env rule_dict in
      R.Search formula
  | Some ("taint", _) ->
      let parse_sub_formula env name pattern =
        parse_formula env (yaml_to_dict env name pattern)
      in
      let parse_specs parse_spec env key x =
        parse_list env key
          (fun env -> parse_spec env (fst key ^ "list item", snd key))
          x
      in
      let sources, sanitizers_opt, sinks =
        ( take rule_dict env (parse_specs parse_sub_formula) "pattern-sources",
          take_opt rule_dict env
            (parse_specs parse_sanitizer)
            "pattern-sanitizers",
          take rule_dict env (parse_specs parse_sub_formula) "pattern-sinks" )
      in
      R.Taint { sources; sanitizers = optlist_to_list sanitizers_opt; sinks }
  | Some key ->
      error_at_key env key
        (spf "Unexpected value for mode, should be 'search' or 'taint', not %s"
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

let parse_one_rule t i rule =
  let rd = yaml_to_dict_no_env ("rules", t) rule in
  let id, languages =
    ( take_no_env rd parse_string_wrap_no_env "id",
      take_no_env rd parse_string_wrap_list_no_env "languages" )
  in
  let languages = parse_languages ~id languages in
  let env = { id = fst id; languages; path = [ string_of_int i; "rules" ] } in
  let ( message,
        severity,
        mode_opt,
        metadata_opt,
        fix_opt,
        fix_regex_opt,
        paths_opt,
        equivs_opt,
        options_opt ) =
    ( take rd env parse_string "message",
      take rd env parse_string_wrap "severity",
      take_opt rd env parse_string_wrap "mode",
      take_opt rd env generic_to_json "metadata",
      take_opt rd env parse_string "fix",
      take_opt rd env parse_fix_regex "fix-regex",
      take_opt rd env parse_paths "paths",
      take_opt rd env parse_equivalences "equivalences",
      take_opt rd env parse_options "options" )
  in
  let mode = parse_mode env mode_opt rd in
  report_unparsed_fields rd;
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
  }

let parse_generic ?(error_recovery = false) file ast =
  ignore error_recovery;
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
                          (_, [ { e = L (String ("rules", _)); _ }; rules ], _)
                        );
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
            let loc = PI.first_loc_of_file file in
            yaml_error (PI.mk_info_of_loc loc)
              "missing rules entry as top-level key")
    | _ -> assert false
    (* yaml_to_generic should always return a ExprStmt *)
  in
  let xs =
    rules
    |> List.mapi (fun i rule ->
           if error_recovery then (
             try Left (parse_one_rule t i rule) with
             | R.InvalidRule ((kind, ruleid, _) as err) ->
                 let s = Rule.string_of_invalid_rule_error_kind kind in
                 logger#warning "skipping rule %s, error = %s" ruleid s;
                 Right err)
           else Left (parse_one_rule t i rule))
  in
  Common.partition_either (fun x -> x) xs

let parse_bis ?error_recovery file =
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
         * Note that this is handled correctly by Yaml_to_generic.program
         * below.
         *)
        Json_to_generic.program ~unescape_strings:true
          (Parse_json.parse_program file)
    | FT.Config FT.Jsonnet ->
        Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
            let cmd = spf "jsonnet %s -o %s" file tmpfile in
            let n = Sys.command cmd in
            if n <> 0 then failwith (spf "error executing %s" cmd);
            Json_to_generic.program ~unescape_strings:true
              (Parse_json.parse_program tmpfile))
    | FT.Config FT.Yaml -> Yaml_to_generic.program file
    | _ ->
        logger#error "wrong rule format, only JSON/YAML/JSONNET are valid";
        logger#info "trying to parse %s as YAML" file;
        Yaml_to_generic.program file
  in
  parse_generic ?error_recovery file ast

let parse file =
  let xs, skipped = parse_bis ~error_recovery:false file in
  assert (skipped = []);
  xs

let parse_and_filter_invalid_rules file = parse_bis ~error_recovery:true file
