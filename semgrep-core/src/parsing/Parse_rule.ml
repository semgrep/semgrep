(* Yoann Padioleau, Emma Jin
 *
 * Copyright (C) 2019-2022 r2c
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
module XP = Xpattern
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
 * See the Yaml_to_generic.parse_rule function. We then abuse this function
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
   * TODO the playground (now the editor) no longer allows multiple
   * rules, so in each path the first number should be removed
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
    | G.N (Id ((s, _), _)) ->
        (* this is possible because templates may include strings that
           the parser interprets as metavariables. TODO turn that
           interpretation off *)
        J.String s
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
(* Experiment: parsers for metatypes *)
(*****************************************************************************)

let parse_one_type type_def =
  match type_def.G.e with
  | Container
      ( Dict,
        ( _,
          [
            {
              e =
                Container
                  ( Tuple,
                    (_, [ { e = L (String (metatype_name, t)); _ }; types ], _)
                  );
              _;
            };
          ],
          _ ) ) ->
      let types =
        parse_string_wrap_list_no_env (metatype_name, t) types |> Common.map fst
      in
      (metatype_name, types)
  | _ -> yaml_error_at_expr type_def "expected a dictionary of types"

let parse_generic_metatypes file ast =
  let _t, types =
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
                          (_, [ { e = L (String ("types", _)); _ }; types ], _)
                        );
                    _;
                  };
                ],
                _ ) ) -> (
            match types.G.e with
            | G.Container (G.Array, (l, types, _r)) -> (l, types)
            | _ ->
                yaml_error_at_expr types
                  "expected a list of types following `types:`")
        | _ ->
            let loc = PI.first_loc_of_file file in
            yaml_error (PI.mk_info_of_loc loc)
              "missing types entry as top-level key")
    | _ -> assert false
    (* yaml_to_generic should always return a ExprStmt *)
  in
  let types_tbl = Hashtbl.create 10 in
  let () =
    types
    |> Common.map (fun type_def -> parse_one_type type_def)
    |> List.iter (fun (key, value) -> Hashtbl.add types_tbl key value)
  in
  types_tbl

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

let parse_python_expression env key s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error_at_key env key "not a Python expression"
  with
  | Timeout _ as e -> Exception.catch_and_reraise e
  | UnixExit _n as e -> Exception.catch_and_reraise e
  | exn -> error_at_key env key ("exn: " ^ Common.exn_to_s exn)

let parse_metavar_cond env key s = parse_python_expression env key s

let parse_regexp env (s, t) =
  try Regexp_engine.pcre_compile s with
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
      XP.mk_xpat (XP.Sem (pat, lang)) (str, tok)
  | Xlang.LRegex -> failwith "TODO"
  | Xlang.LGeneric -> (
      let src = Spacegrep.Src_file.of_string str in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> XP.mk_xpat (XP.Spacegrep ast) (str, tok)
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
  | (Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
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
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Pat xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.PatNot (t, xpat)
  | "pattern-comby" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Comby (fst x)) x in
      R.Pat xpat
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-pattern"
  | "metavariable-comparison" ->
      R.PatExtra (t, parse_extra env key value)
  | "pattern-where-python" ->
      raise (R.InvalidRule (R.DeprecatedFeature (fst key), env.id, t))
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> R.PatFilteredInPythonTodo t
  | _ -> error_at_key env key (spf "unexpected key %s" (fst key))

(* NOTE: this is mostly deadcode! None of our rules are using
 * this new formula syntax directly (internally we do convert
 * old style formula to new formula, but we always use the old
 * syntax formula in yaml files).
 *)
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
          (* sanity check fs *)
          let pos, _negs = R.split_and fs in
          if pos = [] then
            raise (R.InvalidRule (R.MissingPositiveTermInAnd, env.id, t));
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
          let xpat = XP.mk_xpat (XP.Regexp (parse_regexp env x)) x in
          R.P (xpat, None)
      | "comby" ->
          let x = parse_string_wrap env key value in
          let xpat = XP.mk_xpat (XP.Comby (fst x)) x in
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
              Right (t, R.CondRegexp (mvar, parse_regexp env x, false))
          | G.Container (Array, (_, [ mvar; re; const_prop ], _)) ->
              let mvar = parse_string env key mvar in
              let x = parse_string_wrap env key re in
              let const_prop = parse_bool env key const_prop in
              Right (t, R.CondRegexp (mvar, parse_regexp env x, const_prop))
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
      let metavar, regexp, const_prop =
        ( take mv_regex_dict env parse_string "metavariable",
          take mv_regex_dict env parse_string_wrap "regex",
          take_opt mv_regex_dict env parse_bool "constant-propagation" )
      in
      R.MetavarRegexp
        ( metavar,
          parse_regexp env regexp,
          match const_prop with
          | Some b -> b
          | None -> false )
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
        ( take_opt mv_comparison_dict env parse_string "metavariable",
          take mv_comparison_dict env parse_string "comparison",
          take_opt mv_comparison_dict env parse_bool "strip",
          take_opt mv_comparison_dict env parse_int "base" )
      in
      let comparison = parse_metavar_cond env key comparison in
      (match (metavariable, strip) with
      | None, Some true ->
          error_at_key env key
            (fst key
           ^ ": 'metavariable' field is missing, but it is mandatory if \
              'strip: true'")
      | __else__ -> ());
      R.MetavarComparison { R.metavariable; comparison; strip; base }
  | _ -> error_at_key env key ("wrong parse_extra field: " ^ fst key)

let parse_language ~id ((s, t) as _lang) : Lang.t =
  match Lang.lang_of_string_opt s with
  | None -> raise (R.InvalidRule (R.InvalidLanguage s, id, t))
  | Some l -> l

let parse_languages ~id langs : Xlang.t =
  match langs with
  | [ (("none" | "regex"), _t) ] -> LRegex
  | [ ("generic", _t) ] -> LGeneric
  | xs -> (
      let languages = xs |> Common.map (parse_language ~id:(fst id)) in
      match languages with
      | [] ->
          raise
            (R.InvalidRule
               (R.InvalidOther "we need at least one language", fst id, snd id))
      | x :: xs -> L (x, xs))

let parse_extract_dest ~id lang : Xlang.t =
  match lang with
  | ("none" | "regex"), _ -> LRegex
  | "generic", _ -> LGeneric
  | lang -> L (parse_language ~id lang, [])

let parse_severity ~id (s, t) =
  match s with
  | "ERROR" -> R.Error
  | "WARNING" -> R.Warning
  | "INFO" -> R.Info
  | "INVENTORY" -> R.Inventory
  | "EXPERIMENT" -> R.Experiment
  | s ->
      raise
        (R.InvalidRule
           ( R.InvalidOther
               (spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s),
             id,
             t ))

let parse_extract_reduction ~id (s, t) =
  match s with
  | "concat" -> R.Concat
  | "separate" -> R.Separate
  | s ->
      raise
        (R.InvalidRule
           ( R.InvalidOther
               (spf "Bad extract reduction: %s (expected concat or separate)" s),
             id,
             t ))

(*****************************************************************************)
(* Sub parsers taint *)
(*****************************************************************************)

let parse_formula (env : env) (rule_dict : dict) : R.pformula =
  match Hashtbl.find_opt rule_dict.h "match" with
  | Some (_matchkey, v) -> R.New (parse_formula_new env v)
  | None ->
      let old = parse_formula_old env (find_formula_old env rule_dict) in
      (* sanity check *)
      let _new = Rule.convert_formula_old ~rule_id:env.id old in
      R.Old old

let parse_taint_requires env key x =
  let parse_error () =
    error_at_key env key "Expected a Boolean (Python) expression over labels"
  in
  let rec check e =
    match e.G.e with
    | G.L (G.Bool (_v, _)) -> ()
    | G.N (G.Id ((str, _), _)) when Metavariable.is_metavar_name str ->
        error_at_key env key ("Metavariables cannot be used as labels: " ^ str)
    | G.N (G.Id (_id, _)) -> ()
    | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg _e1 ], _)) -> ()
    | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
        List.iter check_arg args;
        match op with
        | G.And
        | G.Or ->
            ()
        | _ -> parse_error ())
    | G.ParenExpr (_, e, _) -> check e
    | ___else__ -> parse_error ()
  and check_arg = function
    | G.Arg e -> check e
    | __else_ -> parse_error ()
  in
  let s = parse_string env key x in
  let e = parse_python_expression env key s in
  check e;
  e

let parse_taint_source env (key : key) (value : G.expr) : Rule.taint_source =
  let source_dict = yaml_to_dict env key value in
  let label =
    take_opt source_dict env parse_string "label"
    |> Option.value ~default:R.default_source_label
  in
  let requires =
    let tok = snd key in
    take_opt source_dict env parse_taint_requires "requires"
    |> Option.value ~default:(R.default_source_requires tok)
  in
  let formula = parse_formula env source_dict in
  { formula; label; requires }

let parse_taint_propagator env (key : key) (value : G.expr) :
    Rule.taint_propagator =
  let propagator_dict = yaml_to_dict env key value in
  let from = take propagator_dict env parse_string_wrap "from" in
  let to_ = take propagator_dict env parse_string_wrap "to" in
  let formula = parse_formula env propagator_dict in
  { formula; from; to_ }

let parse_taint_sanitizer env (key : key) (value : G.expr) =
  let sanitizer_dict = yaml_to_dict env key value in
  let not_conflicting =
    take_opt sanitizer_dict env parse_bool "not_conflicting"
    |> Option.value ~default:false
  in
  let formula = parse_formula env sanitizer_dict in
  { R.not_conflicting; formula }

let parse_taint_sink env (key : key) (value : G.expr) : Rule.taint_sink =
  let sink_dict = yaml_to_dict env key value in
  let requires =
    let tok = snd key in
    take_opt sink_dict env parse_taint_requires "requires"
    |> Option.value ~default:(R.default_sink_requires tok)
  in
  let formula = parse_formula env sink_dict in
  { formula; requires }

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  match mode_opt with
  | None
  | Some ("search", _) ->
      let formula = parse_formula env rule_dict in
      `Search formula
  | Some ("taint", _) ->
      let parse_specs parse_spec env key x =
        parse_list env key
          (fun env -> parse_spec env (fst key ^ "list item", snd key))
          x
      in
      let sources, propagators_opt, sanitizers_opt, sinks =
        ( take rule_dict env (parse_specs parse_taint_source) "pattern-sources",
          take_opt rule_dict env
            (parse_specs parse_taint_propagator)
            "pattern-propagators",
          take_opt rule_dict env
            (parse_specs parse_taint_sanitizer)
            "pattern-sanitizers",
          take rule_dict env (parse_specs parse_taint_sink) "pattern-sinks" )
      in
      `Taint
        {
          sources;
          propagators = optlist_to_list propagators_opt;
          sanitizers = optlist_to_list sanitizers_opt;
          sinks;
        }
  | Some ("extract", _) ->
      let pformula = parse_formula env rule_dict in
      let dst_lang =
        take rule_dict env parse_string_wrap "dest-language"
        |> parse_extract_dest ~id:env.id
      in
      (* TODO: determine fmt---string with interpolated metavars? *)
      let extract = take rule_dict env parse_string "extract" in
      let reduce =
        take_opt rule_dict env parse_string_wrap "reduce"
        |> Option.map (parse_extract_reduction ~id:env.id)
        |> Option.value ~default:R.Separate
      in
      `Extract { pformula; dst_lang; extract; reduce }
  | Some key ->
      error_at_key env key
        (spf
           "Unexpected value for mode, should be 'search', 'taint', or \
            'extract', not %s"
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
  let mode_opt = take_opt rd env parse_string_wrap "mode" in
  let mode = parse_mode env mode_opt rd in
  let ( (message, severity),
        metadata_opt,
        fix_opt,
        fix_regex_opt,
        paths_opt,
        equivs_opt,
        options_opt ) =
    ( (match mode with
      | `Extract _ -> ("", ("INFO", PI.unsafe_fake_info ""))
      | _ ->
          ( take rd env parse_string "message",
            take rd env parse_string_wrap "severity" )),
      take_opt rd env generic_to_json "metadata",
      take_opt rd env parse_string "fix",
      take_opt rd env parse_fix_regex "fix-regex",
      take_opt rd env parse_paths "paths",
      take_opt rd env parse_equivalences "equivalences",
      take_opt rd env parse_options "options" )
  in
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

(* file_kind should be "rules" or "templates" *)
let parse_rules_or_templates file_kind file ast =
  let t, xs =
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
                            [ { e = L (String (top_level_word, _)); _ }; xs ],
                            _ ) );
                    _;
                  };
                ],
                _ ) )
          when top_level_word = file_kind -> (
            match xs.G.e with
            | G.Container (G.Array, (l, xs, _r)) -> (l, xs)
            | _ ->
                yaml_error_at_expr xs
                  (spf "expected a list of %s following `%s:`" file_kind
                     file_kind))
        (* it's also ok to not have the toplevel rules:, anyway we never
         * used another toplevel key
         *)
        | G.Container (G.Dict, (l, _, _)) -> (l, [ e ])
        | G.Container (G.Array, (l, xs, _r)) -> (l, xs)
        (* otherwise, it's bad, and complain *)
        | _ ->
            let loc = PI.first_loc_of_file file in
            yaml_error (PI.mk_info_of_loc loc)
              "missing rules entry as top-level key")
    | _ -> assert false
    (* yaml_to_generic should always return a ExprStmt *)
  in
  (t, xs)

let parse_generic ?(error_recovery = false) file ast =
  let t, rules = parse_rules_or_templates "rules" file ast in
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

(*****************************************************************************)
(* Translation to jsonnet *)
(*****************************************************************************)

(* Converts templates that have been parsed into the generic ast to jsonnet

   The YAML template format looks like:
   ```
   templates:
   - declaration: $FUNCTION_NAME
     contents: $YAML
   - ...
   ```

   And gets translated, after the yaml is jsonified to $JSON, to

   `{ $FUNCTION_NAME :: $JSON, ... }

   As an example, you could have
   ```
   templates:
   - declaration: sources()
     contents:
        pattern: $X
   ```

   This would get translated to

   `{ sources() :: { pattern: "$X" }`

   Right now, this feature is limited by what can be expressed in YAML. But
   we could make it way more expressive by adding an option to insert pure
   jsonnet instead of contents! TODO for later
*)
let parse_template file template_ast =
  let t, templates = parse_rules_or_templates "templates" file template_ast in
  let xs =
    templates
    |> List.mapi (fun i template ->
           let td = yaml_to_dict_no_env ("templates", t) template in
           let declaration =
             take_no_env td parse_string_wrap_no_env "declaration" |> fst
           in
           let env =
             {
               id = declaration;
               languages = Xlang.LGeneric;
               path = [ string_of_int i; "templates" ];
             }
           in
           let contents =
             let content_json =
               take_no_env td (fun k e -> generic_to_json env k e) "contents"
             in
             match content_json with
             | Array _ | Object _ -> 
                 (* interpret as the json object *)
                  JSON.string_of_json content_json
             | String x -> 
                 (* interpret as pure jsonnet *)
                 x 
             | _ -> failwith "Not valid json format"
           in
           spf "   %s :: %s,\n" declaration contents)
  in
  (* The `\n`s here and above aren't necessary, but they add little
     and make the templates much more readable *)
  spf "{\n%s\n}" (String.concat "\n" xs)

(* Creates a jsonnet string for the parsed imports

   Imports in the yaml look like:
   ```
      uses:
        - $NAME: $PATH
        - ...
   ```

   where $PATH is a string.

   This produces

   ```
   local $NAME = import '$PATH';
   ...
   ```
*)
let create_imports libsonnet_defs =
  let create_import (name, path) = spf "local %s = import '%s';" name path in
  let imported_folder =
    match libsonnet_defs with
    | [] ->
        failwith
          "No import definitions (given by the uses key) even though we are \
           creating imports for the import definitions. This should be \
           impossible"
    | (_, path) :: _ -> Filename.dirname path
  in
  let imports = String.concat "\n" (List.map create_import libsonnet_defs) in
  (imports, imported_folder)

let parse_uses uses uses_key =
  let env = { id = "templates"; languages = Xlang.LGeneric; path = [] } in
  let parse_use use =
    match use with
    | {
     G.e =
       G.Container
         ( Dict,
           ( _,
             [
               {
                 e =
                   Container
                     ( Tuple,
                       ( _,
                         [
                           { e = L (String (name, _)); _ };
                           { e = L (String (path, _)); _ };
                         ],
                         _ ) );
                 _;
               };
             ],
             _ ) );
     _;
    } ->
        (* TODO name cannot contain hyphens; add a warning
           or figure out how to support this *)
        (name, path)
    | _ ->
        error_at_expr env uses
          "An entry in `uses` should be a mapping from the nickname of the \
           library to the path of the library (aka a string)"
  in
  parse_list_no_env uses_key parse_use uses

(* Note on ~unescape_strings:true:
   *
   * in a parsing-rule context, we don't want the parsed strings by
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
*)
let parse_json file =
  Json_to_generic.program ~unescape_strings:true (Parse_json.parse_program file)

let parse_as_jsonnet file tmpfile =
  (* TODO any error message this gives is going to be horrible *)
  let cmd = spf "jsonnet %s -o %s" file tmpfile in
  let n = Sys.command cmd in
  if n <> 0 then failwith (spf "error executing %s" cmd);
  (* TODO these error messages will be better but still bad *)
  Json_to_generic.program ~unescape_strings:true
    (Parse_json.parse_program tmpfile)

let parse_templated_rules rules_with_templates rules_keyword =
  (* Separate templates from templated rules *)
  let rules, template_defs =
    List.fold_right
      (fun rule (acc_rules, acc_templates) ->
        let rules, templates = rule in
        (rules :: acc_rules, templates :: acc_templates))
      rules_with_templates ([], [])
  in
  let template_defs = List.flatten template_defs in

  (* Create the libsonnet file for each template *)
  let libsonnet_defs =
    List.map
      (fun (name, yaml_path) ->
        (* TODO Support JSON as well *)
        let template_ast = Yaml_to_generic.program yaml_path in
        let template = parse_template yaml_path template_ast in
        let libsonnet_path = Filename.chop_extension yaml_path ^ ".libsonnet" in
        write_file ~file:libsonnet_path template;
        (name, libsonnet_path))
      template_defs
  in

  (* Generate the Jsonnet string *)
  (* - Replace `{"ref": $ref}` with $ref
     - Prepend the imports *)
  let dummy_id = "JSON of rules generated by semgrep-core" in
  let env = { id = dummy_id; languages = Xlang.LGeneric; path = [] } in
  let t = snd rules_keyword in
  let rules =
    G.Container (G.Array, (t, rules, Parse_info.fake_info t "]")) |> G.e
  in
  let jsonnet_string =
    rules
    |> generic_to_json env rules_keyword
    |> JSON.string_of_json
    (* TODO MAJOR LIMITATION this does not work with string literals and I cannot
       figure out how to make it work *)
    |> Str.global_replace (Str.regexp {|{"ref":"\([^\"]*\)"}|}) {|\1|}
  in
  pr2 jsonnet_string;
  let imports, jsonnet_folder = create_imports libsonnet_defs in
  let jsonnet_string = imports ^ "\n" ^ jsonnet_string in

  (* Write to jsonnet file *)
  let jsonnet_filename = jsonnet_folder ^ "/" ^ "rules.jsonnet" in
  write_file ~file:jsonnet_filename jsonnet_string;

  (* Run jsonnet *)
  let rules_filename = jsonnet_folder ^ "/" ^ "compiled_rules.json" in
  parse_as_jsonnet jsonnet_filename rules_filename

(* TODO add an explanation, also rename this function *)
(* Notes on performance and error reporting: *)
let parse_possibly_templated_rules error_recovery file ast =
  let uses_def field =
    match field.G.e with
    | G.Container (G.Tuple, (_, [ { e = L (String ("uses", t)); _ }; value ], _))
      ->
        Some (parse_uses value ("uses", t))
    | _ -> None
  in

  let split_rules_into_normal_or_templated rules_ast =
    let t, rules = parse_rules_or_templates "rules" file rules_ast in
    let normal_rules, templated_rules =
      rules
      |> List.partition_map (fun rule ->
             match rule.G.e with
             | G.Container (Dict, (l, fields, r)) -> (
                 match List.find_map uses_def fields with
                 | None ->
                     (* Normal rule, just return it *)
                     Left rule
                 | Some uses ->
                     (* Templated rule, return the template defs
                        and the rules without the uses *)
                     let fields =
                       List.filter
                         (fun field ->
                           (* TODO This will unnecesarily parse the uses a second time *)
                           match uses_def field with
                           | None -> true
                           | Some _ -> false)
                         fields
                     in
                     let rule = G.Container (Dict, (l, fields, r)) |> G.e in
                     Right (rule, uses))
             | _ -> yaml_error_at_expr rule "rules")
    in
    (normal_rules, templated_rules, t)
  in
  (* Separate rules that require templates for further processing *)
  let normal_rules, templated_rules, t =
    split_rules_into_normal_or_templated ast
  in
  (* For simplicity, wrap the normal rules as a list instead of a dict *)
  let recreated_normal_rules =
    [
      G.ExprStmt (G.Container (G.Array, (t, normal_rules, t)) |> G.e, t) |> G.s;
    ]
  in
  (* TODO: error messages will be ok within individual rules because the
     tokens for each rule are real, but they will be bad for levels above.
     This should be fine but needs to be tested *)
  let parsed_templated_rules, templated_errors =
    parse_templated_rules templated_rules ("rules", t) |> parse_generic file
  in
  let parsed_normal_rules, normal_errors =
    parse_generic ~error_recovery file recreated_normal_rules
  in
  ( parsed_normal_rules @ parsed_templated_rules,
    templated_errors @ normal_errors )

let translate_to_jsonnet_and_parse ?(error_recovery = false) file =
  (* Semgrep templated rules are actually a thin wrapper for jsonnet.
   * Rules that invoke templates are converted to jsonnet, and the
   * templates to libsonnet. We do this instead of just writing jsonnet
   * directly because we already have a tradition of YAML rules, and this
   * method allows us to implement templated rules while being compatible
   * with all our existing infrastructure. However, it certainly must be
   * observed that it would be simpler to just write all rules in jsonnet
   *)
  match FT.file_type_of_file file with
  | FT.Config FT.Json ->
      file |> parse_json |> parse_possibly_templated_rules error_recovery file
  | FT.Config FT.Jsonnet ->
      Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun x ->
          parse_as_jsonnet file x |> parse_generic file)
  | FT.Config FT.Yaml ->
      logger#warning "Templated rules are not supported in core for YAML\n";
      Yaml_to_generic.parse_rule file
      |> parse_possibly_templated_rules error_recovery file
  | _ ->
      logger#error "wrong rule format, only YAML/JSON/JSONNET is valid";
      logger#info "trying to parse %s as YAML" file;
      Yaml_to_generic.parse_rule file
      |> parse_possibly_templated_rules error_recovery file

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_metatypes file =
  let ast =
    match FT.file_type_of_file file with
    | FT.Config FT.Yaml -> Yaml_to_generic.parse_rule file
    | _ ->
        logger#error "wrong rule format, only YAML is valid";
        logger#info "trying to parse %s as YAML" file;
        Yaml_to_generic.parse_rule file
  in
  parse_generic_metatypes file ast

let parse file =
  let xs, skipped = translate_to_jsonnet_and_parse ~error_recovery:false file in
  assert (skipped = []);
  xs

let parse_and_filter_invalid_rules file =
  translate_to_jsonnet_and_parse ~error_recovery:true file
