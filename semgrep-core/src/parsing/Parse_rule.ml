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
module J = JSON
module FT = File_type
module R = Rule
module XP = Xpattern
module MR = Mini_rule
module G = AST_generic
module PI = Parse_info
module Set = Set_
module MV = Metavariable

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema in rule_schema_v1.yaml.
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
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  (* id of the current rule (needed by some exns) *)
  id : Rule.rule_id;
  (* languages of the current rule (needed by parse_pattern) *)
  languages : Xlang.t;
  (* whether we are underneath a `metavariable-pattern` *)
  in_metavariable_pattern : bool;
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
let take_opt (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a option =
  Option.map
    (fun (key, value) ->
      let res = f env key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let take (dict : dict) (env : env) (f : env -> key -> G.expr -> 'a)
    (key_str : string) : 'a =
  match take_opt dict env f key_str with
  | Some res -> res
  | None -> error env dict.first_tok ("Missing required field " ^ key_str)

let fold_dict f dict x = Hashtbl.fold f dict.h x

let yaml_to_dict env enclosing =
  yaml_to_dict_helper (error_at_expr env) (error_at_expr env) enclosing

(*****************************************************************************)
(* Parsing methods for before env is created *)
(*****************************************************************************)

(* Mutates the Hashtbl! *)
let take_opt_no_env (dict : dict) (f : key -> G.expr -> 'a) (key_str : string) :
    'a option =
  Option.map
    (fun (key, value) ->
      let res = f key value in
      Hashtbl.remove dict.h key_str;
      res)
    (Hashtbl.find_opt dict.h key_str)

(* Mutates the Hashtbl! *)
let take_no_env (dict : dict) (f : key -> G.expr -> 'a) (key_str : string) : 'a
    =
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
(* Parsers for basic types *)
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

let parse_listi env (key : key) f x =
  let get_component i x =
    let env = { env with path = string_of_int i :: env.path } in
    f env x
  in
  match x.G.e with
  | G.Container (Array, (_, xs, _)) -> List.mapi get_component xs
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

let parse_str_or_dict env (value : G.expr) : (G.ident, dict) Either.t =
  match value.G.e with
  | G.L (String (value, t)) ->
      (* should use the unescaped string *)
      Either.Left (value, t)
  | G.L (Float (Some n, t)) ->
      if Float.is_integer n then Left (string_of_int (Float.to_int n), t)
      else Left (string_of_float n, t)
  | G.N (Id ((value, t), _)) -> Left (value, t)
  | G.Container (Dict, _) ->
      Right (yaml_to_dict env ("<TODO>", G.fake "<TODO>") value)
  | _ ->
      error_at_expr env value
        "Wrong field for a pattern, expected string or dictionary"

(* env: general data about the current rule
 * key: the word `focus-metavariable` from the original rule.
 * x: the AST expression for the values in the rule that are under
 *    `focus-metavariable`.
 *)
let parse_focus_mvs env (key : key) (x : G.expr) =
  match x.e with
  | G.N (G.Id ((s, _), _))
  | G.L (String (s, _)) ->
      [ s ]
  | G.Container (Array, (_, mvs, _)) ->
      Common.map (fun mv -> fst (parse_string_wrap env key mv)) mvs
  | _ ->
      error_at_key env key
        ("Expected a string or a list of strings for " ^ fst key)

(*****************************************************************************)
(* Parsers for core fields (languages:, severity:) *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Parsers for extra (metavar-xxx:, fix:, etc.) *)
(*****************************************************************************)

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
(* Parser for xpattern *)
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

(*****************************************************************************)
(* Parser for old (but current) formula *)
(*****************************************************************************)

(* This was in Rule.ml before and represent the old (but still current)
 * way to write metavariable conditions.
 *)
(* extra conditions, usually on metavariable content *)
type extra =
  | MetavarRegexp of MV.mvar * Xpattern.regexp * bool
  | MetavarPattern of MV.mvar * Xlang.t option * Rule.formula
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of MV.mvar * Rule.metavar_analysis_kind
(* old: | PatWherePython of string, but it was too dangerous.
 * MetavarComparison is not as powerful, but safer.
 *)

(* See also engine/Eval_generic.ml *)
and metavariable_comparison = {
  metavariable : MV.mvar option;
  comparison : AST_generic.expr;
  (* I don't think those are really needed; they can be inferred
   * from the values *)
  strip : bool option;
  base : int option;
}

(* Substitutes `$MVAR` with `int($MVAR)` in cond. *)
(* This now changes all such metavariables. We expect in most cases there should
   just be one, anyways.
*)
let rewrite_metavar_comparison_strip cond =
  let visitor =
    Map_AST.mk_visitor
      {
        Map_AST.default_visitor with
        Map_AST.kexpr =
          (fun (k, _) e ->
            (* apply on children *)
            let e = k e in
            match e.G.e with
            | G.N (G.Id ((s, tok), _idinfo)) when Metavariable.is_metavar_name s
              ->
                let py_int = G.Id (("int", tok), G.empty_id_info ()) in
                G.Call (G.N py_int |> G.e, G.fake_bracket [ G.Arg e ]) |> G.e
            | _ -> e);
      }
  in
  visitor.Map_AST.vexpr cond

(* TODO: Old stuff that we can't kill yet. *)
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

let rec parse_formula_old_from_dict (env : env) (rule_dict : dict) : R.formula =
  let formula = parse_pair_old env (find_formula_old env rule_dict) in
  (* sanity check *)
  (* bTODO: filter out unconstrained nots *)
  formula

and parse_pair_old env ((key, value) : key * G.expr) : R.formula =
  let env = { env with path = fst key :: env.path } in
  let parse_listi env (key : key) f x =
    match x.G.e with
    | G.Container (Array, (_, xs, _)) -> List.mapi f xs
    | _ -> error_at_key env key ("Expected a list for " ^ fst key)
  in
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
        parse_pair_old env (key, value)
    | _ -> error_at_expr env x "Wrong parse_formula fields"
  in
  let s, t = key in
  match s with
  | "pattern" -> R.P (get_pattern value)
  | "pattern-not" -> R.Not (t, R.P (get_pattern value))
  | "pattern-inside" -> R.Inside (t, R.P (get_pattern value))
  | "pattern-not-inside" -> R.Not (t, R.Inside (t, R.P (get_pattern value)))
  | "pattern-either" -> R.Or (t, parse_listi env key get_nested_formula value)
  | "patterns" ->
      let parse_pattern _i expr =
        match parse_str_or_dict env expr with
        | Left (_s, _t) -> failwith "use patterns:"
        | Right dict -> (
            let find key_str = Hashtbl.find_opt dict.h key_str in
            let process_extra extra =
              match extra with
              | MetavarRegexp (mvar, regex, b) -> R.CondRegexp (mvar, regex, b)
              | MetavarPattern (mvar, xlang_opt, formula) ->
                  R.CondNestedFormula (mvar, xlang_opt, formula)
              | MetavarComparison { comparison; strip; _ } ->
                  R.CondEval
                    (match strip with
                    (* TODO *)
                    | Some true -> rewrite_metavar_comparison_strip comparison
                    | _ -> comparison)
              | MetavarAnalysis (mvar, kind) -> R.CondAnalysis (mvar, kind)
            in
            match
              ( find "focus-metavariable",
                find "metavariable-analysis",
                find "metavariable-regex",
                find "metavariable-pattern",
                find "metavariable-comparison" )
            with
            | None, None, None, None, None -> Left3 (get_nested_formula 0 expr)
            | Some (((_, t) as key), value), None, None, None, None ->
                Middle3 (t, parse_focus_mvs env key value)
            | None, Some (key, value), None, None, None
            | None, None, Some (key, value), None, None
            | None, None, None, Some (key, value), None
            | None, None, None, None, Some (key, value) ->
                Right3 (snd key, parse_extra env key value |> process_extra)
            | _ ->
                error_at_expr env expr
                  "should not happen, expected\n\
                  \            one of `metavariable-analysis`, \
                   `metavariable-regex`, or `metavariable-comparison`")
      in
      let conjuncts, focus, conditions =
        Common.partition_either3
          (fun x -> x)
          (parse_listi env key parse_pattern value)
      in
      let pos, _ = R.split_and conjuncts in
      if pos = [] && not env.in_metavariable_pattern then
        raise (R.InvalidRule (R.MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus; conditions })
  | "pattern-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Not (t, R.P xpat)
  | "pattern-comby" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Comby (fst x)) x in
      R.P xpat
  | "focus-metavariable"
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-pattern"
  | "metavariable-comparison" ->
      error_at_key env key "Must occur directly under a patterns:"
  | "pattern-where-python" ->
      raise (R.InvalidRule (R.DeprecatedFeature (fst key), env.id, t))
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> failwith "TODO"
  | _ -> error_at_key env key (spf "unexpected key %s" (fst key))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) : extra =
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
      MetavarAnalysis (metavar, kind)
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
      MetavarRegexp
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
                in_metavariable_pattern = env.in_metavariable_pattern;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let env' = { env' with in_metavariable_pattern = true } in
      let formula_old =
        parse_pair_old env' (find_formula_old env mv_pattern_dict)
      in
      MetavarPattern (metavar, opt_xlang, formula_old)
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
      MetavarComparison { metavariable; comparison; strip; base }
  | _ -> error_at_key env key ("wrong parse_extra field: " ^ fst key)

(*****************************************************************************)
(* Parser for new  formula *)
(*****************************************************************************)

let find_formula env (rule_dict : dict) : key * G.expr =
  let find key_str = Hashtbl.find_opt rule_dict.h key_str in
  match
    find_some_opt find
      [ "pattern"; "and"; "or"; "regex"; "taint"; "not"; "inside" ]
  with
  | None ->
      error env rule_dict.first_tok
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, `pattern-comby` to be present"
  | Some (key, value) -> (key, value)

(* intermediate type used for processing 'where' *)
type principal_constraint = Ccompare | Cfocus | Cmetavar | Canalyzer

let find_constraint dict =
  fold_dict
    (fun s ((_, tok), _) -> function
      | Some res -> Some res
      | None -> (
          match s with
          | "comparison" -> Some (tok, Ccompare)
          | "focus" -> Some (tok, Cfocus)
          | "analyzer" -> Some (tok, Canalyzer)
          (* This can appear in a metavariable-analysis as well, but it shouldn't
             matter since this case occurs after the `analyzer` one. *)
          | "metavariable" -> Some (tok, Cmetavar)
          | _ -> None))
    dict None

let rec parse_formula_from_dict (env : env) (rule_dict : dict) : R.formula =
  let formula = parse_pair env (find_formula env rule_dict) in
  (* sanity check *)
  (* bTODO: filter out unconstrained nots *)
  formula

and parse_formula env (value : G.expr) : R.formula =
  (* First, try to parse as a string *)
  match parse_str_or_dict env value with
  | Left (s, t) ->
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
      R.P
        (try parse_xpattern env.languages (s, t) with
        | (Timeout _ | UnixExit _) as e -> Exception.catch_and_reraise e
        (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
        | exn ->
            raise
              (R.InvalidRule
                 ( R.InvalidPattern
                     (s, env.languages, Common.exn_to_s exn, env.path),
                   env.id,
                   t )))
  (* If that doesn't work, it should be a key-value pairing.
   *)
  | Right dict -> (
      (* This is ugly, but here's why. *)
      (* First, we need to figure out if there's a `where`. *)
      let where_formula =
        take_opt dict env (fun _env key value -> (key, value)) "where"
      in
      match where_formula with
      (* If there's a `where`, then there must be one key left, the other of which is the
         pattern. *)
      | _ when Hashtbl.length dict.h <> 1 ->
          error env dict.first_tok
            "Expected exactly one key of `pattern`, `pattern-either`, \
             `patterns`, `pattern-regex`, or `pattern-comby`"
      (* Otherwise, use the where formula if it exists, to modify the formula we know must exist. *)
      | None -> parse_pair env (find_formula env dict)
      | Some (((_, t) as key), value) ->
          parse_pair env (find_formula env dict)
          |> constrain_where env (t, t) key value)

and produce_constraint env dict tok indicator =
  match indicator with
  | Ccompare ->
      (* comparison: ...
         [strip: ...]
         [base: ...]
      *)
      let ((s, t) as compare_key) =
        take dict env parse_string_wrap "comparison"
      in
      let cond = parse_metavar_cond env compare_key s in
      (* This base is pretty unnecessary. *)
      let strip, _base =
        ( take_opt dict env parse_bool "strip",
          take_opt dict env parse_int "base" )
      in
      let cond =
        (* if strip=true we rewrite the condition and insert Python's `int`
            * function to parse the integer value of mvar. *)
        match strip with
        | Some true -> rewrite_metavar_comparison_strip cond
        | _ -> cond
      in
      Left (t, R.CondEval cond)
  | Cfocus ->
      (* focus: ...
       *)
      let mv_list = take dict env parse_focus_mvs "focus" in
      Right (tok, mv_list)
  | Canalyzer ->
      (* metavariable: ...
         analyzer: ...
      *)
      let metavar, t = take dict env parse_string_wrap "metavariable" in
      let analyzer, analyze_t = take dict env parse_string_wrap "analyzer" in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "redos" -> R.CondReDoS
        | other ->
            error_at_key env ("analyzer", analyze_t)
              ("Unsupported analyzer: " ^ other)
      in
      Left (t, CondAnalysis (metavar, kind))
  | Cmetavar -> (
      (* metavariable: ...
         <pattern-pair>
         [language: ...]
      *)
      let metavar, t = take dict env parse_string_wrap "metavariable" in
      let env', opt_xlang =
        match take_opt dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~id:(Some env.id) s in
            let env' =
              {
                env with
                languages = xlang;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let formula = parse_pair env' (find_formula env dict) in
      match formula with
      | R.P { pat = Xpattern.Regexp regexp; _ } ->
          (* TODO: always on by default *)
          Left (t, CondRegexp (metavar, regexp, true))
      | _ -> Left (t, CondNestedFormula (metavar, opt_xlang, formula)))

and constrain_where env (t1, _t2) where_key (value : G.expr) formula : R.formula
    =
  let env = { env with path = "where" :: env.path } in
  (* TODO: first token, or principal token? *)
  let parse_where_pair env (where_value : G.expr) =
    let dict = yaml_to_dict env where_key where_value in
    match find_constraint dict with
    | Some (tok, indicator) -> produce_constraint env dict tok indicator
    | _ -> error_at_expr env value "Wrong where constraint fields"
  in
  (* TODO *)
  let conditions, focus =
    parse_listi env where_key parse_where_pair value
    |> Common.partition_either (fun x -> x)
  in
  let tok, conditions, focus, conjuncts =
    (* If the modified pattern is also an `And`, collect the conditions and focus
        and fold them together.
    *)
    match formula with
    | And (tok, { conjuncts; conditions = conditions2; focus = focus2 }) ->
        (tok, conditions @ conditions2, focus @ focus2, conjuncts)
    (* Otherwise, we consider the modified pattern a degenerate singleton `And`.
    *)
    | _ -> (t1, conditions, focus, [ formula ])
  in
  R.And (tok, { conjuncts; conditions; focus })

and parse_pair env ((key, value) : key * G.expr) : R.formula =
  let env = { env with path = fst key :: env.path } in
  let get_string_pattern str_e = parse_xpattern_expr env str_e in
  let s, t = key in
  match s with
  | "pattern" -> R.P (get_string_pattern value)
  | "not" -> R.Not (t, parse_formula env value)
  | "inside" -> R.Inside (t, parse_formula env value)
  | "and" ->
      let conjuncts = parse_listi env key parse_formula value in
      let pos, _ = R.split_and conjuncts in
      if pos = [] && not env.in_metavariable_pattern then
        raise (R.InvalidRule (R.MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus = []; conditions = [] })
  | "or" -> R.Or (t, parse_listi env key parse_formula value)
  | "regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | _ -> error_at_key env key (spf "unexpected key %s" (fst key))

(*****************************************************************************)
(* Parsers for taint *)
(*****************************************************************************)

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

(* TODO: can add a case where these take in only a single string *)
let parse_taint_source ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_source =
  let f =
    if is_old then parse_formula_old_from_dict else parse_formula_from_dict
  in
  let source_dict = yaml_to_dict env key value in
  let label =
    take_opt source_dict env parse_string "label"
    |> Option.value ~default:R.default_source_label
  in
  let source_requires =
    let tok = snd key in
    take_opt source_dict env parse_taint_requires "requires"
    |> Option.value ~default:(R.default_source_requires tok)
  in
  let source_formula = f env source_dict in
  { source_formula; label; source_requires }

let parse_taint_propagator ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_propagator =
  let f =
    if is_old then parse_formula_old_from_dict else parse_formula_from_dict
  in
  let propagator_dict = yaml_to_dict env key value in
  let from = take propagator_dict env parse_string_wrap "from" in
  let to_ = take propagator_dict env parse_string_wrap "to" in
  let propagator_formula = f env propagator_dict in
  { propagator_formula; from; to_ }

let parse_taint_sanitizer ~(is_old : bool) env (key : key) (value : G.expr) =
  let f =
    if is_old then parse_formula_old_from_dict else parse_formula_from_dict
  in
  let sanitizer_dict = yaml_to_dict env key value in
  let not_conflicting =
    take_opt sanitizer_dict env parse_bool
      (if is_old then "not_conflicting" else "not-conflicting")
    |> Option.value ~default:false
  in
  let sanitizer_formula = f env sanitizer_dict in
  { R.not_conflicting; sanitizer_formula }

let parse_taint_sink ~(is_old : bool) env (key : key) (value : G.expr) :
    Rule.taint_sink =
  let f =
    if is_old then parse_formula_old_from_dict else parse_formula_from_dict
  in
  let sink_dict = yaml_to_dict env key value in
  let sink_requires =
    let tok = snd key in
    take_opt sink_dict env parse_taint_requires "requires"
    |> Option.value ~default:(R.default_sink_requires tok)
  in
  let sink_formula = f env sink_dict in
  { sink_formula; sink_requires }

(*****************************************************************************)
(* Parsers for extract mode *)
(*****************************************************************************)

let parse_extract_dest ~id lang : Xlang.t =
  match lang with
  | ("none" | "regex"), _ -> LRegex
  | "generic", _ -> LGeneric
  | lang -> L (parse_language ~id lang, [])

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
(* Main entry point *)
(*****************************************************************************)

let parse_mode env mode_opt (rule_dict : dict) : R.mode =
  match mode_opt with
  | None
  | Some ("search", _) -> (
      let formula =
        take_opt rule_dict env
          (fun env _ expr -> parse_formula env expr)
          "match"
      in
      match formula with
      | Some formula -> `Search formula
      | None -> `Search (parse_pair_old env (find_formula_old env rule_dict)))
  | Some ("taint", _) ->
      let parse_specs parse_spec env key x =
        ( snd key,
          parse_list env key
            (fun env -> parse_spec env (fst key ^ "list item", snd key))
            x )
      in
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
          sanitizers =
            (match sanitizers_opt with
            | None -> []
            | Some (_, xs) -> xs);
          sinks;
        }
  | Some ("extract", _) ->
      let formula = parse_pair_old env (find_formula_old env rule_dict) in
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
      `Extract { formula; dst_lang; extract; reduce }
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

let parse_one_rule (t : G.tok) (i : int) (rule : G.expr) : Rule.t =
  let rd = yaml_to_dict_no_env ("rules", t) rule in
  let id, languages =
    ( take_no_env rd parse_string_wrap_no_env "id",
      take_no_env rd parse_string_wrap_list_no_env "languages" )
  in
  let languages = parse_languages ~id languages in
  let env =
    {
      id = fst id;
      languages;
      in_metavariable_pattern = false;
      path = [ string_of_int i; "rules" ];
    }
  in
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

let parse_generic_ast ?(error_recovery = false) (file : Common.filename)
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
          (Parse_json.parse_program file)
    | FT.Config FT.Jsonnet ->
        Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
            let cmd = spf "jsonnet -J vendor %s -o %s" file tmpfile in
            let n = Sys.command cmd in
            if n <> 0 then failwith (spf "error executing %s" cmd);
            Json_to_generic.program ~unescape_strings:true
              (Parse_json.parse_program tmpfile))
    | FT.Config FT.Yaml -> Yaml_to_generic.parse_rule file
    | _ ->
        logger#error "wrong rule format, only JSON/YAML/JSONNET are valid";
        logger#info "trying to parse %s as YAML" file;
        Yaml_to_generic.parse_rule file
  in
  parse_generic_ast ?error_recovery file ast

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  let xs, skipped = parse_file ~error_recovery:false file in
  assert (skipped = []);
  xs

let parse_and_filter_invalid_rules file = parse_file ~error_recovery:true file
