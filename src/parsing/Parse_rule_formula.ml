(* Yoann Padioleau, Emma Jin, Brandon Wu
 *
 * Copyright (C) 2019-2023 Semgrep Inc.
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
open Either_
open Parse_rule_helpers
module XP = Xpattern
module MV = Metavariable

(*****************************************************************************)
(* Focus metavariable *)
(*****************************************************************************)

(* env: general data about the current rule
 * key: the word `focus-metavariable` from the original rule.
 * x: the AST expression for the values in the rule that are under
 *    `focus-metavariable`.
 *)
let parse_focus_mvs env (key : key) (x : G.expr) =
  match x.e with
  | G.N (G.Id ((s, _), _))
  | G.L (String (_, (s, _), _)) ->
      [ s ]
  | G.Container (Array, (_, mvs, _)) ->
      List_.map (fun mv -> fst (parse_string_wrap env key mv)) mvs
  | _ ->
      error_at_key env.id key
        ("Expected a string or a list of strings for " ^ fst key)

(*****************************************************************************)
(* Check the validity of the Aliengrep options *)
(*****************************************************************************)

(* Aliengrep word characters must be single-byte characters for now. *)
let word_chars_of_strings env xs =
  xs
  |> List_.map (function
       | "" -> error_at_opt_key env.id env.options_key "Empty opening brace"
       | x when String.length x =|= 1 (* = *) -> x.[0]
       | long ->
           error_at_opt_key env.id env.options_key
             (spf "Multibyte word characters aren't supported: %S" long))

(* Aliengrep brace pairs are specified as strings because ATD doesn't have
   a char type and because they could be multibyte UTF-8-encoded characters.
   For now, aliengrep only supports single-byte characters. *)
let brace_pairs_of_string_pairs env xs =
  xs
  |> List_.map (fun (open_, close) ->
         let opening_char =
           match open_ with
           | "" -> error_at_opt_key env.id env.options_key "Empty opening brace"
           | x -> x.[0]
         in
         let closing_char =
           match close with
           | "" -> error_at_opt_key env.id env.options_key "Empty closing brace"
           | x -> x.[0]
         in
         if String.length open_ > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte opening braces aren't supported: %S" open_);
         if String.length close > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte closing braces aren't supported: %S" close);
         (opening_char, closing_char))

let aliengrep_conf_of_options (env : env) : Aliengrep.Conf.t =
  let options = Option.value env.options ~default:Rule_options.default_config in
  let default = Aliengrep.Conf.default_multiline_conf in
  let caseless = options.generic_caseless in
  let multiline = options.generic_multiline in
  let word_chars =
    default.word_chars
    @ word_chars_of_strings env options.generic_extra_word_characters
  in
  let brackets =
    let base_set =
      match options.generic_braces with
      | None -> default.brackets
      | Some string_pairs -> brace_pairs_of_string_pairs env string_pairs
    in
    let extra_braces =
      brace_pairs_of_string_pairs env options.generic_extra_braces
    in
    base_set @ extra_braces
  in
  { caseless; multiline; word_chars; brackets }

(*****************************************************************************)
(* Type parsing *)
(*****************************************************************************)

let rec parse_type env key (str, tok) =
  match env.target_analyzer with
  | Xlang.L (lang, _) ->
      let str = wrap_type_expr env key lang str in
      try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
          Parse_pattern.parse_pattern lang ~print_errors:false str)
      |> unwrap_type_expr env key lang
  | Xlang.LRegex
  | Xlang.LSpacegrep
  | Xlang.LAliengrep ->
      error_at_key env.id key
        "`type` is not supported with regex, spacegrep or aliengrep."

and wrap_type_expr env key lang str =
  match Parse_metavariable_type.wrap_type_expr lang str with
  | Some x -> x
  | None ->
      error_at_key env.id key
        ("`metavariable-type` is not supported for " ^ Lang.show lang)

and unwrap_type_expr env key lang expr =
  match Parse_metavariable_type.unwrap_type_expr lang expr with
  | Some x -> x
  | None ->
      error_at_key env.id key
        ("Failed to unwrap the type expression." ^ G.show_any expr)

(*****************************************************************************)
(* Parser for xpattern *)
(*****************************************************************************)

(* less: could move in a separate Parse_xpattern.ml *)
let parse_rule_xpattern env (str, tok) =
  match env.target_analyzer with
  | Xlang.L (lang, _) ->
      (* opti: parsing Semgrep patterns lazily improves speed significantly.
       * Parsing p/default goes from 13s to just 0.2s, mostly because
       * p/default contains lots of ruby rules which are currently very
       * slow to parse. Still, even if there was no Ruby rule, it's probably
       * still worth the optimization.
       * The disadvantage of parsing lazily is that
       * parse errors in the pattern are detected only later, when
       * the rule/pattern is actually needed. In practice we have pretty
       * good error management and error recovery so the error should
       * find its way to the JSON error field anyway.
       *)
      let lpat =
        lazy
          ((* we need to raise the right error *)
           try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
               Parse_pattern.parse_pattern lang ~print_errors:false
                 ~rule_options:env.options str))
      in
      XP.mk_xpat (XP.Sem (lpat, lang)) (str, tok)
  | Xlang.LRegex ->
      XP.mk_xpat (XP.Regexp (parse_regexp env (str, tok))) (str, tok)
  | Xlang.LSpacegrep -> (
      let src = Spacegrep.Src_file.of_string str in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> XP.mk_xpat (XP.Spacegrep ast) (str, tok)
      (* TODO: use R.Err exn instead? *)
      | Error err -> failwith err.msg)
  | Xlang.LAliengrep ->
      let conf = aliengrep_conf_of_options env in
      let pat = Aliengrep.Pat_compile.from_string conf str in
      XP.mk_xpat (XP.Aliengrep pat) (str, tok)

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
    | None ->
        error_at_expr env.id e
          ("Expected a string value for " ^ Rule_ID.to_string env.id)
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
  try_and_raise_invalid_pattern_if_error env (s, t) (fun () ->
      parse_rule_xpattern env (s, t))

(*****************************************************************************)
(* Parser for old (but current) formula *)
(*****************************************************************************)

(* This was in Rule.ml before and represent the old (but still current)
 * way to write metavariable conditions.
 *)
(* extra conditions, usually on metavariable content *)
type extra =
  | MetavarRegexp of MV.mvar * Xpattern.regexp_string * bool
  | MetavarType of MV.mvar * Xlang.t option * string list * G.type_ list
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
  base : Parsed_int.t option;
}

(* Substitutes `$MVAR` with `int($MVAR)` in cond. *)
(* This now changes all such metavariables. We expect in most cases there should
   just be one, anyways.
*)
let rewrite_metavar_comparison_strip cond =
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.map_legacy as super

      method! visit_expr env e =
        (* apply on children *)
        let e = super#visit_expr env e in
        match e.G.e with
        | G.N (G.Id ((s, tok), _idinfo)) when Metavariable.is_metavar_name s ->
            let py_int = G.Id (("int", tok), G.empty_id_info ()) in
            G.Call (G.N py_int |> G.e, Tok.unsafe_fake_bracket [ G.Arg e ])
            |> G.e
        | _ -> e
    end
  in
  visitor#visit_expr () cond

(* TODO: Old stuff that we can't kill yet. *)
let find_formula_old env (rule_dict : dict) : key * G.expr =
  let find key_str = Hashtbl.find_opt rule_dict.h key_str in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex" )
  with
  | None, None, None, None ->
      error env.id rule_dict.first_tok
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex` to be present"
  | Some (key, value), None, None, None
  | None, Some (key, value), None, None
  | None, None, Some (key, value), None
  | None, None, None, Some (key, value) ->
      (key, value)
  | _ ->
      error env.id rule_dict.first_tok
        "Expected only one of `pattern`, `pattern-either`, `patterns`, or \
         `pattern-regex`"

let rec parse_formula_old_from_dict (env : env) (rule_dict : dict) : R.formula =
  let formula = parse_pair_old env (find_formula_old env rule_dict) in
  (* sanity check *)
  (* bTODO: filter out unconstrained nots *)
  formula

and parse_pair_old env ((key, value) : key * G.expr) : R.formula =
  let env = { env with path = fst key :: env.path } in
  let parse_listi env (key : key) f x =
    match x.G.e with
    | G.Container (Array, (_, xs, _)) -> List_.mapi f xs
    | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)
  in
  let get_pattern str_e = parse_xpattern_expr env str_e in
  (* We use this for lists (patterns and pattern-either) as well as things which
     can be both base patterns and pairs.
     The former does not allow base patterns to appear, so we add an `allow_string`
     parameter.
  *)
  let get_formula ?(allow_string = false) env x =
    match (parse_str_or_dict env x, x.G.e) with
    | Left (value, t), _ when allow_string ->
        R.P (parse_rule_xpattern env (value, t))
    | Left (s, _), _ ->
        error_at_expr env.id x
          (Common.spf
             "Strings not allowed at this position, maybe try `pattern: %s`" s)
    | _, G.Container (Dict, (_, entries, _)) -> (
        match entries with
        | [
         {
           e =
             Container
               (Tuple, (_, [ { e = L (String (_, key, _)); _ }; value ], _));
           _;
         };
        ] ->
            parse_pair_old env (key, value)
        | __else__ ->
            error_at_expr env.id x
              "Expected object with only one entry -- did you forget a hyphen?")
    | _ -> error_at_expr env.id x "Received invalid Semgrep pattern"
  in
  let get_nested_formula_in_list env i x =
    let env = { env with path = string_of_int i :: env.path } in
    get_formula env x
  in
  let s, t = key in
  match s with
  | "pattern" -> R.P (get_pattern value)
  | "pattern-not" -> R.Not (t, get_formula ~allow_string:true env value)
  | "pattern-inside" -> R.Inside (t, get_formula ~allow_string:true env value)
  | "pattern-not-inside" ->
      R.Not (t, R.Inside (t, get_formula ~allow_string:true env value))
  | "semgrep-internal-pattern-anywhere" -> (
      match parse_str_or_dict env value with
      | Left _ -> R.Anywhere (t, R.P (get_pattern value))
      | Right dict -> R.Anywhere (t, parse_formula_old_from_dict env dict))
  | "pattern-either" ->
      R.Or (t, parse_listi env key (get_nested_formula_in_list env) value)
  | "patterns" ->
      let parse_pattern i expr =
        match parse_str_or_dict env expr with
        | Left (s, _t) ->
            error_at_expr env.id value
              (Common.spf
                 "Strings are not valid under `patterns`: did you mean to \
                  write `pattern: %s` instead?"
                 s)
        | Right dict -> (
            let find key_str = Hashtbl.find_opt dict.h key_str in
            let process_extra extra =
              match extra with
              | MetavarRegexp (mvar, regex, b) -> R.CondRegexp (mvar, regex, b)
              | MetavarType (mvar, xlang_opt, s, t) ->
                  R.CondType (mvar, xlang_opt, s, t)
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
                find "metavariable-type",
                find "metavariable-pattern",
                find "metavariable-comparison" )
            with
            | None, None, None, None, None, None ->
                Either_.Left3 (get_nested_formula_in_list env i expr)
            | Some (((_, t) as key), value), None, None, None, None, None ->
                Either_.Middle3 (t, parse_focus_mvs env key value)
            | None, Some (key, value), None, None, None, None
            | None, None, Some (key, value), None, None, None
            | None, None, None, Some (key, value), None, None
            | None, None, None, None, Some (key, value), None
            | None, None, None, None, None, Some (key, value) ->
                Either_.Right3
                  (snd key, parse_extra env key value |> process_extra)
            | _ ->
                error_at_expr env.id expr
                  "should not happen, expected\n\
                  \            one of `metavariable-analysis`, \
                   `metavariable-regex`, or `metavariable-comparison`")
      in
      let conjuncts, focus, conditions =
        Either_.partition_either3
          (fun x -> x)
          (parse_listi env key parse_pattern value)
      in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Rule.raise_error (Some env.id)
          (InvalidRule (MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus; conditions })
  | "pattern-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | "pattern-not-regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.Not (t, R.P xpat)
  | "focus-metavariable"
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-type"
  | "metavariable-pattern"
  | "metavariable-comparison" ->
      error_at_key env.id key "Must occur directly under a patterns:"
  | "pattern-where-python" ->
      Rule.raise_error (Some env.id)
        (InvalidRule (DeprecatedFeature (fst key), env.id, t))
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env.id key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> failwith "TODO"
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) : extra =
  match fst key with
  | "metavariable-analysis" ->
      let mv_analysis_dict = yaml_to_dict env key value in
      let metavar = take_key mv_analysis_dict env parse_string "metavariable" in
      let analyzer = take_key mv_analysis_dict env parse_string "analyzer" in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "entropy_v2" -> R.CondEntropyV2
        | "redos" -> R.CondReDoS
        | other -> error_at_key env.id key ("Unsupported analyzer: " ^ other)
      in
      MetavarAnalysis (metavar, kind)
  | "metavariable-regex" ->
      let mv_regex_dict =
        try yaml_to_dict env key value with
        | R.Error { kind = DuplicateYamlKey (msg, t); _ } ->
            error env.id t (msg ^ ". You should use multiple metavariable-regex")
      in
      let metavar, regexp, const_prop =
        ( take_key mv_regex_dict env parse_string "metavariable",
          take_key mv_regex_dict env parse_string_wrap "regex",
          take_opt mv_regex_dict env parse_bool "constant-propagation" )
      in
      MetavarRegexp
        ( metavar,
          parse_regexp env regexp,
          match const_prop with
          | Some b -> b
          | None -> false )
  | "metavariable-type" ->
      let mv_type_dict = yaml_to_dict env key value in
      let metavar = take_key mv_type_dict env parse_string "metavariable" in
      let type_strs =
        take_opt mv_type_dict env parse_string_wrap "type" |> Option.to_list
      in
      let type_strs =
        type_strs
        @ (take_opt mv_type_dict env
             (parse_string_wrap_list (fun x -> x))
             "types"
          |> Option.to_list |> List.flatten)
      in
      if type_strs =*= [] then
        error env.id mv_type_dict.first_tok
          "Missing required field: type or types";
      let env', opt_xlang =
        match take_opt mv_type_dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                id = env.id;
                target_analyzer = xlang;
                in_metavariable_pattern = env.in_metavariable_pattern;
                path = "metavariable-type" :: "metavariable" :: env.path;
                options_key = None;
                options = None;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let ts = type_strs |> List_.map (parse_type env' key) in
      MetavarType (metavar, opt_xlang, type_strs |> List_.map fst, ts)
  | "metavariable-pattern" ->
      let mv_pattern_dict = yaml_to_dict env key value in
      let metavar = take_key mv_pattern_dict env parse_string "metavariable" in
      let env', opt_xlang =
        match take_opt mv_pattern_dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                id = env.id;
                target_analyzer = xlang;
                in_metavariable_pattern = env.in_metavariable_pattern;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
                options_key = None;
                options = None;
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
          take_key mv_comparison_dict env parse_string "comparison",
          take_opt mv_comparison_dict env parse_bool "strip",
          take_opt mv_comparison_dict env parse_int "base" )
      in
      let comparison = parse_metavar_cond env key comparison in
      (match (metavariable, strip) with
      | None, Some true ->
          error_at_key env.id key
            (fst key
           ^ ": 'metavariable' field is missing, but it is mandatory if \
              'strip: true'")
      | __else__ -> ());
      MetavarComparison { metavariable; comparison; strip; base }
  | _ -> error_at_key env.id key ("wrong parse_extra field: " ^ fst key)

(*****************************************************************************)
(* Parser for new formula *)
(*****************************************************************************)

let formula_keys =
  [ "pattern"; "all"; "any"; "regex"; "taint"; "not"; "inside"; "anywhere" ]

let find_formula env (rule_dict : dict) : key * G.expr =
  match List_.find_some_opt (Hashtbl.find_opt rule_dict.h) formula_keys with
  | None ->
      error env.id rule_dict.first_tok
        ("Expected one of " ^ String.concat "," formula_keys ^ " to be present")
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
        (try parse_rule_xpattern env (s, t) with
        | (Time_limit.Timeout _ | UnixExit _) as e ->
            Exception.catch_and_reraise e
        (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
        | exn ->
            Rule.raise_error (Some env.id)
              (InvalidRule
                 ( InvalidPattern
                     (s, env.target_analyzer, Common.exn_to_s exn, env.path),
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
          error env.id dict.first_tok
            "Expected exactly one key of `pattern`, `all`, `any`, `regex`, \
             `not`, or `inside`"
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
        take_key dict env parse_string_wrap "comparison"
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
      [ Left (t, R.CondEval cond) ]
  | Cfocus ->
      (* focus: ...
       *)
      let mv_list = take_key dict env parse_focus_mvs "focus" in
      [ Right (tok, mv_list) ]
  | Canalyzer ->
      (* metavariable: ...
         analyzer: ...
      *)
      let metavar, t = take_key dict env parse_string_wrap "metavariable" in
      let analyzer, analyze_t =
        take_key dict env parse_string_wrap "analyzer"
      in
      let kind =
        match analyzer with
        | "entropy" -> R.CondEntropy
        | "redos" -> R.CondReDoS
        | other ->
            error_at_key env.id ("analyzer", analyze_t)
              ("Unsupported analyzer: " ^ other)
      in
      [ Left (t, CondAnalysis (metavar, kind)) ]
  | Cmetavar ->
      (* metavariable: ...
         [<pattern-pair>]
         [type: ...]
         [language: ...]
      *)
      let metavar, t = take_key dict env parse_string_wrap "metavariable" in
      let env', opt_xlang =
        match take_opt dict env parse_string "language" with
        | Some s ->
            let xlang = Xlang.of_string ~rule_id:(Rule_ID.to_string env.id) s in
            let env' =
              {
                env with
                target_analyzer = xlang;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some xlang)
        | ___else___ -> (env, None)
      in
      let pat =
        match List_.find_some_opt (Hashtbl.find_opt dict.h) formula_keys with
        | Some ps -> (
            let env' = { env' with in_metavariable_pattern = true } in
            let formula = parse_pair env' ps in
            match formula with
            | R.P { pat = Xpattern.Regexp regexp; _ } ->
                (* TODO: always on by default *)
                [ Left (t, R.CondRegexp (metavar, regexp, true)) ]
            | _ -> [ Left (t, CondNestedFormula (metavar, opt_xlang, formula)) ]
            )
        | None -> []
      in
      let type_strs =
        take_opt dict env parse_string_wrap "type" |> Option.to_list
      in
      let type_strs =
        type_strs
        @ (take_opt dict env (parse_string_wrap_list (fun x -> x)) "types"
          |> Option.to_list |> List.flatten)
      in
      let typ =
        match type_strs with
        | ts :: _ ->
            [
              Left
                ( snd ts,
                  R.CondType
                    ( metavar,
                      opt_xlang,
                      type_strs |> List_.map fst,
                      type_strs |> List_.map (parse_type env (metavar, t)) ) );
            ]
        | _ -> []
      in
      List.flatten [ pat; typ ]

and constrain_where env (t1, _t2) where_key (value : G.expr) formula : R.formula
    =
  let env = { env with path = "where" :: env.path } in
  (* TODO: first token, or principal token? *)
  let parse_where_pair env (where_value : G.expr) =
    let dict = yaml_to_dict env where_key where_value in
    match find_constraint dict with
    | Some (tok, indicator) -> produce_constraint env dict tok indicator
    | _ -> error_at_expr env.id value "Wrong where constraint fields"
  in
  (* TODO *)
  let conditions, focus =
    parse_listi env where_key parse_where_pair value
    |> List.flatten
    |> Either_.partition_either (fun x -> x)
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
  | "anywhere" -> R.Anywhere (t, parse_formula env value)
  | "all" ->
      let conjuncts = parse_listi env key parse_formula value in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Rule.raise_error (Some env.id)
          (InvalidRule (MissingPositiveTermInAnd, env.id, t));
      R.And (t, { conjuncts; focus = []; conditions = [] })
  | "any" -> R.Or (t, parse_listi env key parse_formula value)
  | "regex" ->
      let x = parse_string_wrap env key value in
      let xpat = XP.mk_xpat (Regexp (parse_regexp env x)) x in
      R.P xpat
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))
