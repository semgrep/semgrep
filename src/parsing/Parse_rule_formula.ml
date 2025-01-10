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
module G = AST_generic
module H = Parse_rule_helpers
module XP = Xpattern
module MV = Metavariable
module R = Rule

(* Unwrap the Ok value and map it to another Ok value.
   Compare this to 'let/' which maps an Ok value to a result (Ok or Error).

   The point of using 'let+' and 'let/' is that they leave any Error intact,
   propagating it without us having to worry about it.
*)
let ( let+ ) x f = Result.map f x

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
      Ok [ s ]
  | G.Container (Array, (_, mvs, _)) ->
      List_.map
        (fun mv ->
          let+ s, _ = parse_string_wrap env key mv in
          s)
        mvs
      |> Base.Result.all
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
       | x when String.length x =|= 1 (* = *) -> Ok x.[0]
       | long ->
           error_at_opt_key env.id env.options_key
             (spf "Multibyte word characters aren't supported: %S" long))
  |> Base.Result.all

(* Aliengrep brace pairs are specified as strings because ATD doesn't have
   a char type and because they could be multibyte UTF-8-encoded characters.
   For now, aliengrep only supports single-byte characters. *)
let brace_pairs_of_string_pairs env xs =
  xs
  |> List_.map (fun (open_, close) ->
         let/ opening_char =
           match open_ with
           | "" -> error_at_opt_key env.id env.options_key "Empty opening brace"
           | x -> Ok x.[0]
         in
         let/ closing_char =
           match close with
           | "" -> error_at_opt_key env.id env.options_key "Empty closing brace"
           | x -> Ok x.[0]
         in
         if String.length open_ > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte opening braces aren't supported: %S" open_)
         else if String.length close > 1 then
           error_at_opt_key env.id env.options_key
             (spf "Multibyte closing braces aren't supported: %S" close)
         else Ok (opening_char, closing_char))
  |> Base.Result.all

let aliengrep_conf_of_options (env : env) :
    (Aliengrep.Conf.t, Rule_error.t) Result.t =
  let options = Option.value env.options ~default:Rule_options.default in
  let default = Aliengrep.Conf.default_multiline_conf in
  let caseless = options.generic_caseless in
  let multiline = options.generic_multiline in
  let/ extra_word_chars =
    word_chars_of_strings env options.generic_extra_word_characters
  in
  let word_chars = default.word_chars @ extra_word_chars in
  let/ brackets =
    let/ base_set =
      match options.generic_braces with
      | None -> Ok default.brackets
      | Some string_pairs -> brace_pairs_of_string_pairs env string_pairs
    in
    let/ extra_braces =
      brace_pairs_of_string_pairs env options.generic_extra_braces
    in
    Ok (base_set @ extra_braces)
  in
  Ok ({ caseless; multiline; word_chars; brackets } : Aliengrep.Conf.t)

(*****************************************************************************)
(* Type parsing *)
(*****************************************************************************)

let rec parse_type env key (str, tok) =
  match env.target_analyzer with
  | Analyzer.L (lang, _) ->
      let/ str = wrap_type_expr env key lang str in
      let/ p =
        try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
            parse_pattern_with_rule_error env (str, tok) lang str)
      in
      unwrap_type_expr env key lang p
  | Analyzer.LRegex
  | Analyzer.LSpacegrep
  | Analyzer.LAliengrep ->
      error_at_key env.id key
        "`type` is not supported with regex, spacegrep or aliengrep."

and wrap_type_expr env key lang str =
  match Parse_metavariable_type.wrap_type_expr lang str with
  | Some x -> Ok x
  | None ->
      error_at_key env.id key
        ("`metavariable-type` is not supported for " ^ Lang.show lang)

and unwrap_type_expr env key lang expr =
  match Parse_metavariable_type.unwrap_type_expr lang expr with
  | Some x -> Ok x
  | None ->
      error_at_key env.id key
        ("Failed to unwrap the type expression." ^ G.show_any expr)

(*****************************************************************************)
(* Parser for xpattern *)
(*****************************************************************************)

(* less: could move in a separate Parse_xpattern.ml *)
(* NOTE: I don't think this can raise any more *)
let parse_rule_xpattern env (str, tok) =
  match env.target_analyzer with
  | Analyzer.L (lang, _) ->
      let+ pat =
        (* we need to raise the right error *)
        try_and_raise_invalid_pattern_if_error env (str, tok) (fun () ->
            parse_pattern_with_rule_error env (str, tok)
              ?rule_options:env.options lang str)
      in
      XP.mk_xpat (XP.Sem (pat, lang)) (str, tok)
  | Analyzer.LRegex ->
      let+ re = parse_regexp env (str, tok) in
      XP.mk_xpat (XP.Regexp re) (str, tok)
  | Analyzer.LSpacegrep -> (
      let src = Spacegrep.Src_file.of_string str in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> Ok (XP.mk_xpat (XP.Spacegrep ast) (str, tok))
      (* TODO: use R.Err exn instead? *)
      | Error err -> failwith err.msg)
  | Analyzer.LAliengrep ->
      let+ conf = aliengrep_conf_of_options env in
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
  let/ s, t =
    match read_string_wrap e.G.e with
    | Some (s, t) -> Ok (s, t)
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
  | MetavarType of MV.mvar * Analyzer.t option * string list * G.type_ list
  | MetavarPattern of MV.mvar * Analyzer.t option * Rule.formula
  | MetavarComparison of metavariable_comparison
  | MetavarAnalysis of MV.mvar * Rule.metavar_analysis_kind
  | MetavarName of MV.mvar * Rule.metavar_name_kind option * string list option

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
        | G.N (G.Id ((s, tok), _idinfo)) when Mvar.is_metavar_name s ->
            let py_int = G.Id (("int", tok), G.empty_id_info ()) in
            G.Call (G.N py_int |> G.e, Tok.unsafe_fake_bracket [ G.Arg e ])
            |> G.e
        | _ -> e
    end
  in
  visitor#visit_expr () cond

let find_formula_old env (rule_dict : dict) :
    (key * G.expr, Rule_error.t) result =
  match
    ( H.dict_take_opt rule_dict "pattern",
      H.dict_take_opt rule_dict "pattern-either",
      H.dict_take_opt rule_dict "patterns",
      H.dict_take_opt rule_dict "pattern-regex" )
  with
  | None, None, None, None ->
      error env.id rule_dict.first_tok
        (* Note, we shouldn't reach this code if
           r2c-internal-project-depends-on is present. *)
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex`, or `r2c-internal-project-depends-on` to be present."
  | Some (key, value), None, None, None
  | None, Some (key, value), None, None
  | None, None, Some (key, value), None
  | None, None, None, Some (key, value) ->
      Ok (key, value)
  | _ ->
      error env.id rule_dict.first_tok
        "Expected only one of `pattern`, `pattern-either`, `patterns`, or \
         `pattern-regex`"

(*
let parse_options (env : env) (dict : dict) :
    (R.formula, Rule_error.t) result =
      let/ mode = take_opt mv_analysis_dict env parse_string "mode" in

      let/ options =
        take_opt mv_analysis_dict env parse_metavariable_analysis_options "options" in
*)

let rec parse_formula_old_from_dict (env : env) (rule_dict : dict) :
    (R.formula, Rule_error.t) result =
  (* bTODO: filter out unconstrained nots *)
  let/ root = find_formula_old env rule_dict in
  parse_pair_old env root

and parse_pair_old env ((key, value) : key * G.expr) :
    (R.formula, Rule_error.t) result =
  let env = { env with path = fst key :: env.path } in
  let parse_listi env (key : key) f x =
    match x.G.e with
    | G.Container (Array, (_, xs, _)) -> List_.mapi f xs |> Base.Result.all
    | _ -> error_at_key env.id key ("Expected a list for " ^ fst key)
  in
  let get_pattern str_e = parse_xpattern_expr env str_e in
  (* We use this for lists (patterns and pattern-either) as well as things which
     can be both base patterns and pairs.
     The former does not allow base patterns to appear, so we add an `allow_string`
     parameter.
  *)
  let get_formula ?(allow_string = false) env x =
    let/ pattern = parse_str_or_dict env x in
    match (pattern, x.G.e) with
    | Left (value, t), _ when allow_string ->
        let/ xpat = parse_rule_xpattern env (value, t) in
        Ok (R.P xpat |> R.f)
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
  | "pattern" ->
      let+ pat = get_pattern value in
      R.P pat |> R.f
  | "pattern-not" ->
      let+ formula = get_formula ~allow_string:true env value in
      R.Not (t, formula) |> R.f
  | "pattern-inside" ->
      let+ formula = get_formula ~allow_string:true env value in
      R.Inside (t, formula) |> R.f
  | "pattern-not-inside" ->
      let+ formula = get_formula ~allow_string:true env value in
      R.Not (t, R.Inside (t, formula) |> R.f) |> R.f
  | "semgrep-internal-pattern-anywhere" ->
      let/ pattern = parse_str_or_dict env value in
      let+ formula =
        match pattern with
        | Left _ ->
            let+ pattern = get_pattern value in
            R.Anywhere (t, R.P pattern |> R.f)
        | Right dict ->
            let+ formula = parse_formula_old_from_dict env dict in
            R.Anywhere (t, formula)
      in
      R.f formula
  | "pattern-either" ->
      let+ formulae =
        parse_listi env key (get_nested_formula_in_list env) value
      in
      R.Or (t, formulae) |> R.f
  | "patterns" ->
      let parse_pattern i expr =
        let/ pattern = parse_str_or_dict env expr in
        match pattern with
        | Left (s, _t) ->
            error_at_expr env.id value
              (Common.spf
                 "Strings are not valid under `patterns`: did you mean to \
                  write `pattern: %s` instead?"
                 s)
        | Right dict -> (
            let process_extra extra =
              match extra with
              | MetavarRegexp (mvar, regex, b) -> R.CondRegexp (mvar, regex, b)
              | MetavarType (mvar, analyzer_opt, s, t) ->
                  R.CondType (mvar, analyzer_opt, s, t)
              | MetavarPattern (mvar, analyzer_opt, formula) ->
                  R.CondNestedFormula (mvar, analyzer_opt, formula)
              | MetavarComparison { comparison; strip; _ } ->
                  R.CondEval
                    (match strip with
                    (* TODO *)
                    | Some true -> rewrite_metavar_comparison_strip comparison
                    | _ -> comparison)
              | MetavarAnalysis (mvar, kind) -> R.CondAnalysis (mvar, kind)
              | MetavarName (mvar, kind, modules) ->
                  R.CondName { mvar; kind; modules }
            in
            match
              ( H.dict_take_opt dict "focus-metavariable",
                H.dict_take_opt dict "metavariable-analysis",
                H.dict_take_opt dict "metavariable-regex",
                H.dict_take_opt dict "metavariable-type",
                H.dict_take_opt dict "metavariable-pattern",
                H.dict_take_opt dict "metavariable-comparison",
                H.dict_take_opt dict "semgrep-internal-metavariable-name" )
            with
            | None, None, None, None, None, None, None ->
                let+ nested_formula = get_nested_formula_in_list env i expr in
                Either_.Left3 nested_formula
            | Some (((_, t) as key), value), None, None, None, None, None, None
              ->
                let+ focus = parse_focus_mvs env key value in
                Either_.Middle3 (t, focus)
            | None, Some (key, value), None, None, None, None, None
            | None, None, Some (key, value), None, None, None, None
            | None, None, None, Some (key, value), None, None, None
            | None, None, None, None, Some (key, value), None, None
            | None, None, None, None, None, Some (key, value), None
            | None, None, None, None, None, None, Some (key, value) ->
                let+ extra = parse_extra env key value in
                Either_.Right3 (snd key, extra |> process_extra)
            | _ ->
                error_at_expr env.id expr
                  "should not happen, expected\n\
                  \            one of `metavariable-analysis`, \
                   `metavariable-regex`, or `metavariable-comparison`")
      in
      let/ patterns = parse_listi env key parse_pattern value in
      let conjuncts, focus, conditions =
        Either_.partition_either3 Fun.id patterns
      in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Error
          (Rule_error.mk_error ~rule_id:env.id
             (InvalidRule (MissingPositiveTermInAnd, env.id, t)))
      else
        Ok
          {
            R.f = R.And (t, conjuncts);
            focus;
            conditions;
            fix = None;
            as_ = None;
          }
  | "pattern-regex" ->
      let/ x = parse_string_wrap env key value in
      let/ regex = parse_regexp env x in
      let xpat = XP.mk_xpat (Regexp regex) x in
      Ok (R.P xpat |> R.f)
  | "pattern-not-regex" ->
      let/ x = parse_string_wrap env key value in
      let/ regex = parse_regexp env x in
      let xpat = XP.mk_xpat (Regexp regex) x in
      Ok (R.Not (t, R.P xpat |> R.f) |> R.f)
  | "focus-metavariable"
  | "metavariable-analysis"
  | "metavariable-regex"
  | "metavariable-type"
  | "metavariable-pattern"
  | "metavariable-comparison" ->
      error_at_key env.id key "Must occur directly under a patterns:"
  | "pattern-where-python" ->
      Error
        (Rule_error.mk_error ~rule_id:env.id
           (InvalidRule (DeprecatedFeature (fst key), env.id, t)))
  (* fix suggestions *)
  | "metavariable-regexp" ->
      error_at_key env.id key
        (spf "unexpected key %s, did you mean metavariable-regex" (fst key))
  (* These keys are handled in Python *)
  (* TODO really we should either remove these keys before sending
     the rules or handle them in OCaml, this is not good *)
  | "r2c-internal-patterns-from" -> failwith "TODO"
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))

and parse_analyzer (env : env) (key : key) (dict : dict) :
    (Rule.metavar_analysis_kind, Rule_error.t) result =
  let/ analyzer_kind, options_dict =
    take_key dict env parse_variant "analyzer"
  in
  let/ analyzer =
    match analyzer_kind with
    | "entropy" -> Ok R.CondEntropy
    | "entropy_v2" ->
        let/ mode =
          match options_dict with
          | None -> Ok R.Default
          | Some options_dict ->
              let/ mode = take_opt options_dict env parse_string "mode" in
              let/ mode =
                match mode with
                | Some "lax" -> Ok R.Lax
                | Some "strict" -> Ok R.Strict
                | Some "default"
                | None ->
                    Ok R.Default
                | Some other ->
                    error_at_key env.id key
                      ("Unsupported optional analyzer mode: " ^ other)
              in
              Ok mode
        in
        Ok (R.CondEntropyV2 mode)
    | "redos" -> Ok R.CondReDoS
    | other -> error_at_key env.id key ("Unsupported analyzer: " ^ other)
  in
  let/ () =
    match options_dict with
    | Some options_dict -> check_that_dict_is_empty options_dict
    | None -> Ok ()
  in
  Ok analyzer

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and parse_extra (env : env) (key : key) (value : G.expr) :
    (extra, Rule_error.t) result =
  match fst key with
  | "metavariable-analysis" ->
      let/ mv_analysis_dict = parse_dict env key value in
      let/ metavar =
        take_key mv_analysis_dict env parse_string "metavariable"
      in
      let/ analyzer_kind = parse_analyzer env key mv_analysis_dict in
      let/ () = check_that_dict_is_empty mv_analysis_dict in
      Ok (MetavarAnalysis (metavar, analyzer_kind))
  | "metavariable-regex" ->
      let/ mv_regex_dict =
        match parse_dict env key value with
        | Error { kind = DuplicateYamlKey (msg, t); _ } ->
            error env.id t (msg ^ ". You should use multiple metavariable-regex")
        | x -> x
      in
      let/ metavar = take_key mv_regex_dict env parse_string "metavariable" in
      let/ regexp = take_key mv_regex_dict env parse_string_wrap "regex" in
      let/ regexp = parse_regexp env regexp in
      let/ const_prop =
        take_opt mv_regex_dict env parse_bool "constant-propagation"
      in
      Ok
        (MetavarRegexp
           ( metavar,
             regexp,
             match const_prop with
             | Some b -> b
             | None -> false ))
  | "metavariable-type" ->
      let/ mv_type_dict = parse_dict env key value in
      let/ metavar = take_key mv_type_dict env parse_string "metavariable" in
      let/ type_strs = take_opt mv_type_dict env parse_string_wrap "type" in
      let/ type_list =
        take_opt mv_type_dict env
          (parse_string_wrap_list (fun _ _ x -> Ok x))
          "types"
      in
      let type_list = type_list |> Option.to_list |> List_.flatten in
      let type_strs = (type_strs |> Option.to_list) @ type_list in
      if type_strs =*= [] then
        error env.id mv_type_dict.first_tok
          "Missing required field: type or types"
      else
        let/ env', opt_analyzer =
          let+ lang = take_opt mv_type_dict env parse_string "language" in
          match lang with
          | Some s ->
              let analyzer =
                Analyzer.of_string ~rule_id:(Rule_ID.to_string env.id) s
              in
              let env' =
                {
                  env with
                  target_analyzer = analyzer;
                  path = "metavariable-type" :: "metavariable" :: env.path;
                }
              in
              (env', Some analyzer)
          | ___else___ -> (env, None)
        in
        let/ ts =
          type_strs |> List_.map (parse_type env' key) |> Base.Result.all
        in
        Ok (MetavarType (metavar, opt_analyzer, type_strs |> List_.map fst, ts))
  | "metavariable-pattern" ->
      let/ mv_pattern_dict = parse_dict env key value in
      let/ metavar = take_key mv_pattern_dict env parse_string "metavariable" in
      let/ env', opt_analyzer =
        let+ lang = take_opt mv_pattern_dict env parse_string "language" in
        match lang with
        | Some s ->
            let analyzer =
              Analyzer.of_string ~rule_id:(Rule_ID.to_string env.id) s
            in
            let env' =
              {
                env with
                target_analyzer = analyzer;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some analyzer)
        | ___else___ -> (env, None)
      in
      let env' = { env' with in_metavariable_pattern = true } in
      let/ formula_root = find_formula_old env mv_pattern_dict in
      let/ formula_old = parse_pair_old env' formula_root in
      Ok (MetavarPattern (metavar, opt_analyzer, formula_old))
  | "metavariable-comparison" ->
      let/ mv_comparison_dict = parse_dict env key value in
      let/ metavariable =
        take_opt mv_comparison_dict env parse_string "metavariable"
      in
      let/ comparison =
        take_key mv_comparison_dict env parse_string "comparison"
      in
      let/ strip = take_opt mv_comparison_dict env parse_bool "strip" in
      let/ base = take_opt mv_comparison_dict env parse_int "base" in
      let/ comparison = parse_metavar_cond env key comparison in
      let/ () =
        match (metavariable, strip) with
        | None, Some true ->
            error_at_key env.id key
              (fst key
             ^ ": 'metavariable' field is missing, but it is mandatory if \
                'strip: true'")
        | __else__ -> Ok ()
      in
      Ok (MetavarComparison { metavariable; comparison; strip; base })
  | "semgrep-internal-metavariable-name" -> (
      let/ mv_name_dict = parse_dict env key value in
      let/ mvar = take_key mv_name_dict env parse_string "metavariable" in
      let parse_kind = function
        | "django-view", _ -> Ok R.DjangoView
        | "express-app", _ -> Ok R.ExpressApp
        | "express-controller", _ -> Ok R.ExpressController
        | str, _ -> error_at_key env.id key ("unsupported kind: " ^ str)
      in
      let/ kind_str = take_opt mv_name_dict env parse_string_wrap "kind" in
      let/ module_ = take_opt mv_name_dict env parse_string "module" in
      let/ modules =
        take_opt mv_name_dict env
          (fun e k -> parse_list e k (fun e -> parse_string e k))
          "modules"
      in
      match (kind_str, module_, modules) with
      | None, None, None ->
          error_at_key env.id key "expected at least one of kind, module(s)"
      | _, Some _, Some _ ->
          error_at_key env.id key "expected only one of module, modules"
      | _ ->
          let/ kind =
            match Option.map parse_kind kind_str with
            | None -> Ok None
            | Some (Ok x) -> Ok (Some x)
            | Some (Error e) -> Error e
          in
          Ok
            (MetavarName
               ( mvar,
                 kind,
                 match modules with
                 | None -> Option.map (fun x -> [ x ]) module_
                 | _ -> modules )))
  | _ -> error_at_key env.id key ("wrong parse_extra field: " ^ fst key)

(*****************************************************************************)
(* Parser for new formula *)
(*****************************************************************************)

let formula_keys =
  [ "pattern"; "all"; "any"; "regex"; "taint"; "not"; "inside"; "anywhere" ]

let find_formula env (rule_dict : dict) : (key * G.expr, Rule_error.t) result =
  match List_.find_some_opt (H.dict_take_opt rule_dict) formula_keys with
  | None ->
      error env.id rule_dict.first_tok
        ("Expected one of " ^ String.concat "," formula_keys ^ " to be present")
  | Some (key, value) -> Ok (key, value)

(* intermediate type used for processing 'where' *)
type principal_constraint = Ccompare | Cfocus | Cmetavar | Canalyzer | Ctype

let find_constraint dict =
  fold_dict
    (fun s ((_, tok), _) acc ->
      match acc with
      (* If we have seen a `metavariable:`, it still could be a
         metavariable-type or metavariable-analysis.
         We continue to match to see if it is any of these fields.
      *)
      | Some (_, Cmetavar)
      | None -> (
          match s with
          | "comparison" -> Some (tok, Ccompare)
          | "focus" -> Some (tok, Cfocus)
          | "analyzer" -> Some (tok, Canalyzer)
          | "type"
          | "types" ->
              Some (tok, Ctype)
          | "metavariable" -> Some (tok, Cmetavar)
          | _ -> acc)
      | Some res -> Some res)
    dict None

let rec parse_formula env (value : G.expr) : (R.formula, Rule_error.t) result =
  (* First, try to parse as a string *)
  let/ pattern = parse_str_or_dict env value in
  match pattern with
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
      (* TODO: capture and adjust pos of parsing error exns instead of using [t] *)
      let+ xpattern =
        try_and_raise_invalid_pattern_if_error env (s, t) (fun () ->
            parse_rule_xpattern env (s, t))
      in
      R.P xpattern |> R.f
  (* If that doesn't work, it should be a key-value pairing.
   *)
  | Right dict -> parse_formula_from_dict env dict

and parse_formula_from_dict env dict =
  (* First, parse the other fields than the pattern itself.
     These are `where`, `as` and `fix` at the moment.
  *)
  let/ where_formula =
    take_opt dict env (fun _env key value -> Ok (key, value)) "where"
  in
  let/ as_metavariable = take_opt dict env parse_string "as" in
  let/ fix = take_opt dict env parse_string "fix" in
  (* There should be only one key left, which is the pattern. *)
  if Hashtbl.length dict.h <> 1 then
    error env.id dict.first_tok
      "Expected exactly one key of `pattern`, `all`, `any`, `regex`, `not`, or \
       `inside`"
  else
    (* Add the where information and fix information. *)
    let/ formula =
      match where_formula with
      | None ->
          let/ formula_root = find_formula env dict in
          parse_pair env formula_root
      | Some (((_, t) as key), value) ->
          let/ formula_root = find_formula env dict in
          let/ formula = parse_pair env formula_root in
          constrain_where env (t, t) key value formula
    in
    Ok { formula with fix; as_ = as_metavariable }

and produce_constraint (env : env) (key : key) dict tok indicator =
  match indicator with
  | Ccompare ->
      (* comparison: ...
         [strip: ...]
         [base: ...]
      *)
      let/ ((s, t) as compare_key) =
        take_key dict env parse_string_wrap "comparison"
      in
      let/ cond = parse_metavar_cond env compare_key s in
      let/ strip = take_opt dict env parse_bool "strip" in
      (* This base is pretty unnecessary. *)
      let/ _base = take_opt dict env parse_int "base" in
      let cond =
        (* if strip=true we rewrite the condition and insert Python's `int`
            * function to parse the integer value of mvar. *)
        match strip with
        | Some true -> rewrite_metavar_comparison_strip cond
        | _ -> cond
      in
      Ok [ Left (t, R.CondEval cond) ]
  | Cfocus ->
      (* focus: ...
       *)
      let+ mv_list = take_key dict env parse_focus_mvs "focus" in
      [ Right (tok, mv_list) ]
  | Canalyzer ->
      (* metavariable: ...
         analyzer: ...
      *)
      let/ metavar, t = take_key dict env parse_string_wrap "metavariable" in
      let/ analyzer_kind = parse_analyzer env key dict in
      Ok [ Left (t, R.CondAnalysis (metavar, analyzer_kind)) ]
  | Ctype ->
      (* metavariable: ...
         [<pattern-pair>]
         [type: ...]
         [language: ...]
      *)
      let/ metavar, t = take_key dict env parse_string_wrap "metavariable" in
      let/ opt_analyzer =
        let+ lang = take_opt dict env parse_string "language" in
        match lang with
        | Some s ->
            Some (Analyzer.of_string ~rule_id:(Rule_ID.to_string env.id) s)
        | ___else___ -> None
      in
      let/ type_strs = take_opt dict env parse_string_wrap "type" in
      let type_strs = type_strs |> Option.to_list in
      let/ type_list =
        take_opt dict env (parse_string_wrap_list (fun _ _ x -> Ok x)) "types"
      in
      let type_list = type_list |> Option.to_list |> List_.flatten in
      let type_strs = type_strs @ type_list in
      let/ types =
        type_strs |> List_.map (parse_type env (metavar, t)) |> Base.Result.all
      in
      Ok
        (match type_strs with
        | ts :: _ ->
            [
              Left
                ( snd ts,
                  R.CondType
                    (metavar, opt_analyzer, type_strs |> List_.map fst, types)
                );
            ]
        | _ -> [])
  | Cmetavar ->
      (* metavariable: ...
         [<pattern-pair>]
         [language: ...]
      *)
      let/ metavar, t = take_key dict env parse_string_wrap "metavariable" in
      let/ env', opt_analyzer =
        let+ lang = take_opt dict env parse_string "language" in
        match lang with
        | Some s ->
            let analyzer =
              Analyzer.of_string ~rule_id:(Rule_ID.to_string env.id) s
            in
            let env' =
              {
                env with
                target_analyzer = analyzer;
                path = "metavariable-pattern" :: "metavariable" :: env.path;
              }
            in
            (env', Some analyzer)
        | ___else___ -> (env, None)
      in
      let env' = { env' with in_metavariable_pattern = true } in
      let/ formula = parse_formula_from_dict env' dict in
      Ok
        (match formula with
        | {
         f = R.P { pat = Xpattern.Regexp regexp; _ };
         focus = [];
         conditions = [];
         fix = None;
         as_ = None;
        } ->
            [ Left (t, R.CondRegexp (metavar, regexp, true)) ]
        | _ ->
            let pat =
              [ Left (t, R.CondNestedFormula (metavar, opt_analyzer, formula)) ]
            in
            pat)

and constrain_where env (_t1, _t2) where_key (value : G.expr) formula :
    (R.formula, Rule_error.t) result =
  let env = { env with path = "where" :: env.path } in
  (* TODO: first token, or principal token? *)
  let parse_where_pair env (where_value : G.expr) =
    let/ dict = parse_dict env where_key where_value in
    match find_constraint dict with
    | Some (tok, indicator) ->
        produce_constraint env where_key dict tok indicator
    | _ -> error_at_expr env.id value "Wrong where constraint fields"
  in
  (* TODO *)
  let/ conditions, focus =
    let+ where_contents = parse_listi env where_key parse_where_pair value in
    where_contents |> List_.flatten |> Either_.partition (fun x -> x)
  in
  Ok
    {
      formula with
      conditions = formula.conditions @ conditions;
      focus = formula.focus @ focus;
    }

and parse_pair env ((key, value) : key * G.expr) :
    (R.formula, Rule_error.t) result =
  let env = { env with path = fst key :: env.path } in
  let get_string_pattern str_e = parse_xpattern_expr env str_e in
  let s, t = key in
  match s with
  | "pattern" ->
      let+ pattern = get_string_pattern value in
      R.P pattern |> R.f
  | "not" ->
      let+ formula = parse_formula env value in
      R.Not (t, formula) |> R.f
  | "inside" ->
      let+ formula = parse_formula env value in
      R.Inside (t, formula) |> R.f
  | "anywhere" ->
      let+ formula = parse_formula env value in
      R.Anywhere (t, formula) |> R.f
  | "all" ->
      let/ conjuncts = parse_listi env key parse_formula value in
      let pos, _ = R.split_and conjuncts in
      if pos =*= [] && not env.in_metavariable_pattern then
        Error
          (Rule_error.mk_error ~rule_id:env.id
             (InvalidRule (MissingPositiveTermInAnd, env.id, t)))
      else Ok (R.And (t, conjuncts) |> R.f)
  | "any" ->
      let+ formulae = parse_listi env key parse_formula value in
      R.Or (t, formulae) |> R.f
  | "regex" ->
      let/ x = parse_string_wrap env key value in
      let/ regex = parse_regexp env x in
      let xpat = XP.mk_xpat (Regexp regex) x in
      Ok (R.P xpat |> R.f)
  | _ -> error_at_key env.id key (spf "unexpected key %s" (fst key))
