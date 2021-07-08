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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parsing a Semgrep rule, including complex pattern formulas.
 *
 * See also the JSON schema in rule_schema.yaml
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

(* less: could use a hash to accelerate things *)
let rec find_fields flds xs =
  match flds with
  | [] -> ([], xs)
  | fld :: flds ->
      let fld_match = List.assoc_opt fld xs in
      let xs = List.remove_assoc fld xs in
      let matches, rest = find_fields flds xs in
      ((fld, fld_match) :: matches, rest)

let error s = raise (E.InvalidYamlException s)

(*****************************************************************************)
(* Yaml parsers basic types *)
(*****************************************************************************)

let parse_string ctx = function
  | G.L (String (value, _)) -> value
  | G.N (Id ((value, _), _)) -> value
  | _ -> error ("Expected a string value for " ^ ctx)

let parse_list ctx f = function
  | G.Container (Array, (_, xs, _)) -> List.map f xs
  | _ -> error ("Expected a list for " ^ ctx)

let parse_string_list ctx e =
  let extract_string = function
    | G.L (String (value, _)) -> value
    | _ -> error ("Expected all values in the list to be strings for " ^ ctx)
  in
  parse_list ctx extract_string e

(*****************************************************************************)
(* Sub parsers basic types *)
(*****************************************************************************)
(* let parse_string ctx = function
  | J.String s -> s
  | x ->
      pr2_gen x;
      error (spf "parse_string for %s" ctx)

let parse_strings ctx = function
  | J.Array xs -> List.map (fun t -> parse_string ctx t) xs
  | x ->
      pr2_gen x;
      error (spf "parse_strings for %s" ctx) *)

let parse_bool ctx = function
  | J.String "true" -> true
  | J.String "false" -> false
  | J.Bool b -> b
  | x ->
      pr2_gen x;
      error (spf "parse_bool for %s" ctx)

let parse_int ctx = function
  | J.String s -> (
      try int_of_string s
      with Failure _ -> error (spf "parse_int  for %s" ctx) )
  | J.Float f ->
      let i = int_of_float f in
      if float_of_int i = f then i
      else (
        pr2_gen f;
        error "not an int" )
  | x ->
      pr2_gen x;
      error (spf "parse_int for %s" ctx)

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

let parse_regexp (id, _langs) s =
  try (s, Pcre.regexp s)
  with Pcre.Error exn ->
    raise (E.InvalidRegexpException (id, pcre_error_to_string s exn))

let parse_fix_regex env = function
  | J.Object xs -> (
      match find_fields [ "regex"; "replacement"; "count" ] xs with
      | ( [
            ("regex", Some (J.String regex));
            ("replacement", Some (J.String replacement));
            ("count", count_opt);
          ],
          [] ) ->
          ( parse_regexp env regex,
            Common.map_opt (parse_int "count") count_opt,
            replacement )
      | x ->
          pr2_gen x;
          error "parse_fix_regex" )
  | x ->
      pr2_gen x;
      error "parse_fix_regex"

let parse_equivalences = function
  | J.Array xs ->
      xs
      |> List.map (function
           | J.Object [ ("equivalence", J.String s) ] -> s
           | x ->
               pr2_gen x;
               error "parse_equivalence")
  | x ->
      pr2_gen x;
      error "parse_equivalences"

let parse_paths = function
  | J.Object xs -> (
      match find_fields [ "include"; "exclude" ] xs with
      | [ ("include", inc_opt); ("exclude", exc_opt) ], [] ->
          {
            R.include_ =
              ( match inc_opt with
              | None -> []
              | Some _xs -> [] (* parse_strings "include" xs *) );
            exclude =
              ( match exc_opt with
              | None -> []
              | Some _xs -> [] (* parse_strings "exclude" xs *) );
          }
      | x ->
          pr2_gen x;
          error "parse_paths" )
  | x ->
      pr2_gen x;
      error "parse_paths"

let parse_options json =
  let s = J.string_of_json json in
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

type _env = string * R.xlang

let parse_pattern (id, lang) e =
  let s =
    match e with
    | G.L (String (value, _)) -> value
    | G.N (Id ((value, _), _)) -> value
    | _ -> error ("Expected a string value for " ^ id)
  in
  let start, end_ = Visitor_AST.range_of_any (G.E e) in
  let s_range = (PI.mk_info_of_loc start, PI.mk_info_of_loc end_) in
  match lang with
  | R.L (lang, _) ->
      R.mk_xpat (Sem (H.parse_pattern ~id ~lang s s_range, lang)) s
  | R.LRegex -> failwith "you should not use real pattern with language = none"
  | R.LGeneric -> (
      let src = Spacegrep.Src_file.of_string s in
      match Spacegrep.Parse_pattern.of_src src with
      | Ok ast -> R.mk_xpat (Spacegrep ast) s
      | Error err ->
          raise (H.InvalidPatternException (id, s, "generic", err.msg, s_range))
      )

let find_formula_old rule_info : string * G.expr =
  let find key = (key, Hashtbl.find_opt rule_info key) in
  match
    ( find "pattern",
      find "pattern-either",
      find "patterns",
      find "pattern-regex" )
  with
  | (_, None), (_, None), (_, None), (_, None) ->
      error
        "Expected one of `pattern`, `pattern-either`, `patterns`, \
         `pattern-regex` to be present"
  | (key, Some value), (_, None), (_, None), (_, None)
  | (_, None), (key, Some value), (_, None), (_, None)
  | (_, None), (_, None), (key, Some value), (_, None)
  | (_, None), (_, None), (_, None), (key, Some value) ->
      (key, value)
  | _ ->
      error
        "Expected only one of `pattern`, `pattern-either`, `patterns`, or \
         `pattern-regex`"

let rec parse_formula env (rule_info : (string, G.expr) Hashtbl.t) : R.pformula
    =
  match Hashtbl.find_opt rule_info "match" with
  | Some v -> R.New (parse_formula_new env v)
  | None -> R.Old (parse_formula_old env (find_formula_old rule_info))

and parse_formula_old env ((key, value) : string * G.expr) : R.formula_old =
  let get_pattern str_e = parse_pattern env str_e in
  let get_nested_formula x =
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
  | "pattern-either", xs -> R.PatEither (parse_list key get_nested_formula xs)
  | "patterns", xs -> R.Patterns (parse_list key get_nested_formula xs)
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
          | _ -> error "Expected a metavariable and regex" )
      | _ -> error ("Invalid key for formula_new " ^ key) )
  | _ -> R.Leaf (R.P (parse_pattern env x, None))

(* This is now mutually recursive because of metavariable-pattern: which can
 * contain itself a formula! *)
and _parse_extra env x =
  match x with
  | "metavariable-regex", J.Object xs -> (
      match find_fields [ "metavariable"; "regex" ] xs with
      | ( [
            ("metavariable", Some (J.String metavar));
            ("regex", Some (J.String regexp));
          ],
          [] ) ->
          R.MetavarRegexp (metavar, parse_regexp env regexp)
      | x ->
          pr2_gen x;
          error "metavariable-regex: wrong parse_extra fields" )
  | "metavariable-pattern", J.Object xs -> (
      match find_fields [ "metavariable" ] xs with
      | [ ("metavariable", Some (J.String metavar)) ], rest ->
          let id, _ = env in
          let _env', opt_xlang, rest' =
            match find_fields [ "language" ] rest with
            | [ ("language", Some (J.String s)) ], rest' ->
                let xlang =
                  (* TODO: This code is similar to Main.xlang_of_string. *)
                  if s =$= "none" || s =$= "regex" then R.LRegex
                  else if s =$= "generic" then R.LGeneric
                  else
                    match Lang.lang_of_string_opt s with
                    | None ->
                        raise
                          (E.InvalidLanguageException
                             (id, spf "unsupported language: %s" s))
                    | Some l -> R.L (l, [])
                in
                let env' = (id, xlang) in
                (env', Some xlang, rest')
            | ___else___ -> (env, None, rest)
          in
          let pformula =
            match rest' with
            | [ _x ] -> error "unimplemented" (* parse_formula env' x *)
            | x ->
                pr2_gen x;
                error "wrong rule fields"
          in
          let formula = R.formula_of_pformula pformula in
          R.MetavarPattern (metavar, opt_xlang, formula)
      | x ->
          pr2_gen x;
          error "metavariable-pattern:  wrong parse_extra fields" )
  | "metavariable-comparison", J.Object xs -> (
      match
        find_fields [ "metavariable"; "comparison"; "strip"; "base" ] xs
      with
      | ( [
            ("metavariable", Some (J.String metavariable));
            ("comparison", Some (J.String comparison));
            ("strip", strip_opt);
            ("base", base_opt);
          ],
          [] ) ->
          let comparison = parse_metavar_cond comparison in
          R.MetavarComparison
            {
              R.metavariable;
              comparison;
              strip = Common.map_opt (parse_bool "strip") strip_opt;
              base = Common.map_opt (parse_int "base") base_opt;
            }
      | x ->
          pr2_gen x;
          error "metavariable-comparison: wrong parse_extra fields" )
  | "pattern-where-python", J.String s -> R.PatWherePython s
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
               ( match Lang.lang_of_string_opt s with
               | None ->
                   raise
                     (E.InvalidLanguageException
                        (id, spf "unsupported language: %s" s))
               | Some l -> l ))
      in
      match languages with
      | [] ->
          raise (E.InvalidRuleException (id, "we need at least one language"))
      | x :: xs -> R.L (x, xs) )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let yaml_to_dict rule =
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
  | _ -> error "each rule should be a dictionary of fields"

let get dict f key =
  match Hashtbl.find_opt dict key with
  | Some value -> f key value
  | None -> error ("Missing required field " ^ key)

let get_opt dict f key = Common.map_opt (f key) (Hashtbl.find_opt dict key)

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
  |> List.map (fun rule ->
         let rule_info = yaml_to_dict rule in
         let get f key = get rule_info f key in
         let get_opt f key = get_opt rule_info f key in
         let ( id,
               languages,
               message,
               severity,
               metadata_opt,
               fix_opt,
               fix_regex_opt,
               paths_opt,
               equivs_opt,
               options_opt ) =
           ( get parse_string "id",
             get parse_string_list "languages",
             get parse_string "message",
             get parse_string "severity",
             None,
             get_opt parse_string "fix",
             None,
             None,
             None,
             None )
         in
         let languages = parse_languages ~id languages in
         let formula = parse_formula (id, languages) rule_info in
         let mode = R.Search formula in
         {
           R.id;
           mode;
           message;
           languages;
           file;
           severity = H.parse_severity ~id severity;
           (* optional fields *)
           metadata = metadata_opt;
           fix = fix_opt;
           fix_regexp =
             Common.map_opt (parse_fix_regex (id, languages)) fix_regex_opt;
           paths = Common.map_opt parse_paths paths_opt;
           equivalences = Common.map_opt parse_equivalences equivs_opt;
           options = Common.map_opt parse_options options_opt;
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
