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

let rec yaml_to_json = function
  | `Null -> J.Null
  | `Bool b -> J.Bool b
  | `Float f ->
      (* or use J.Int? *)
      J.Float f
  | `String s -> J.String s
  | `A xs -> J.Array (xs |> List.map yaml_to_json)
  | `O xs -> J.Object (xs |> List.map (fun (k, v) -> (k, yaml_to_json v)))

(*****************************************************************************)
(* Sub parsers basic types *)
(*****************************************************************************)
let parse_string ctx = function
  | J.String s -> s
  | x ->
      pr2_gen x;
      error (spf "parse_string for %s" ctx)

let parse_strings ctx = function
  | J.Array xs -> List.map (fun t -> parse_string ctx t) xs
  | x ->
      pr2_gen x;
      error (spf "parse_strings for %s" ctx)

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

let parse_metavar_cond s =
  try
    let lang = Lang.Python in
    (* todo? use lang in env? *)
    match Parse_pattern.parse_pattern lang ~print_errors:false s with
    | AST_generic.E e -> e
    | _ -> error "not an expression"
  with exn -> raise exn

let parse_regexp s =
  try (s, Pcre.regexp s)
  with Pcre.Error _ as exn ->
    failwith
      (spf "failing to parse regexp %s, error = %s" s (Common.exn_to_s exn))

let parse_extra _env x =
  match x with
  | "metavariable-regex", J.Object xs -> (
      match find_fields [ "metavariable"; "regex" ] xs with
      | ( [
            ("metavariable", Some (J.String metavar));
            ("regex", Some (J.String regexp));
          ],
          [] ) ->
          R.MetavarRegexp (metavar, parse_regexp regexp)
      | x ->
          pr2_gen x;
          error "wrong parse_extra fields" )
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
          error "wrong parse_extra fields" )
  | "pattern-where-python", J.String s -> R.PatWherePython s
  | x ->
      pr2_gen x;
      error "wrong parse_extra fields"

let parse_fix_regex = function
  | J.Object xs -> (
      match find_fields [ "regex"; "replacement"; "count" ] xs with
      | ( [
            ("regex", Some (J.String regex));
            ("replacement", Some (J.String replacement));
            ("count", count_opt);
          ],
          [] ) ->
          ( parse_regexp regex,
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
              | Some xs -> parse_strings "include" xs );
            exclude =
              ( match exc_opt with
              | None -> []
              | Some xs -> parse_strings "exclude" xs );
          }
      | x ->
          pr2_gen x;
          error "parse_paths" )
  | x ->
      pr2_gen x;
      error "parse_paths"

(*****************************************************************************)
(* Sub parsers patterns and formulas *)
(*****************************************************************************)

type _env = string * R.xlang

let parse_pattern (id, lang) s =
  match lang with
  | R.L (lang, _) -> R.mk_xpat (Sem (H.parse_pattern ~id ~lang s, lang)) s
  | R.LNone -> failwith "you should not use real pattern with language = none"
  | R.LGeneric ->
      let src = Spacegrep.Src_file.of_string s in
      let ast = Spacegrep.Parse_pattern.of_src src in
      R.mk_xpat (Spacegrep ast) s

let rec parse_formula_old env (x : string * J.t) : R.formula_old =
  match x with
  | "pattern", J.String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.Pat pattern
  | "pattern-not", J.String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatNot pattern
  | "pattern-inside", J.String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatInside pattern
  | "pattern-not-inside", J.String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatNotInside pattern
  | "pattern-either", J.Array xs ->
      R.PatEither
        (List.map
           (fun x ->
             match x with
             | J.Object [ x ] -> parse_formula_old env x
             | x ->
                 pr2_gen x;
                 error "wrong parse_formula fields")
           xs)
  | "patterns", J.Array xs ->
      R.Patterns
        (List.map
           (fun x ->
             match x with
             | J.Object [ x ] -> parse_formula_old env x
             | x ->
                 pr2_gen x;
                 error "wrong parse_formula fields")
           xs)
  | "pattern-regex", J.String s ->
      let xpat = R.mk_xpat (Regexp (parse_regexp s)) s in
      R.Pat xpat
  | x ->
      let extra = parse_extra env x in
      R.PatExtra extra

let rec parse_formula_new env (x : J.t) : R.formula =
  match x with
  | J.String s -> R.Leaf (R.P (parse_pattern env s, None))
  | J.Object xs -> (
      match xs with
      | [ ("and", J.Array xs) ] ->
          let xs = xs |> List.map (parse_formula_new env) in
          R.And xs
      | [ ("or", J.Array xs) ] ->
          let xs = xs |> List.map (parse_formula_new env) in
          R.Or xs
      | [ ("not", v) ] ->
          let f = parse_formula_new env v in
          R.Not f
      | [ ("inside", J.String s) ] ->
          R.Leaf (R.P (parse_pattern env s, Some Inside))
      | [ ("regex", J.String s) ] ->
          let xpat = R.mk_xpat (R.Regexp (parse_regexp s)) s in
          R.Leaf (R.P (xpat, None))
      | [ ("where", J.String s) ] ->
          R.Leaf (R.MetavarCond (R.CondGeneric (parse_metavar_cond s)))
      | [ ("metavariable_regex", J.Array [ J.String mvar; J.String re ]) ] ->
          R.Leaf (R.MetavarCond (R.CondRegexp (mvar, parse_regexp re)))
      | _ ->
          pr2_gen x;
          error "parse_formula_new" )
  | _ ->
      pr2_gen x;
      error "parse_formula_new"

let parse_formula env (x : string * J.t) : R.pformula =
  match x with
  | "match", v -> R.New (parse_formula_new env v)
  | _ -> R.Old (parse_formula_old env x)

let parse_languages ~id langs =
  match langs with
  | [ J.String ("none" | "regex") ] -> R.LNone
  | [ J.String "generic" ] -> R.LGeneric
  | xs -> (
      let languages =
        xs
        |> List.map (function
             | J.String s -> (
                 match Lang.lang_of_string_opt s with
                 | None ->
                     raise
                       (E.InvalidLanguageException
                          (id, spf "unsupported language: %s" s))
                 | Some l -> l )
             | _ ->
                 raise
                   (E.InvalidRuleException
                      (id, spf "expecting a string for languages")))
      in
      match languages with
      | [] ->
          raise (E.InvalidRuleException (id, "we need at least one language"))
      | x :: xs -> R.L (x, xs) )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let top_fields =
  [
    "id";
    "languages";
    "message";
    "severity";
    (* pattern* handled specialy via parse_formula *)

    (* optional *)
    "metadata";
    "fix";
    "fix-regex";
    "paths";
    "equivalences";
  ]

let parse_json file json =
  match json with
  | J.Object [ ("rules", J.Array xs) ] ->
      xs
      |> List.map (fun v ->
             match v with
             | J.Object xs -> (
                 match find_fields top_fields xs with
                 (* coupling: the order of the fields below must match the
                  * order in top_fields. *)
                 | ( [
                       ("id", Some (J.String id));
                       ("languages", Some (J.Array langs));
                       ("message", Some (J.String message));
                       ("severity", Some (J.String sev));
                       ("metadata", metadata_opt);
                       ("fix", fix_opt);
                       ("fix-regex", fix_regex_opt);
                       ("paths", paths_opt);
                       ("equivalences", equivs_opt);
                     ],
                     rest ) ->
                     let languages = parse_languages ~id langs in
                     let formula =
                       match rest with
                       | [ x ] -> parse_formula (id, languages) x
                       | x ->
                           pr2_gen x;
                           error "wrong rule fields"
                     in
                     {
                       R.id;
                       formula;
                       message;
                       languages;
                       file;
                       severity = H.parse_severity ~id sev;
                       (* optional fields *)
                       metadata = metadata_opt;
                       fix = Common.map_opt (parse_string "fix") fix_opt;
                       fix_regexp = Common.map_opt parse_fix_regex fix_regex_opt;
                       paths = Common.map_opt parse_paths paths_opt;
                       equivalences =
                         Common.map_opt parse_equivalences equivs_opt;
                     }
                 | x ->
                     pr2_gen x;
                     error "wrong rule fields" )
             | x ->
                 pr2_gen x;
                 error "wrong rule fields")
  | _ -> error "missing rules entry as top-level key"

let parse file =
  let json =
    match FT.file_type_of_file file with
    | FT.Config FT.Yaml -> (
        let str = Common.read_file file in
        let yaml_res = Yaml.of_string str in
        match yaml_res with
        | Result.Ok v -> yaml_to_json v
        | Result.Error (`Msg s) -> raise (E.UnparsableYamlException s) )
    | FT.Config FT.Json -> J.load_json file
    | FT.Config FT.Jsonnet ->
        Common2.with_tmp_file ~str:"parse_rule" ~ext:"json" (fun tmpfile ->
            let cmd = spf "jsonnet %s -o %s" file tmpfile in
            let n = Sys.command cmd in
            if n <> 0 then failwith (spf "error executing %s" cmd);
            J.load_json tmpfile)
    | _ ->
        failwith
          (spf "wrong rule format, only JSON/YAML/JSONNET are valid:%s:" file)
  in
  parse_json file json

(*e: semgrep/parsing/Parse_rule.ml *)
