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
 *  - use the streaming API of Yaml.mli to get position information for
 *    precise error location? At the same time in a the long term we want
 *    to use JSON and jsonnet, so we might get anyway a line location
 *    in a generated file, so maybe better to give error location by
 *    describing the line and what is wrong wit it.
 *  - support JSON in addition to Yaml, convert yaml to JSON (or reverse)
 *  - Move the H.xxx here and get rid of Parse_mini_rule.ml
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could use a hash to accelerate things *)
let rec find_fields flds xs =
  match flds with
  | [] -> [], xs
  | fld::flds ->
      let fld_match = List.assoc_opt fld xs in
      let xs = List.remove_assoc fld xs in
      let (matches, rest) = find_fields flds xs in
      (fld, fld_match)::matches, rest

let error s =
  raise (E.InvalidYamlException s)

let rec yaml_to_json = function
  | `Null -> J.Null
  | `Bool b -> J.Bool b
  | `Float f ->
      (* or use J.Int? *)
      J.Float f
  | `String s -> J.String s
  | `A xs -> J.Array (xs |> List.map yaml_to_json)
  | `O xs ->
      J.Object (xs |> List.map (fun (k, v) -> k, yaml_to_json v))

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)
let parse_string ctx = function
  | `String s -> s
  | x -> pr2_gen x; error (spf "parse_string for %s" ctx)

let parse_strings ctx = function
  | `A xs -> List.map (parse_string ctx) xs
  | x -> pr2_gen x; error (spf "parse_strings for %s" ctx)

let parse_bool ctx = function
  | `String "true" -> true
  | `String "false" -> false
  | `Bool b -> b
  | x -> pr2_gen x; error (spf "parse_bool for %s" ctx)

let parse_int ctx = function
  | `String s ->
      (try int_of_string s
       with Failure _ -> error (spf "parse_int  for %s" ctx)
      )
  | `Float f ->
      let i = int_of_float f in
      if float_of_int i = f
      then i
      else begin pr2_gen f; error "not an int" end
  | x -> pr2_gen x; error (spf "parse_int for %s" ctx)

type _env = (string * R.xlang)

let parse_pattern (id, lang) s =
  match lang with
  | R.L (lang, _) ->
      { R.pstr = s; p = Sem (H.parse_pattern ~id ~lang s) }
  | R.LNone ->
      failwith ("you should not use real pattern with language = none")
  | R.LGeneric ->
      (* todo: call spacegrep *)
      { R.pstr = s; p = Space s}

let rec parse_formula_old env (x: string * Yaml.value) : R.formula_old =
  match x with
  | "pattern", `String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.Pat pattern
  | "pattern-not", `String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatNot pattern

  | "pattern-inside", `String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatInside pattern
  | "pattern-not-inside", `String pattern_string ->
      let pattern = parse_pattern env pattern_string in
      R.PatNotInside pattern

  | "pattern-either", `A xs ->
      R.PatEither (List.map (fun x ->
        match x with
        | `O [x] -> parse_formula_old env x
        | x ->
            pr2_gen x;
            error "wrong parse_formula fields"
      ) xs)
  | "patterns", `A xs ->
      R.Patterns (List.map (fun x ->
        match x with
        | `O [x] -> parse_formula_old env x
        | x ->
            pr2_gen x;
            error "wrong parse_formula fields"
      ) xs)
  | x ->
      let extra = parse_extra env x in
      R.PatExtra extra

and parse_extra _env x =
  match x with
  | "metavariable-regex", `O xs ->
      (match find_fields ["metavariable";"regex"] xs with
       | ["metavariable", Some (`String metavar);
          "regex", Some (`String regexp);
         ], [] ->
           R.MetavarRegexp (metavar, regexp)
       | x ->
           pr2_gen x;
           error "wrong parse_extra fields"
      )

  | "metavariable-comparison", `O xs ->
      (match find_fields ["metavariable";"comparison";"strip";"base"] xs with
       | ["metavariable", Some (`String metavariable);
          "comparison", Some (`String comparison);
          "strip", strip_opt;
          "base", base_opt;
         ], [] ->
           R.MetavarComparison
             { R. metavariable; comparison;
               strip = Common.map_opt (parse_bool "strip") strip_opt;
               base = Common.map_opt (parse_int "base") base_opt;
             }
       | x ->
           pr2_gen x;
           error "wrong parse_extra fields"
      )

  | "pattern-regex", `String s ->
      R.PatRegexp s
  | "pattern-where-python", `String s ->
      R.PatWherePython s

  | x ->
      pr2_gen x;
      error "wrong parse_extra fields"

let parse_formula env (x: string * Yaml.value) : R.pformula =
  match x with
  | _ -> R.Old (parse_formula_old env x)


let parse_languages ~id langs =
  match langs with
  | [`String "none"] -> R.LNone
  | [`String "generic"] -> R.LGeneric
  | xs ->
      let languages = xs |> List.map (function
        | `String s ->
            (match Lang.lang_of_string_opt s with
             | None -> raise (E.InvalidLanguageException (id, (spf "unsupported language: %s" s)))
             | Some l -> l
            )
        | _ -> raise (E.InvalidRuleException (id, (spf "expecting a string for languages")))
      )
      in
      match languages with
      | [] -> raise (E.InvalidRuleException (id, "we need at least one language"))
      | x::xs -> R.L (x, xs)

let parse_fix_regex = function
  | `O xs ->
      (match find_fields ["regex";"replacement";"count"] xs with
       | ["regex", Some (`String regex);
          "replacement", Some (`String replacement);
          "count", count_opt;
         ], [] ->
           (regex,
            Common.map_opt (parse_int "count") count_opt,
            replacement)
       | x -> pr2_gen x; error "parse_fix_regex"
      )
  | x -> pr2_gen x; error "parse_fix_regex"

let parse_equivalences = function
  | `A xs ->
      xs |> List.map (function
        | `O ["equivalence", `String s] -> s
        | x -> pr2_gen x; error "parse_equivalence"
      )
  | x -> pr2_gen x; error "parse_equivalences"

let parse_paths = function
  | `O xs ->
      (match find_fields ["include"; "exclude"] xs with
       | ["include", inc_opt;
          "exclude", exc_opt;
         ], [] ->
           { R.include_ =
               (match inc_opt with
                | None -> [] | Some xs -> parse_strings "include" xs);
             exclude =
               (match exc_opt with
                | None -> [] | Some xs -> parse_strings "exclude" xs);
           }
       | x -> pr2_gen x; error "parse_paths"
      )
  | x -> pr2_gen x; error "parse_paths"

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let top_fields = [
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

let parse file =
  let str = Common.read_file file in
  let yaml_res = Yaml.of_string str in
  match yaml_res with
  | Result.Ok v ->
      (match v with
       | `O ["rules", `A xs] ->
           xs |> List.map (fun v ->
             match v with
             | `O xs ->
                 (match find_fields top_fields xs with
                  (* coupling: the order of the fields below must match the
                   * order in top_fields. *)
                  | [
                    "id", Some (`String id);
                    "languages", Some (`A langs);
                    "message", Some (`String message);
                    "severity", Some (`String sev);

                    "metadata", metadata_opt;
                    "fix", fix_opt;
                    "fix-regex", fix_regex_opt;
                    "paths", paths_opt;
                    "equivalences", equivs_opt;

                  ], rest ->
                      let languages = parse_languages ~id langs in
                      let formula =
                        match rest with
                        | [x] ->
                            parse_formula (id, languages) x
                        | x ->
                            pr2_gen x;
                            error "wrong rule fields"
                      in
                      { R. id; formula; message; languages; file;
                        severity = H.parse_severity ~id sev;
                        (* optional fields *)
                        metadata =
                          Common.map_opt yaml_to_json metadata_opt;
                        fix =
                          Common.map_opt (parse_string "fix") fix_opt;
                        fix_regexp =
                          Common.map_opt parse_fix_regex fix_regex_opt;
                        paths =
                          Common.map_opt parse_paths paths_opt;
                        equivalences =
                          Common.map_opt parse_equivalences equivs_opt;
                      }
                  | x ->
                      pr2_gen x;
                      error "wrong rule fields"
                 )
             | x ->
                 pr2_gen x;
                 error "wrong rule fields"
           )
       | _ -> error  "missing rules entry as top-level key"
      )
  | Result.Error (`Msg s) ->
      raise (E.UnparsableYamlException s)

(*e: semgrep/parsing/Parse_rule.ml *)
