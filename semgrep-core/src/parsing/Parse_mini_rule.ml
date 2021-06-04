(*s: semgrep/parsing/Parse_mini_rule.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module R = Mini_rule

(*s: exception [[Parse_rules.InvalidRuleException]] *)
exception InvalidRuleException of string * string

(*e: exception [[Parse_rules.InvalidRuleException]] *)
(*s: exception [[Parse_rules.InvalidLanguageException]] *)
exception InvalidLanguageException of string * string

(*e: exception [[Parse_rules.InvalidLanguageException]] *)
(*s: exception [[Parse_rules.InvalidPatternException]] *)
exception InvalidPatternException of string * string * string * string

(*e: exception [[Parse_rules.InvalidPatternException]] *)

exception InvalidRegexpException of string * string

(*s: exception [[Parse_rules.UnparsableYamlException]] *)
exception UnparsableYamlException of string

(*e: exception [[Parse_rules.UnparsableYamlException]] *)
(*s: exception [[Parse_rules.InvalidYamlException]] *)
exception InvalidYamlException of string

(*e: exception [[Parse_rules.InvalidYamlException]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Parse_rules.parse_severity]] *)
(* also used in Parse_tainting_rules.ml *)
let parse_severity ~id s =
  match s with
  | "ERROR" -> R.Error
  | "WARNING" -> R.Warning
  | "INFO" -> R.Info
  | s ->
      raise
        (InvalidRuleException
           (id, spf "Bad severity: %s (expected ERROR, WARNING or INFO)" s))

(*e: function [[Parse_rules.parse_severity]] *)

(*s: function [[Parse_rules.parse_pattern]] *)
let parse_pattern ~id ~lang pattern =
  (* todo? call Normalize_ast.normalize here? *)
  try Parse_pattern.parse_pattern lang ~print_errors:false pattern with
  | Timeout -> raise Timeout
  | UnixExit n -> raise (UnixExit n)
  | exn ->
      raise
        (InvalidPatternException
           (id, pattern, Lang.string_of_lang lang, Common.exn_to_s exn))

(*e: function [[Parse_rules.parse_pattern]] *)

(*s: function [[Parse_rules.parse_languages]] *)
let parse_languages ~id langs =
  let languages =
    langs
    |> List.map (function
         | `String s -> (
             match Lang.lang_of_string_opt s with
             | None ->
                 raise
                   (InvalidLanguageException
                      (id, spf "unsupported language: %s" s))
             | Some l -> l )
         | _ ->
             raise
               (InvalidRuleException (id, spf "expecting a string for languages")))
  in
  let lang =
    match languages with
    | [] -> raise (InvalidRuleException (id, "we need at least one language"))
    | x :: _xs -> x
  in
  (languages, lang)

(*e: function [[Parse_rules.parse_languages]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Parse_rules.parse]] *)
let parse file =
  let str = Common.read_file file in
  let yaml_res = Yaml.of_string str in
  match yaml_res with
  | Result.Ok v -> (
      match v with
      | `O [ ("rules", `A xs) ] ->
          xs
          |> List.map (fun v ->
                 match v with
                 | `O xs -> (
                     match Common.sort_by_key_lowfirst xs with
                     | [
                      ("id", `String id);
                      ("languages", `A langs);
                      ("message", `String message);
                      ("pattern", `String pattern_string);
                      ("severity", `String sev);
                     ] ->
                         let languages, lang = parse_languages ~id langs in
                         let pattern = parse_pattern ~id ~lang pattern_string in
                         let severity = parse_severity ~id sev in
                         {
                           R.id;
                           pattern;
                           message;
                           languages;
                           severity;
                           pattern_string;
                         }
                     | x ->
                         pr2_gen x;
                         raise (InvalidYamlException "wrong rule fields") )
                 | x ->
                     pr2_gen x;
                     raise (InvalidYamlException "wrong rule fields"))
      | _ -> raise (InvalidYamlException "missing rules entry as top-level key")
      )
  | Result.Error (`Msg s) -> raise (UnparsableYamlException s)

(*e: function [[Parse_rules.parse]] *)

(*
      let sgrep_string = Common.matched1 s in
      let title, msg = match group with
        | title :: description -> title, Common2.unlines description
        | _ -> failwith ("sgrep_lint: expected \"[title]\\n\\n[description]\"")
      in
      Parse_generic.parse_pattern !lang sgrep_string,
      title,
      (* yes ocaml regexps are not that good ... *)
      (if msg =~ "^\\([A-Z]+\\):\\(\\(.\\|\n\\)*\\)"
       then
         let (error_kind, rest_msg) = Common.matched2 msg in
         (match error_kind with
         | _ -> failwith ("sgrep_lint: wrong format: " ^ msg)
         )
        else failwith ("sgrep_lint: wrong format: " ^ msg)
      )
    else raise Impossible
  )
*)
(*e: semgrep/parsing/Parse_mini_rule.ml *)
