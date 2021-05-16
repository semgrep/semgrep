(*s: semgrep/parsing/Parse_tainting_rules.ml *)
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
module R = Tainting_rule
open Parse_mini_rule (* for the exns *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Parse_tainting_rules.parse_patterns]] *)
let parse_patterns ~id ~lang xs =
  xs
  |> List.map (function
       | `String s -> Parse_mini_rule.parse_pattern ~id ~lang s
       | x ->
           pr2_gen x;
           raise (InvalidYamlException "wrong pattern field"))

(*e: function [[Parse_tainting_rules.parse_patterns]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: function [[Parse_tainting_rules.parse]] *)
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
                 | `O xs ->
                     let id = ref None in
                     let languages = ref None in
                     let lang = ref None in
                     let severity = ref None in
                     let message = ref "" in
                     let sources = ref [] in
                     let sanitizers = ref [] in
                     let sinks = ref [] in

                     let current_id () =
                       match !id with
                       | Some s -> s
                       | None -> raise (InvalidYamlException "no id field")
                     in
                     let current_lang () =
                       match !lang with
                       | Some s -> s
                       | None ->
                           raise (InvalidYamlException "no languages field")
                     in

                     (* ugly: use sort so id/languages are before the source/sink/...
                      * which need an id and lang set
                      *)
                     xs |> Common.sort_by_key_lowfirst
                     |> List.iter (fun x ->
                            match x with
                            | "id", `String s -> id := Some s
                            | "languages", `A langs ->
                                let a, b =
                                  Parse_mini_rule.parse_languages
                                    ~id:(current_id ()) langs
                                in
                                languages := Some a;
                                lang := Some b
                            | "message", `String s -> message := s
                            | "severity", `String s ->
                                severity :=
                                  Some
                                    (Parse_mini_rule.parse_severity
                                       ~id:(current_id ()) s)
                            | "pattern-sources", `A xs ->
                                sources :=
                                  parse_patterns ~id:(current_id ())
                                    ~lang:(current_lang ()) xs
                            | "pattern-sinks", `A xs ->
                                sinks :=
                                  parse_patterns ~id:(current_id ())
                                    ~lang:(current_lang ()) xs
                            | "pattern-sanitizers", `A xs ->
                                sanitizers :=
                                  parse_patterns ~id:(current_id ())
                                    ~lang:(current_lang ()) xs
                            | x ->
                                pr2_gen x;
                                raise (InvalidYamlException "wrong rule field"));
                     let id =
                       match !id with Some s -> s | None -> raise Todo
                     in
                     let message = !message in
                     let languages =
                       match !languages with
                       | Some xs -> xs
                       | None -> raise Todo
                     in
                     let severity =
                       match !severity with Some x -> x | None -> raise Todo
                     in
                     let source =
                       match !sources with
                       | _ :: _ -> !sources
                       | [] -> raise Todo
                     in
                     let sink =
                       match !sinks with _ :: _ -> !sinks | [] -> raise Todo
                     in
                     let sanitizer = !sanitizers in
                     {
                       R.id;
                       message;
                       languages;
                       severity;
                       source;
                       sink;
                       sanitizer;
                     }
                 | x ->
                     pr2_gen x;
                     raise (InvalidYamlException "wrong rule fields"))
      | _ -> raise (InvalidYamlException "missing rules entry as top-level key")
      )
  | Result.Error (`Msg s) -> raise (UnparsableYamlException s)

(*e: function [[Parse_tainting_rules.parse]] *)
(*e: semgrep/parsing/Parse_tainting_rules.ml *)
