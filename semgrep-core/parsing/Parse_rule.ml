(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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

module R = Rule

module E = Parse_mini_rule
module H = Parse_mini_rule

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

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
                 (match Common.sort_by_key_lowfirst xs with
                  | [
                    "id", `String id;
                    "languages", `A langs;
                    "message", `String message;
                    "pattern", `String pattern_string;
                    "severity", `String sev;
                  ] ->
                      let languages, lang = H.parse_languages ~id langs in
                      let pattern = H.parse_pattern ~id ~lang pattern_string in
                      let severity = H.parse_severity ~id sev in
                      let formula = R.Pat pattern in
                      let metadata = [] in
                      { R. id; formula; message; languages; severity; metadata}
                  | x ->
                      pr2_gen x;
                      raise (E.InvalidYamlException "wrong rule fields")
                 )
             | x ->
                 pr2_gen x;
                 raise (E.InvalidYamlException "wrong rule fields")
           )
       | _ -> raise (E.InvalidYamlException "missing rules entry as top-level key")
      )
  | Result.Error (`Msg s) ->
      raise (E.UnparsableYamlException s)
