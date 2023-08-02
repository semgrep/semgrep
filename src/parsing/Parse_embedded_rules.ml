(* Sophia Roshal
 *
 * Copyright (C) 2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Instead of reading rules from a semgrep.yml file, the idea of this module
 * is to let users "embed" semgrep rules inside their code, with special
 * markers (see below), which can be automatically extracted
 * from semgrep itself.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* TODO: support <semgrep format="jsonnet">
 * Note that I use concatenation below to avoid having this string considered
 * as starting a semgrep rule :)
 *)
let start_string = "<" ^ "semgrep" ^ ">"
let end_string = "</semgrep>"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find content pattern loc =
  let re = Str.regexp_string pattern in
  try Str.search_forward re content loc with
  | Not_found -> -1

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let has_embedded_rules fpath =
  let file_contents = File.read_file fpath in
  find file_contents start_string 0 > -1

let parse_embedded_rules fpath =
  let file_contents = File.read_file fpath in

  let rec parse_embedded_rules_helper (acc_rule, acc_error) prev_loc str =
    let next_loc = find str start_string prev_loc in
    if next_loc = -1 then (acc_rule, acc_error)
    else
      let end_loc = find str end_string (next_loc + 1) in
      let rule_string =
        String.sub str
          (next_loc + String.length start_string)
          (end_loc - (next_loc + String.length start_string))
      in
      (* TODO: support also .json and especially .jsonnet rules! *)
      let temp_file = Filename.open_temp_file "temp_rule" ".yaml" in
      Out_channel.output_string (snd temp_file) rule_string;
      Out_channel.close (snd temp_file);
      let path = Fpath.of_string (fst temp_file) in
      match path with
      | Ok p ->
          (* TODO? maybe we want to pass the parse_rule function as a parameter
           * so we can pass Parse_rule.parse variants such as
           * Rule_fetching.load_rules_from_source which for example handle
           * rules in jsonnet
           *)
          let rules, errors = Parse_rule.parse_and_filter_invalid_rules p in
          parse_embedded_rules_helper
            (rules @ acc_rule, errors @ acc_error)
            end_loc str
      | Error _ -> failwith "not a correct file path"
  in
  parse_embedded_rules_helper ([], []) 0 file_contents
