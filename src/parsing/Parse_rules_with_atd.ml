(* Yoann Padioleau
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
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Experiment to parse rules using semgrep-interfaces/rule_schema_v2.atd
 * instead of Parse_rule.ml or even rule_schema_v1.yaml
 *
 * See the comment in rule_schema_v2.atd for more info.
 *
 * Note that ATD by default does error recovery on variant names it does not
 * know or field names it does not know. However, for schema validation we
 * actually want to raise errors, hence the use of -j-strict-fields in
 * src/core/dune when compiling rule_schema_v2.atd.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_rules_v2 file =
  match File_type.file_type_of_file file with
  | Config Json ->
      let json_str = UFile.read_file file in
      let rules = Rule_schema_v2_j.rules_of_string json_str in
      rules
  | Config Yaml ->
      let str = UFile.read_file file in
      let ezjsonm = Yaml.of_string_exn str in
      let yojson = JSON.ezjsonm_to_yojson ezjsonm in
      let json_str = Yojson.Basic.to_string ~std:true yojson in
      let rules = Rule_schema_v2_j.rules_of_string json_str in
      rules
  | _else_ -> failwith (spf "format not handled for %s" !!file)
