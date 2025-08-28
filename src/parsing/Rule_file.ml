(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Fpath_.Operators
module FT = File_type

(*****************************************************************************)
(* Valid rule filename checks *)
(*****************************************************************************)

(* alt: could define
 * type yaml_kind = YamlRule | YamlTest | YamlFixed | YamlOther
 *)
let is_test_yaml_file (filepath : Fpath.t) : bool =
  (* .test.yaml files are YAML target files rather than config files! *)
  let filepath = !!filepath in
  Filename.check_suffix filepath ".test.yaml"
  || Filename.check_suffix filepath ".test.yml"
  || Filename.check_suffix filepath ".test.fixed.yaml"
  || Filename.check_suffix filepath ".test.fixed.yml"

(* coupling: with Parse_rule.parse_file *)
let is_valid_rule_filename (filename : Fpath.t) : bool =
  match File_type.file_type_of_file filename with
  (* ".yml" or ".yaml" *)
  | FT.Config FT.Yaml -> not (is_test_yaml_file filename)
  (* old: we were allowing Jsonnet before, but better to skip
   * them for now to avoid adding a jsonnet dependency in our docker/CI
   * FT.Config (FT.Json FT.Jsonnet) when not unit_testing -> true
   *)
  | _else_ -> false
