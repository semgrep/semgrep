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
(* ex: foo.yaml, foo.yml, but not foo.test.yaml.
 *
 * Note that even if parse() above accepts JSON (and Jsonnet) files,
 * foo.json (and foo.jsonnet) are currently not considered
 * valid_rule_filename.
 *
 * This function is currently used for osemgrep, to get all
 * the valid rule files when using --config <DIR>,
 * and also in Test_engine.ml.
 *)
val is_valid_rule_filename : Fpath.t -> bool
