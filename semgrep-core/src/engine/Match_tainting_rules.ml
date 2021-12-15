(* Yoann Padioleau, Iago Abal
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

module FM = File_and_more
module RP = Report

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let lazy_force x = Lazy.force x [@@profiling]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let check ~match_hook default_config taint_rules file_and_more =
  match taint_rules with
  | [] -> RP.empty_semgrep_result
  | __else__ ->
      let { FM.file; xlang; lazy_ast_and_errors; _ } = file_and_more in
      let lang =
        match xlang with
        | L (lang, _) -> lang
        | LGeneric
        | LRegex ->
            failwith "taint-mode and generic/regex matching are incompatible"
      in
      let (ast, errors), parse_time =
        Common.with_time (fun () -> lazy_force lazy_ast_and_errors)
      in
      let matches, match_time =
        Common.with_time (fun () ->
            Tainting_generic.check match_hook default_config taint_rules file
              lang ast)
      in
      {
        RP.matches;
        errors;
        skipped = [];
        profiling = { RP.parse_time; match_time };
      }
