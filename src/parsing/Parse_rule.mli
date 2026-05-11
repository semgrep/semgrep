(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Parse a rule file, either in YAML or JSON (or even Jsonnet) format
   depending on the filename extension.

   The parser accepts invalid rules, skips them, and returns them in
   the list of errors.
   This will not return [Error (Rule.InvalidRule _)] as the main result.
   However, this function may return the other instances of
   [Error (x : Rule.Error.t)], (e.g., [Error (Rule.InvalidYaml _)]).

   rewrite_rule_ids, if not None, provides what's needed to parse the rule
   ID 'foo' as 'path.to.foo'. This is the default behavior for 'semgrep scan'.
   See the command-line option --rewrite-rule-ids.

   When [par_conf] is an [Eio_executor] and [num_jobs > 1], the per-rule
   validation step that runs after deserialization is parallelized across
   [num_jobs] domains.
*)
val parse_and_filter_invalid_rules :
  ?par_conf:Parallelism_config.t ->
  ?num_jobs:int ->
  ?rewrite_rule_ids:(Rule_ID.t -> Rule_ID.t) ->
  Fpath.t ->
  (Rule_error.rules_and_invalid, Rule_error.t) result

(* This is used for parsing -e/-f extended patterns in Run_semgrep.ml
 * and now also in osemgrep Config_resolver.ml.
 * This can raise Failure for spacegrep parsing errors, and returns
 * Error (Rule.InvalidRegexp _) for regexp errors.
 *)
val parse_xpattern :
  Analyzer.t -> string Rule.wrap -> (Xpattern.t, Rule_error.t) result

val parse_fake_xpattern :
  Analyzer.t -> string -> (Xpattern.t, Rule_error.t) result

(* This should be used mostly in testing code. Otherwise you should
 * use parse_and_filter_invalid_rules.
 * This function may raise (Rule.Err ....) or Assert_failure (when
 * there are invalid rules).
 *)
val parse :
  ?par_conf:Parallelism_config.t ->
  ?num_jobs:int ->
  Fpath.t ->
  (Rule.rules, Rule_error.t) result

(* Internals, used by osemgrep to setup a ojsonnet import hook.
 * The filename parameter is just used in case of missing 'rules:'
 * to report the error on the first line of the file.
 *)
val parse_generic_ast :
  ?error_recovery:bool ->
  ?par_conf:Parallelism_config.t ->
  ?num_jobs:int ->
  ?rewrite_rule_ids:(Rule_ID.t -> Rule_ID.t) ->
  Fpath.t ->
  AST_generic.program ->
  (Rule_error.rules_and_invalid, Rule_error.t) result
