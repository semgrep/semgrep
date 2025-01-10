(* Yoann Padioleau
 *
 * Copyright (C) 2019-2024 Semgrep Inc.
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
open Rule

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Rule errors, mostly created during rule parsing. Those errors can be
 * divided in two categories: "recoverable" and "fatal" errors.
 * TODO: what about "skippable" errors, a subcategory of "recoverable"?
 *
 * A recoverable error is one that does not prevent to parse the rest of
 * the file containing the rules. For example, a rule containing a bad
 * language. This leads to split rules in regular fully-parsed rules, and
 * invalid rules where we only remember the rule ID and position.
 *
 * history: used to be in Rule.ml but this file was getting really big.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Those are recoverable errors; We can just skip the rules containing them.
 * TODO? put in semgrep_output_v1.atd?
 *)
type invalid_rule = invalid_rule_kind * Rule_ID.t * Tok.t

and invalid_rule_kind =
  | InvalidLanguage of string (* the language string *)
  (* TODO: the Parse_info.t for InvalidPattern is not precise for now;
   * it corresponds to the start of the pattern *)
  | InvalidPattern of
      string (* pattern *)
      * Analyzer.t
      * string (* exn *)
      * string list (* yaml path *)
  | InvalidRegexp of string (* PCRE error message *)
  | DeprecatedFeature of string (* e.g., pattern-where-python: *)
  | MissingPositiveTermInAnd
  | IncompatibleRule of
      Semver_.t (* this version of Semgrep *)
      * (Semver_.t option (* minimum version supported by this rule *)
        * Semver_.t option (* maximum version *))
  | MissingPlugin of string (* error message *)
  | InvalidOther of string
[@@deriving show]

type rules_and_invalid = rules * invalid_rule list

(* General errors *)
type error_kind =
  | InvalidRule of invalid_rule
  (* we can't recover from those *)
  | InvalidYaml of string * Tok.t
  | DuplicateYamlKey of string * Tok.t
  | UnparsableYamlException of string
[@@deriving show]

type t = { rule_id : Rule_ID.t option; file : Fpath.t; kind : error_kind }
[@@deriving show]

(*
    You must provide a rule ID for a rule to be reported properly as an invalid
    rule.
    TODO: make the argument not optional because it's important to not forget
    to specify a rule ID whenever possible? It was originally but pad
    refactored the code but maybe we should revert that part and restore
    ~rule_id instead of ?rule_id

    It's not great that we set this temporary file below, but because of how we
    guard `Parse_rule`, this should definitely be populated in any entry point
   functions other than `parse_xpattern` and `parse_fake_xpattern`.

   alt: we could try to provide the `file` at each call-site of
   `mk_error` itself, such as in the `Parse_rule_helpers.env`. This is actually
   a pretty hard refactor, though, as not all of those call-sites have easy
   access to a file or an env.
   For instance, yaml_error calls Rule.Error.mk_error, but yaml_error is
   explicitly called by take_no_env. We can't thread an env through there,
   the point is that it doesn't take an env, so we would need to do a broader
   refactor. This will do to start, as I anticipate we probably won't be
   refactoring this module to add more entry points at any time.
*)
let mk_error ?rule_id kind = { rule_id; file = Fpath_.fake_file; kind }

(* for intercepting an error before it leaves `Parse_rule`, by augmenting it with
     the file path
*)
let augment_with_file (file : Fpath.t) (error : t) : t = { error with file }

(*****************************************************************************)
(* String-of *)
(*****************************************************************************)

let string_of_invalid_rule_kind = function
  | InvalidLanguage language -> spf "invalid language %s" language
  | InvalidRegexp message -> spf "invalid regex %s" message
  (* coupling: this is actually intercepted in
   * Semgrep_error_code.exn_to_error to generate a PatternParseError instead
   * of a RuleParseError *)
  | InvalidPattern (pattern, analyzer, message, _yaml_path) ->
      spf
        "Invalid pattern for %s: %s\n\
         ----- pattern -----\n\
         %s\n\
         ----- end pattern -----\n"
        (Analyzer.to_string analyzer)
        message pattern
  | MissingPositiveTermInAnd ->
      "you need at least one positive term (not just negations or conditions)"
  | DeprecatedFeature s -> spf "deprecated feature: %s" s
  | IncompatibleRule (cur, (Some min_version, None)) ->
      spf "This rule requires upgrading Semgrep from version %s to at least %s"
        (Semver.to_string cur)
        (Semver.to_string min_version)
  | IncompatibleRule (cur, (None, Some max_version)) ->
      spf
        "This rule is no longer supported by Semgrep. The last compatible \
         version was %s. This version of Semgrep is %s"
        (Semver.to_string max_version)
        (Semver.to_string cur)
  | IncompatibleRule (cur, (Some min_version, Some max_version)) ->
      spf
        "This rule requires a version of Semgrep within [%s, %s] but we're \
         using version %s"
        (Semver.to_string min_version)
        (Semver.to_string max_version)
        (Semver.to_string cur)
  | IncompatibleRule (_, (None, None)) -> assert false
  | MissingPlugin msg -> msg
  | InvalidOther s -> s

let string_of_invalid_rule ((kind, rule_id, pos) : invalid_rule) =
  spf "invalid rule %s, %s: %s"
    (Rule_ID.to_string rule_id)
    (Tok.stringpos_of_tok pos)
    (string_of_invalid_rule_kind kind)

let string_of_error (error : t) : string =
  match error.kind with
  | InvalidRule x -> string_of_invalid_rule x
  | InvalidYaml (msg, pos) ->
      spf "invalid YAML, %s: %s" (Tok.stringpos_of_tok pos) msg
  | DuplicateYamlKey (key, pos) ->
      spf "invalid YAML, %s: duplicate key %S" (Tok.stringpos_of_tok pos) key
  | UnparsableYamlException s ->
      (* TODO: what's the string s? *)
      spf "unparsable YAML: %s" s

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(*
   Determine if an error can be skipped. This is for presumably well-formed
   rules that aren't compatible with the current version of semgrep
   and shouldn't cause a failure.
*)
let is_skippable_error (kind : invalid_rule_kind) : bool =
  match kind with
  | InvalidLanguage _
  | InvalidPattern _
  | InvalidRegexp _
  | DeprecatedFeature _
  | MissingPositiveTermInAnd
  | InvalidOther _ ->
      false
  | IncompatibleRule _
  | MissingPlugin _ ->
      true
