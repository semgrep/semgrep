(* Austin Theriault
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Hashing functions to identify semgrep findings.
 *
 * This is mostly useful for the Semgrep backend to know whether a finding has
 * already been reported because it matches the same hash than a previous
 * finding (from a previous scan).
 *
 * This module provides two hashing functions:
 *  - "CI/CLI unique key", using murmur, a.k.a "Syntactic ID"
 *    currently used for our Gitlab output and for baseline finding filtering
 *  - "Match-based" ID
 *    currently used for the fingerprint of a finding in scan and ci and
 *    used by our backend; used also for our JSON and SARIF output.
 *
 * Why two hashing functions? From Austin:
 *   We had the original cli match hash (the one with murmur), but that one
 *   changes whenever file formatting changes. "Match-based" ID tried to fix
 *   this, but in the long term we noticed that it doesn't have as much
 *   granularity as cli match, so sometimes some matches will have the same
 *   match-based ID but not be the same. We've discussed getting rid of cli
 *   match (it's also insecure using the murmur hash, especially for secrets...),
 *   but it would be a big effort apparently and break some things.
 *
 * For full context, see also
 * https://www.notion.so/semgrep/Identifying-unique-findings-match_based_id-and-syntactic_id-cf1a59099c06417d96f777802050ea18#0fde2306cb7c4c5991387b458dcfb064
 *
 * As summarized by Pang:
 * Hashing process:
 * 1. Generate a hash from a combination of:
 *  - The file path
 *  - The rule name
 *  - The rule pattern with the metavariables’ values substituted in
 * 2. Tacking an index at the end _# to differentiate findings from the same
 *    rule within the same file
 *
 * e.g. if we have the hash 123AVDe234 from step 1 and the finding is the first
 * instance seen in the file, the hash we store would be 123AVDe234_0.
 *
 * This is a port of a few functions in rule_match.py and rule.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* From rule_match.py:
   # NOTE: We include the previous scan's rules in the config for consistent
   # fixed status work.
   # For unique hashing/grouping, previous and current scan rules must have
   # distinct check IDs.
   # Hence, previous scan rules are annotated with a unique check ID, while
   # the original ID is kept in metadata.
   # As check_id is used for ci_unique_key, this patch fetches the check ID
   # from metadata for previous scan findings.
*)
let name (c : Out.cli_match) =
  let transient =
    match JSON.member "semgrep.dev" (JSON.from_yojson c.extra.metadata) with
    | Some dev -> (
        match JSON.member "src" dev with
        | Some (JSON.String x) -> String.equal x "previous_scan"
        | Some _
        | None ->
            false)
    | None -> false
  in
  let default = Rule_ID.to_string c.check_id in
  if transient then
    match JSON.member "semgrep.dev" (JSON.from_yojson c.extra.metadata) with
    | Some dev -> (
        match JSON.member "rule" dev with
        | Some rule -> (
            match JSON.member "rule_name" rule with
            | Some (JSON.String rule) -> rule
            | Some _
            | None ->
                default)
        | None -> default)
    | None -> default
  else default

(* TODO(pad): the algorithm below is not what pysemgrep and formula_string do.
 * For instance, pysemgrep does care about the taint labels in a taint source
 * so we need to redesign the whole thing to better match what pysemgrep does.
 *
 * See Unit_reporting.ml for some tests.
 * coupling: rule.py formula_string
 *)
let string_of_formulas (xs : Rule.formula list) : string =
  (* We need to do this as flattening and sorting does not always produce the
   * same result: [[a c] b] become "a c b" while [a c b] becomes "a b c". *)
  let rec go formula =
    match formula.Rule.f with
    | Rule.P p -> fst p.pstr
    | Rule.Anywhere (_, formula)
    | Rule.Inside (_, formula)
    | Rule.Not (_, formula) ->
        go formula
    | Rule.Or (_, formulae)
    | Rule.And (_, formulae) ->
        let xs = List_.map go formulae in
        String.concat " " (List.sort String.compare xs)
  in
  let xpat_strs = List_.map go xs in
  let sorted_xpat_strs = List.sort String.compare xpat_strs in
  let xpat_str = String.concat " " sorted_xpat_strs in
  xpat_str

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* coupling: rule_match.py get_ci_unique_key() *)
let ci_unique_key (c : Out.cli_match) : string * Fpath.t * string * int =
  (* ugly: coupling: Cli_json_output.index_match_based_ids() *)
  let index =
    let fingerprint = c.extra.fingerprint in
    if fingerprint =~ ".*_\\([0-9]+\\)$" then
      int_of_string (Common.matched1 fingerprint)
    else (
      Logs.warn (fun m -> m "wrong fingerprint format: %s" fingerprint);
      0)
  in
  (* TODO the code for syntactic_context should be:
   *     # The code that matched, with whitespace and nosem comments removed.
   *     #
   *     # This is useful to so that findings can be considered the same
   *     # when `    5 == 5` is updated to `  5 == 5  # nosemgrep`,
   *     # and thus CI systems don't retrigger notifications.
   *     lines = [*self.lines]
   *     if len(lines) > 0:
   *         lines[0] = NOSEM_INLINE_COMMENT_RE.sub("", lines[0])
   *         lines[0] = lines[0].rstrip() + "\n"
   *
   *     code = "".join(lines)  # the lines end with newlines already
   *     code = textwrap.dedent(code)
   *     code = code.strip()
   *     return code
   *)
  let syntactic_context = String.trim c.extra.lines in
  (name c, c.path, syntactic_context, index)

(* coupling: rule_match.py get_syntactic_id()
 * TODO? the return value in pysemgrep is the hex output of the
 * murmur3 hash, here the binary value is returned directly
 *)
let syntactic_id (c : Out.cli_match) : string =
  let name_, path, syntactic_context, index = ci_unique_key c in
  let repr = Python_str_repr.repr in
  spf "(%s, %s, %s, %u)" (repr name_) (repr !!path) (repr syntactic_context)
    index
  |> Murmur3.hash128

(* coupling: rule_match.py main part of get_match_based_key() *)
let match_formula_interpolated_str (rule : Rule.t) metavars : string =
  (* We don't use pattern sanitizers (nor propagators) at all in
   * calculating match based id, which seems weird, but this is because if code
   * matches a pattern sanitizer, then its ALWAYS sanitized which means it
   * would never show up as a taint mode finding. So we can safely ignore it,
   * since it shouldn't affect the match based id.
   *)
  let mode =
    match rule.mode with
    | `Taint { Rule.sources; sanitizers = _; sinks; propagators = _ } ->
        `Taint { Rule.sources; sanitizers = None; sinks; propagators = [] }
    | (`Search _ | `Extract _ | `Steps _ | `SCA _) as mode -> mode
  in
  let xs = Rule.formulas_of_mode mode in
  let str = string_of_formulas xs in
  Metavar_replacement.interpolate_metavars str
    (Metavar_replacement.of_out (metavars ||| []))

(* This is a cursed function that calculates everything but the index part
 * of the match_based_id (hence the partial suffix). It is cursed because we
 * need hashes to be exactly the same, but the algorithm used on the Python
 * side to generate the final string thats hashed has some Python specific
 * quirks.
 *
 * The way match based ID is calculated on the python side is as follows:
 * (see https://github.com/semgrep/semgrep/blob/2d34ce584d16c4e954349690a5f12fae877a94d6/cli/src/semgrep/rule.py#L289-L334)
 * 1. Sort all top level keys (i.e pattern, patterns etc.) alphabetically
 * 2. For each key: DFS the tree and find all pattern values (i.e. the rhs of
 *    pattern: <THING>)
 * 3. Sort all pattern values alphabetically and concatenate them with a space
 * 4. Concatenate all the sorted pattern values with a space
 * 5. Hash the tuple `(sorted_pattern_values, path, rule_id)` w/ blake2b
 * 6. Append the index of the match in the list of matches for the rule
 *    (see [index_match_based_ids])
 *
 * coupling: rule_match.py get_match_based_id ()
 *)
let match_based_id_partial (rule : Rule.t) (rule_id : Rule_ID.t) metavars path :
    string =
  let str_interp = match_formula_interpolated_str rule metavars in
  (* We have been hashing w/ this PosixPath thing in Python so we must recreate
   * it here. We also have been hashing a tuple formatted as below.
   *)
  let string =
    spf "(%s, PosixPath(%s), %s)"
      (Python_str_repr.repr str_interp)
      (Python_str_repr.repr path)
      (Python_str_repr.repr (Rule_ID.to_string rule_id))
  in
  let hash = Digestif.BLAKE2B.digest_string string |> Digestif.BLAKE2B.to_hex in
  Logs.debug (fun m -> m "match_key = %s, match_id = %s" string hash);
  hash
