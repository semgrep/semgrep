module OutT = Semgrep_output_v1_t
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Hashing functions to identify semgrep findings. This is mostly useful
 * for the Semgrep backend to know whether a finding has already been reported
 * because it matches the same hash than a previous finding (from a previous
 * scan).
 * This module provides 2 hashing functions:
 *  - "CI/CLI unique key", using murmur, a.k.a "Syntactic ID"
 *  - "Match-based" ID
 *
 * Why two hashing functions? From Austin:
 * "
 *  We had the original cli match hash (the one with murmur), but that one
 *  changes whenever file formatting changes. "Match-based" ID tried to fix
 *  this, but in the long term we noticed that it doesn't have as much
 *  granularity as cli match, so sometimes some matches will have the same
 *  match-based ID but not be the same. We've discussed getting rid of cli
 *  match (it's also insecure using the murmur hash, especially for secrets...),
 *  but it would be a big effort apparently and break some things.
 * "
 *
 * For full context, see also
 * https://www.notion.so/semgrep/Identifying-unique-findings-match_based_id-and-syntactic_id-cf1a59099c06417d96f777802050ea18#0fde2306cb7c4c5991387b458dcfb064
 *
 * As summarized by Pang:
 *
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
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* From rule_match.py:
   # NOTE: We include the previous scan's rules in the config for consistent fixed status work.
   # For unique hashing/grouping, previous and current scan rules must have distinct check IDs.
   # Hence, previous scan rules are annotated with a unique check ID, while the original ID is kept in metadata.
   # As check_id is used for ci_unique_key, this patch fetches the check ID from metadata for previous scan findings.
*)
let name (c : OutT.cli_match) =
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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let cli_unique_key (c : OutT.cli_match) =
  (* type-wise this is a tuple of string * string * int * int * string * string option *)
  (* # TODO: Once the fixed status work is stable, all findings should
     # fetch the check ID from metadata. This fallback prevents breaking
     # current scan results if an issue arises.
     #
     # TODO: Bring this back.
     # This is necessary so we don't deduplicate taint findings which
     # have different sources.
     #
     # self.match.extra.dataflow_trace.to_json_string
     # if self.match.extra.dataflow_trace
     # else None,
     None,
     # NOTE: previously, we considered self.match.extra.validation_state
     # here, but since in some cases (e.g., with `anywhere`) we generate
     # many matches in certain cases, we want to consider secrets
     # matches unique under the above set of things, but with a priority
     # associated with the validation state; i.e., a match with a
     # confirmed valid state should replace all matches equal under the
     # above key. We can't do that just by not considering validation
     # state since we would pick one arbitrarily, and if we added it
     # below then we would report _both_ valid and invalid (but we only
     # want to report valid, if a valid one is present and unique per
     # above fields). See also `should_report_instead`.
  *)
  ( name c,
    Fpath.to_string c.path,
    c.start.offset,
    c.end_.offset,
    c.extra.message,
    None )

let ci_unique_key ?(index = 0) (c : OutT.cli_match) =
  (* TODO the third element should be "syntactic_context", as defined in rule_match.py:
        # The code that matched, with whitespace and nosem comments removed.
        #
        # This is useful to so that findings can be considered the same
        # when `    5 == 5` is updated to `  5 == 5  # nosemgrep`,
        # and thus CI systems don't retrigger notifications.
        lines = [*self.lines]
        if len(lines) > 0:
            lines[0] = NOSEM_INLINE_COMMENT_RE.sub("", lines[0])
            lines[0] = lines[0].rstrip() + "\n"

        code = "".join(lines)  # the lines end with newlines already
        code = textwrap.dedent(code)
        code = code.strip()
        return code
  *)
  (* TODO the return value in python's ci_unique_key is the hex output of the murmur3 hash,
     here the binary value is returned directly *)
  let repr = Python_str_repr.repr in
  spf "(%s, %s, %s, %u)"
    (repr (name c))
    (repr (Fpath.to_string c.path))
    (repr (String.trim c.extra.lines))
    index
  |> Murmur3.hash128

(* This is a cursed function that calculates everything but the index part
 * of the match_based_id. It's cursed because we need hashes to be exactly
 * the same, but the algorithm used on the python side to generate
 * the final string thats hashed has some python specific quirks.
 *
 * The way match based ID is calculated on the python side
 * is as follows:
 * (see https://github.com/returntocorp/semgrep/blob/2d34ce584d16c4e954349690a5f12fae877a94d6/cli/src/semgrep/rule.py#L289-L334)
 * 1. Sort all top level keys (i.e pattern, patterns etc.) alphabetically
 * 2. For each key: DFS the tree and find all pattern values (i.e. the rhs of pattern: <THING>)
 * 3. Sort all pattern values alphabetically and concatenate them with a space
 * 4. Concatenate all the sorted pattern values with a space
 * 5. Hash the tuple `(sorted_pattern_values, path, rule_id)` w/ blake2b
 * 6. Append the index of the match in the list of matches for the rule (see [index_match_based_ids])
 *
 * Austin: I wrote the initial match based ID, and this one below is a port of it.
 * Looking back it seems like I ended up writing a roundabout version of the below algorithm
 * which is how this function works
 * 1. Same as before
 * 2. for each rule: get all xpatterns, then sort them, and concatenate them with a space
 * 3. Sort all of step 2 results alphabetically and concatenate them with a space
 * 4. Same as step 5 and 6
 *
 * Assumptions:
 * Somewhere it seems that all keys are already sorted alphabetically when the rule is parsed.
 * I have not seen this code. I simply have faith that it is true and this will not change.
 *
 * I'm also hoping that [interpolate_metavariables] works similar to what we do on the python side
 *
 * I have not tested this code beyond checking a bunch of examples manually.
 *
 * There's some weird thing we do w/ join mode. I am hoping that this doesn't matter irl
 *
 * We also don't use pattern sanitizers at all in calculating match based id, which seems
 * weird, but this because if code matches a pattern sanitizer, then its ALWAYS sanitized
 * which means it would never show up as a taint mode finding. So we can safely ignore
 * it, since it shouldn't affect the match based id.
 *)
let match_based_id_partial (rule : Rule.t) (rule_id : Rule_ID.t) metavars path :
    string =
  (* the python implementation does not include sanitizers and propagators; so
   * as to not break fingerprints we ignore sanitizers, too. see above
   * assumptions on why.
   *)
  let mode =
    match rule.mode with
    | `Taint { Rule.sources; sanitizers = _; sinks; propagators = _ } ->
        `Taint { Rule.sources; sanitizers = None; sinks; propagators = [] }
    | (`Search _ | `Extract _ | `Steps _) as mode -> mode
  in
  let formulae = Rule.formula_of_mode mode in
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
  let xpat_strs = List_.map go formulae in
  let sorted_xpat_strs = List.sort String.compare xpat_strs in
  let xpat_str = String.concat " " sorted_xpat_strs in
  let metavars = Option.value ~default:[] metavars in
  let xpat_str_interp =
    Metavar_replacement.interpolate_metavars xpat_str
      (Metavar_replacement.of_out metavars)
  in
  (* We have been hashing w/ this PosixPath thing in python so we must recreate it here  *)
  (* We also have been hashing a tuple formatted as below *)
  let string =
    spf "(%s, PosixPath(%s), %s)"
      (Python_str_repr.repr xpat_str_interp)
      (Python_str_repr.repr path)
      (Python_str_repr.repr (Rule_ID.to_string rule_id))
  in
  let hash = Digestif.BLAKE2B.digest_string string |> Digestif.BLAKE2B.to_hex in
  hash
