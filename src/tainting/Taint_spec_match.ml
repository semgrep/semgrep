(* Iago Abal
 *
 * Copyright (C) 2023-present Semgrep Inc
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
module PM = Core_match
module Effect = Shape_and_sig.Effect

type overlap = float

type 'spec t = {
  spec : 'spec;
  spec_id : string;
  spec_pm : PM.t;
  range : Range.t;
  overlap : overlap;
}

let is_exact x = x.overlap > 0.99
let sink_of_match x = { Effect.pm = x.spec_pm; rule_sink = x.spec }

let _show x =
  Range.content_at_range x.spec_pm.path.internal_path_to_content x.range

type any = Any : 'a t -> any

(* NOTE "Best matches":
 * (See note "Taint-tracking via ranges" in 'Match_tainting_mode.ml' for context.)
 *
 * We use sub-range checks to determine whether a piece of code is a source/sink/etc,
 * and this can lead to unintuitive results. For example, in the past,
 *
 *     sink(if tainted then ok1 else ok2)
 *
 * was reported as a tainted sink despite `if tainted then ok1 else ok2` is actually
 * not tainted. The problem was that `tainted` is inside what `sink(...)` matches,
 * and using a sub-range check we end up considering `tainted` itself as a sink.
 *
 * Unfortunately simply checking for exact matches is fragile, because sometimes we
 * are not able to match sources/sinks/etc exactly. For example, if you want
 * `echo ...;` to be a sink in PHP, you cannot omit the ';' as `echo` is not an
 * expression. But in the IL, `echo` is represented as a `Call` instruction and
 * the ';' is not part of the`iorig`.
 *
 * So, given a source/sink/etc specification, we look for all the sub-matches of
 * that spec on every instruction and (sub)expression in the CFG, and we try find
 * the "best matches".
 *
 * The way we determine the best-matches is by taking the sub-matches two by two,
 * if one sub-match is contained inside the other, then we take the larger one as
 * the canonical. We repeat this until we find the ones that fit the spec the best.
 * We store the canonical matches in this 'Best_matches' data structure, and we use
 * these "best matches" as a practical definition of what an "exact match" is.
 *
 * For example, if the sink specification is `echo ...;`, and the code we have is
 * `echo $_GET['foo'];`, the sub-matches are `$_GET`, `'foo'` and n`$_GET['foo']`.
 * And this method will determine that `echo $_GET['foo']` is the best match of all,
 * and that is what we will consider an "exact" match for the `echo ...;` sink.
 *
 * In our example above, `tainted`, `ok1` and `ok2` are all potential sub-matches,
 * but not the "best" ones. The sub match `sink(if tainted then ok1 else ok2)`,
 * contains them all and it will be considered the "best"/"exact" match for that
 * sink. Thus, we will not have a FP due to `tainted` being considered a sink.
 *)
module Best_matches = struct
  (* For m, m' in S.t, not (m.range $<=$ m'.range) && not (m'.range $<=$ m.range) *)
  module S = Set.Make (struct
    type t = any

    (* This compare function is subtle but it allows both `add` and `is_best_match`
     * to be simple and quite efficient. *)
    let compare (Any m1) (Any m2) =
      let sink_id_cmp = String.compare m1.spec_id m2.spec_id in
      if sink_id_cmp <> 0 then sink_id_cmp
      else
        (* If m1 is contained in m2 or vice-versa, then they are the *same* sink match.
         * We only want to keep one match per sink, the best match! *)
        let r1 = m1.range in
        let r2 = m2.range in
        if Range.(r1 $<=$ r2 || r2 $<=$ r1) then 0 else Stdlib.compare r1 r2
  end)

  type t = S.t

  let empty = S.empty

  let _debug xs =
    xs |> S.elements
    |> List_.map (fun (Any m) ->
           m.spec_id ^ ":"
           ^ Range.content_at_range m.spec_pm.path.internal_path_to_content
               m.range)
    |> String.concat " ; "

  let rec add (Any m' as x') best_matches =
    (* We check if we have another match for the *same* source/sink/etc spec
     * (i.e., same 'source_id'/'sink_id'/etc), and if so we must keep the
     * best match and drop the other one. *)
    match S.find_opt x' best_matches with
    | None -> S.add x' best_matches
    | Some (Any m as x) ->
        let r = m.range in
        let r' = m'.range in
        (* Note that by `S`s definition, either `r` is contained in `r'` or vice-versa. *)
        if r'.start > r.start || r'.end_ < r.end_ then
          (* The new match is a worse fit so we keep the current one. *)
          best_matches
        else
          (* We found a better (larger) match! *)
          (* There may be several matches in `best_matches` that are subsumed by `m'`.
           * E.g. we could have found sinks at ranges (1,5) and (6,10), and then
           * we find that there is better sink match at range (1,10). This
           * new larger match subsumes both (1,5) and (6, 10) matches.
           * Thus, before we try adding `m'` to `best_matches`, we need to make sure
           * that there is no other match `m` that is included in `m'`.
           * Otherwise `m'` would be considered a duplicate and it would not
           * be added (e.g., if we try adding the range (1,10) to a set that
           * still contains the range (6,10), given our `compare` function above
           * the (1,10) range will be considered a duplicate), hence the
           * recursive call to `add` here. *)
          add x' (S.remove x best_matches)

  let is_best_match best_matches m' =
    match S.find_opt (Any m') best_matches with
    | None -> true
    | Some (Any m) -> m.range =*= m'.range
end

let is_best_match = Best_matches.is_best_match

(* See NOTE "Best matches" for more context. *)
let best_matches_in_nodes ~sub_matches_of_orig fun_cfg =
  let find_origs_visitor =
    object (_self : 'self)
      inherit [_] IL.iter as super

      method! visit_instr acc i =
        acc := Seq.cons i.iorig !acc;
        super#visit_instr acc i

      method! visit_exp acc e =
        acc := Seq.cons e.eorig !acc;
        super#visit_exp acc e
    end
  in
  (* We traverse the CFG and we check for every sub-match for a taint specification.
   * These are the potential matches that we will pass to the 'Best_matches' data
   * structure, from which it will select the "best matches".
   *
   * For example, this code `sink(if tainted then ok1 else ok2)` is translated
   * in the following IL:
   *
   *     if tainted:
   *       tmp := ok1
   *     else:
   *       tmp := ok2
   *     sink(!tmp)
   *
   * The CFG is essentially:
   *
   *     if tainted -> tmp := ok1 | ->  sink([tmp])
   *               \-> tmp := ok2 |
   *
   * We then check `tainted`, `tmp := ok1`, `ok1`, `tmp := ok2`, `ok2`,
   * `sink(!tmp)`, `sink`,  and`!tmp`, for sub-matches against the spec:
   *
   *     - patterns:
   *       - pattern: sink($X)
   *       - focus-metavariable: $X
   *
   * All of `tainted`, `ok1`, `ok2`, and `!tmp` are potential sub-matches.
   * Yet `!tmp`, which has `if tainted then ok1 else ok2` as 'eorig', and
   * thus has a range that contains them all, is the chosen as the "best"
   * match of the sink spec.
   * *)
  fun_cfg |> IL_helpers.reachable_nodes
  |> Seq.concat_map (fun node ->
         let all_origs : IL.orig Seq.t =
           let origs = ref Seq.empty in
           find_origs_visitor#visit_node origs node;
           !origs
         in
         all_origs |> Seq.concat_map sub_matches_of_orig)
  |> Seq.fold_left (fun s x -> Best_matches.add x s) Best_matches.empty
