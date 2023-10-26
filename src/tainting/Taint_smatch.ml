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
module PM = Pattern_match

type overlap = float

type 'spec t = {
  spec : 'spec;
  spec_id : string;
  spec_pm : PM.t;
  range : Range.t;
  overlap : overlap;
}

let is_exact x = x.overlap > 0.99

type any = Any : 'a t -> any

(* NOTE "Top matches":
 * (See note "Taint-tracking via ranges" in 'Match_tainting_mode.ml' for context.)
 *
 * We use sub-range checks to determine whether a piece of code is a source/sink/etc,
 * and this can lead to unintuitive results. For example,
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
 * So, given a source/sink/etc specification, we check whether the spec matches
 * any of the top-level nodes in the CFG (see 'IL.node_kind'). Given two matches,
 * if one is contained inside the other, then we consider them the same match and
 * we take the larger one as the canonical. We store the canonical matches in a
 * 'Top_matches' data structure, and we use these "top matches" matches as a
 * practical definition of what an "exact match" is.
 *
 * For example, if the sink specification is `echo ...;`, and the code we have is
 * `echo $_GET['foo'];`, then this method will determine that `echo $_GET['foo']`
 * is the best match we can get, and that becomes the definition of exact. Then,
 * when we check whether an expression or instruction is a sink, if its range is a
 * strict sub-range of one of these top matches, we simply disregard it (because it
 * is not an exact match). In our example above the top sink match will be
 * `sink(if tainted then ok1 else ok2)`, so we will disregard `tainted` as a sink
 * because we know there is a better match.
 *)
module Top_matches = struct
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
    |> Common.map (fun (Any m) ->
           m.spec_id ^ ":" ^ Range.content_at_range m.spec_pm.file m.range)
    |> String.concat " ; "

  let rec add (Any m' as x') top_matches =
    (* We check if we have another match for the *same* source/sink/etc spec
     * (i.e., same 'source_id'/'sink_id'/etc), and if so we must keep the
     * best match and drop the other one. *)
    match S.find_opt x' top_matches with
    | None -> S.add x' top_matches
    | Some (Any m as x) ->
        let r = m.range in
        let r' = m'.range in
        (* Note that by `S`s definition, either `r` is contained in `r'` or vice-versa. *)
        if r'.start > r.start || r'.end_ < r.end_ then
          (* The new match is a worse fit so we keep the current one. *)
          top_matches
        else
          (* We found a better (larger) match! *)
          (* There may be several matches in `top_matches` that are subsumed by `m'`.
           * E.g. we could have found sinks at ranges (1,5) and (6,10), and then
           * we find that there is better sink match at range (1,10). This
           * new larger match subsumes both (1,5) and (6, 10) matches.
           * Thus, before we try adding `m'` to `top_matches`, we need to make sure
           * that there is no other match `m` that is included in `m'`.
           * Otherwise `m'` would be considered a duplicate and it would not
           * be added (e.g., if we try adding the range (1,10) to a set that
           * still contains the range (6,10), given our `compare` function above
           * the (1,10) range will be considered a duplicate), hence the
           * recursive call to `add` here. *)
          add x' (S.remove x top_matches)

  let is_best_match top_matches m' =
    match S.find_opt (Any m') top_matches with
    | None -> true
    | Some (Any m) -> m.range =*= m'.range
end

let is_best_match = Top_matches.is_best_match

let top_level_matches_in_nodes ~matches_of_orig flow =
  (* We traverse the CFG and we check whether the top-level expressions match
   * any taint specification. Those that do match a spec are potential
   * "top-level matches". *)
  (* TODO: This handles the common cases that people have more often complained
   * about, it doesn't yet handle e.g. a sink specification like `sink([$SINK, ...])`
   * (with `focus-metavariable: $SINK`), and code like `sink([ok1 if tainted else ok2])`.
   * For that, we would need to visit subexpressions. *)
  flow.CFG.reachable |> CFG.NodeiSet.to_seq
  |> Seq.concat_map (fun ni ->
         let origs_of_args args =
           Seq.map (fun a -> (IL_helpers.exp_of_arg a).eorig) (List.to_seq args)
         in
         let node = flow.CFG.graph#nodes#assoc ni in
         match node.IL.n with
         | NInstr instr ->
             let top_expr_origs : IL.orig Seq.t =
               Seq.cons instr.iorig
                 (match instr.i with
                 | Call (_, c, args) -> Seq.cons c.eorig (origs_of_args args)
                 | New (_, ty, _, args) ->
                     let ty_origs =
                       ty.exps |> List.to_seq |> Seq.map (fun e -> e.IL.eorig)
                     in
                     Seq.append ty_origs (origs_of_args args)
                 | CallSpecial (_, _, args) -> origs_of_args args
                 | Assign (_, e) -> List.to_seq [ e.eorig ]
                 | AssignAnon _
                 | FixmeInstr _ ->
                     Seq.empty)
             in
             top_expr_origs |> Seq.concat_map (fun o -> matches_of_orig o)
         | NCond (_, exp)
         | NReturn (_, exp)
         | NThrow (_, exp) ->
             matches_of_orig exp.eorig
         | Enter
         | Exit
         | TrueNode _
         | FalseNode _
         | Join
         | NGoto _
         | NLambda _
         | NOther _
         | NTodo _ ->
             Seq.empty)
  |> Seq.fold_left (fun s x -> Top_matches.add x s) Top_matches.empty
