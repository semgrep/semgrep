(* Iago Abal
 *
 * Copyright (C) 2022 r2c
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

module G = AST_generic
module PM = Pattern_match
module R = Rule
module LabelSet = Set.Make (String)
open Common

let logger = Logging.get_logger [ __MODULE__ ]

(* NOTE "on compare functions":
 *
 * We should be careful about using ppx_deriving. The problem is having "automagic"
 * comparisons that silently change as you change data types. Automagic comparisons
 * are very convenient but sometimes the comparison is not what you want. Initially
 * I just thought that if you carefully considered whether that automagic comparison
 * is OK for each data type then you are fine... So we used `Stdlib.compare` in several
 * places, until one day `Taint.arg` evolved, and we added an `offset` to it, and we
 * forgot to revisit whether `Stdlib.compare` was still a good option (it wasn't)...
 * and this led to duplicates which led to an explosion in the size of taint sets and
 * to big perf problems. I think the only way to get warned about this is to write these
 * comparisons manually, no matter how painful it is, so the typechecker will force
 * you to revisit the comparison functions if the types change. It's safe to use
 * ppx_deriving when all the types are primitive, but if any is not then it can cause
 * these problems.
 *
 * Besides, we now favor the use of pattern matching over record field access, e.g.
 * ```ocaml
 * let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
 *     =
 *   Stdlib.compare
 *     (sink1.Rule.sink_id, pm1.rule_id, pm1.range_loc, pm1.env)
 *     (sink2.Rule.sink_id, pm2.rule_id, pm2.range_loc, pm2.env)
 * ```
 * instead of
 * ```ocaml
 * let compare_sink sink1 sink2
 *      =
 *   Stdlib.compare
 *     (sink1.rule_sink.Rule.sink_id, sink1.pm.rule_id, ...)
 *     (sink2.rule_sink.Rule.sink_id, sink2.pm.rule_id, ...)
 * ```
 * If we pattern-match, and later we add new fields to the record, the typechecker
 * will let us know that we need to revisit `compare_sink`---this does not happen
 * when you use dot accesses.
 *)

(*****************************************************************************)
(* Call traces *)
(*****************************************************************************)

type tainted_tokens = G.tok list [@@deriving show]
(* TODO: Given that the analysis is path-insensitive, the trace should capture
 * all potential paths. So a set of tokens seems more appropriate than a list.
 * TODO: May have to annotate each tainted token with a `call_trace` that explains
 * how it got tainted, so it may help triaging. For example, if we got
 * `x = f(tainted)`, it may be interesting to see how is `f` propagating the
 * taint from its input.
 *)

type 'a call_trace =
  | PM of PM.t * 'a
  | Call of G.expr * tainted_tokens * 'a call_trace
[@@deriving show]

let length_of_call_trace ct =
  let rec loop acc = function
    | PM _ -> acc
    | Call (_, _, ct') -> loop (acc + 1) ct'
  in
  loop 0 ct

type sink = { pm : Pattern_match.t; rule_sink : R.taint_sink } [@@deriving show]

let compare_metavar_env env1 env2 =
  (* It's important that we only return 0 if the two bindings are
     structurally equal. Otherwise, there will be many duplicates.
     We use Stdlib.compare in the other case because deriving
     compare is rather difficult and the specific ordering doesn't
     matter. *)
  if Metavariable.Structural.equal_bindings env1 env2 then 0
  else Stdlib.compare env1 env2

let compare_matches pm1 pm2 =
  match
    String.compare
      (Rule_ID.to_string pm1.PM.rule_id.id)
      (Rule_ID.to_string pm2.PM.rule_id.id)
  with
  | 0 ->
      let compare_range_loc = compare pm1.range_loc pm2.range_loc in
      if compare_range_loc <> 0 then compare_range_loc
      else compare_metavar_env pm1.env pm2.env
  | other -> other

let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
    =
  match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
  | 0 -> compare_matches pm1 pm2
  | other -> other

let rec pm_of_trace = function
  | PM (pm, x) -> (pm, x)
  | Call (_, _, trace) -> pm_of_trace trace

let trace_of_pm (pm, x) = PM (pm, x)

let rec _show_call_trace show_thing = function
  | PM (pm, x) ->
      let toks = Lazy.force pm.PM.tokens |> List.filter Tok.is_origintok in
      let s = toks |> Common.map Tok.content_of_tok |> String.concat " " in
      Printf.sprintf "%s [%s]" s (show_thing x)
  | Call (_e, _, trace) ->
      Printf.sprintf "Call(... %s)" (_show_call_trace show_thing trace)

(*****************************************************************************)
(* Taint arguments ("variables", kind of) *)
(*****************************************************************************)

type arg_pos = { name : string; index : int } [@@deriving show, ord]
type arg_base = BThis | BArg of arg_pos [@@deriving show, ord]
type arg = { base : arg_base; offset : IL.name list } [@@deriving show]

let compare_arg { base = base1; offset = offset1 }
    { base = base2; offset = offset2 } =
  match compare_arg_base base1 base2 with
  | 0 -> List.compare IL_helpers.compare_name offset1 offset2
  | other -> other

let _show_pos { name = s; index = i } = Printf.sprintf "arg(%s@%d)" s i

let _show_base base =
  match base with
  | BThis -> "this"
  | BArg pos -> _show_pos pos

let _show_arg { base; offset = os } =
  _show_base base
  ^
  if os <> [] then
    let os_str =
      os |> Common.map (fun n -> fst n.IL.ident) |> String.concat "."
    in
    "." ^ os_str
  else ""

(*****************************************************************************)
(* Taint *)
(*****************************************************************************)

type source = {
  call_trace : R.taint_source call_trace;
  label : string;
      (* This is needed because we may change the label of a taint,
         from the original source that it came from.
         This happens from propagators which change the label of the taint.
         We don't put it under `taint`, though, because Arg and Control taints
         are polymorphic in label.
      *)
  precondition : (taint list * R.precondition) option;
}
[@@deriving show]

and orig = Src of source | Arg of arg | Control [@@deriving show]
and taint = { orig : orig; tokens : tainted_tokens } [@@deriving show]

let compare_precondition (_ts1, f1) (_ts2, f2) =
  (* We don't consider the "incoming" taints here, assuming both
     preconditions come from otherwise the same source.
     See 'pick_taint' below for details. *)
  R.compare_precondition f1 f2

let compare_source
    { call_trace = call_trace1; label = label1; precondition = precondition1 }
    { call_trace = call_trace2; label = label2; precondition = precondition2 } =
  (* Comparing metavariable environments this way is not robust, e.g.:
   * [("$A",e1);("$B",e2)] is not considered equal to [("$B",e2);("$A",e1)].
   * For our purposes, this is OK.
   *)
  let pm1, ts1 = pm_of_trace call_trace1
  and pm2, ts2 = pm_of_trace call_trace2 in
  match compare_matches pm1 pm2 with
  | 0 -> (
      let l1 = label1 ^ ts1.R.label in
      let l2 = label2 ^ ts2.R.label in
      match String.compare l1 l2 with
      | 0 -> (
          (* It's important that we include preconditions as a distinguishing factor
             between two taints.

             Otherwise, suppose that we had a taint with label A with precondition `false`
             and one with precondition `true`. Obviously, only one actually exists. But
             if we pick the wrong one, we might fallaciously say a taint label finding does
             not actually occur.
          *)
          match (precondition1, precondition2) with
          | None, _
          | _, None ->
              (* 'None' here is the same as 'true', although the `requires` of both taints
                 * may not be the same, in this specific case we consider them "the same",
                 * see 'pick_best_taint'. *)
              0
          | Some pre1, Some pre2 -> compare_precondition pre1 pre2)
      | other -> other)
  | other -> other

let compare_orig orig1 orig2 =
  match (orig1, orig2) with
  | Src p, Src q -> compare_source p q
  | Arg a1, Arg a2 -> compare_arg a1 a2
  | Control, Control -> 0
  | Src _, (Arg _ | Control) -> -1
  | Arg _, Control -> -1
  | Arg _, Src _ -> 1
  | Control, (Arg _ | Src _) -> 1

let compare_taint taint1 taint2 =
  (* THINK: Right now we disregard the trace because we just want to keep one
   * potential path. *)
  compare_orig taint1.orig taint2.orig

let _show_taint_label taint =
  match taint.orig with
  | Src src -> src.label
  | Arg arg -> _show_arg arg
  | Control -> "<control>"

let rec _show_precondition = function
  | R.PLabel str -> str
  | R.PBool b -> Bool.to_string b
  | R.PNot p -> Printf.sprintf "not %s" (_show_precondition p)
  | R.PAnd [ p1; p2 ] ->
      Printf.sprintf "(%s and %s)" (_show_precondition p1)
        (_show_precondition p2)
  | R.PAnd _ -> "(and ...)"
  | R.POr _ -> "(or ...)"

let rec _show_source { call_trace; label; precondition } =
  (* We want to show the actual label, not the originating label.
     This may change, for instance, if we have ever propagated this taint to
     a different label.
  *)
  let rec depth acc = function
    | PM _ -> acc
    | Call (_, _, x) -> depth (acc + 1) x
  in
  let pm, ts = pm_of_trace call_trace in
  let tok1, tok2 = pm.range_loc in
  let r = Range.range_of_token_locations tok1 tok2 in
  let precondition_prefix = _show_taints_with_precondition precondition in
  let str =
    let toks = Lazy.force pm.PM.tokens |> List.filter Tok.is_origintok in
    toks |> Common.map Tok.content_of_tok |> String.concat "_"
  in
  Printf.sprintf "<%s(%d,%d)%s#%s/%s|t:%d>" precondition_prefix r.start r.end_
    str ts.label label (depth 0 call_trace)

and _show_taints_with_precondition precondition =
  match precondition with
  | None -> ""
  | Some (ts, pre) ->
      Common.spf "PRE|%s|if %s|"
        (Common.map _show_taint ts |> String.concat " + ")
        (_show_precondition pre)

and _show_taint taint =
  match taint.orig with
  | Src src -> _show_source src
  | Arg arg -> _show_arg arg
  | Control -> "<control>"

let _show_sink { rule_sink; _ } = rule_sink.R.sink_id

type taint_to_sink_item = { taint : taint; sink_trace : unit call_trace }
[@@deriving show]

let _show_taint_to_sink_item { taint; sink_trace } =
  Printf.sprintf "%s@{%s}" (_show_taint taint)
    (_show_call_trace [%show: unit] sink_trace)

let _show_taints_and_traces taints =
  Common2.string_of_list _show_taint_to_sink_item taints

let compare_taint_to_sink_item { taint = taint1; sink_trace = _ }
    { taint = taint2; sink_trace = _ } =
  compare_taint taint1 taint2

type taints_to_sink = {
  (* These taints were incoming to the sink, under a certain
     REQUIRES expression.
     When we discharge the taint signature, we will produce
     a certain number of findings suitable to how the sink was
     reached.
  *)
  taints_with_precondition : taint_to_sink_item list * R.precondition;
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

let compare_taints_to_sink
    { taints_with_precondition = ttsis1, pre1; sink = sink1; merged_env = env1 }
    { taints_with_precondition = ttsis2, pre2; sink = sink2; merged_env = env2 }
    =
  match compare_sink sink1 sink2 with
  | 0 -> (
      match List.compare compare_taint_to_sink_item ttsis1 ttsis2 with
      | 0 -> (
          match R.compare_precondition pre1 pre2 with
          | 0 -> compare_metavar_env env1 env2
          | other -> other)
      | other -> other)
  | other -> other

type finding =
  | ToSink of taints_to_sink
  | ToReturn of taint list * G.tok
  | ToArg of taint list * arg (* TODO: CleanArg ? *)
[@@deriving show]

let _show_taints_to_sink { taints_with_precondition = taints, _; sink; _ } =
  Common.spf "%s ~~~> %s" (_show_taints_and_traces taints) (_show_sink sink)

let _show_finding = function
  | ToSink x -> _show_taints_to_sink x
  | ToReturn (taints, _) ->
      Printf.sprintf "return (%s)" (Common2.string_of_list _show_taint taints)
  | ToArg (taints, a2) ->
      Printf.sprintf "%s ----> %s"
        (Common2.string_of_list _show_taint taints)
        (_show_arg a2)

let compare_finding fi1 fi2 =
  match (fi1, fi2) with
  | ToSink tts1, ToSink tts2 -> compare_taints_to_sink tts1 tts2
  | ToReturn (ts1, tok1), ToReturn (ts2, tok2) -> (
      match List.compare compare_taint ts1 ts2 with
      | 0 -> Tok.compare tok1 tok2
      | other -> other)
  | ToArg (ts1, a1), ToArg (ts2, a2) -> (
      match List.compare compare_taint ts1 ts2 with
      | 0 -> compare_arg a1 a2
      | other -> other)
  | ToSink _, (ToReturn _ | ToArg _) -> -1
  | ToReturn _, ToArg _ -> -1
  | ToReturn _, ToSink _ -> 1
  | ToArg _, (ToSink _ | ToReturn _) -> 1

module Findings = Set.Make (struct
  type t = finding

  let compare = compare_finding
end)

module Findings_tbl = Hashtbl.Make (struct
  type t = finding

  let equal fi1 fi2 = compare_finding fi1 fi2 =|= 0
  let hash = Hashtbl.hash
end)

type signature = Findings.t

(*****************************************************************************)
(* Taint sets *)
(*****************************************************************************)

module Taint_set = struct
  (* NOTE "Taint sets"
   *
   * Why not just 'Set.Make(...)' using `compare_taint`? If we did this, then
   * given two taints that we consider "the same", we would pick one arbitrarily.
   * While we consider then _essentially_ "the same", there are deatils such as
   * the call trace or the precondition (for labeled taint) that may differ, and
   * we want to pick "the best". This is what this data structure is for, the
   * key functions are 'add' and 'pick_best_taint'.
   *)
  module Taint_map = Map.Make (struct
    type t = orig

    let compare = compare_orig
  end)
  (* TODO: Use a 'Set' instead, 'Map' is error prone, the same problem in 'map'
   * may apply to 'union' as well. Not even clear that using a 'Map' here has any
   * advantages. *)

  type t = taint Taint_map.t

  let empty = Taint_map.empty
  let is_empty set = Taint_map.is_empty set
  let cardinal set = Taint_map.cardinal set

  let equal set1 set2 =
    let eq t1 t2 = compare_taint t1 t2 =|= 0 in
    Taint_map.equal eq set1 set2

  let to_seq set = set |> Taint_map.to_seq |> Seq.map snd
  let elements set = set |> to_seq |> List.of_seq

  let rec add taint set =
    (* If two taints are "the same", we still want to pick "the best", e.g.
     * the one with the shortest trace.
     *
     * This also helps avoiding infinite loops, which can happen when inferring
     * taint sigantures for functions like this:
     *
     *     f(tainted) {
     *         while (true) {
     *             x = g(tainted, f(tainted));
     *             if (true) return x;
     *         }
     *     }
     *
     * Intuitively `f` propagates taint from its input to its output, and with every
     * iteration we have a "new" taint source made by the tainted input passing N
     * times through `f`, and so the fixpoint computation diverges. This is actually
     * rather tricky and removing the `if (true)` or the `g` breaks the infinite loop,
     * but this has not been investigated in detail. (TODO)
     *
     * THINK: We could do more clever things like checking whether a trace is an
     *   extension of another trace and such. This could also be dealt with in the
     *   taint-signatures themselves. But for now this solution is good.
     *
     * coupling: If this changes, make sure to update docs for the `Taint.signature` type.
     *)
    set
    |> Taint_map.update taint.orig (function
         | None -> Some taint
         | Some taint' -> Some (pick_best_taint taint taint'))

  and union set1 set2 =
    Taint_map.union
      (fun _k taint1 taint2 -> Some (pick_best_taint taint1 taint2))
      set1 set2

  and of_list taints =
    List.fold_left (fun set taint -> add taint set) Taint_map.empty taints

  and pick_best_taint taint1 taint2 =
    (* Here we assume that 'compare taint1 taint2 = 0' so we could keep any
       * of them, but we want "the best" one, e.g. the one with the shortest trace. *)
    match (taint1.orig, taint2.orig) with
    | Arg _, Arg _
    | Control, Control ->
        (* Polymorphic taint should only be intraprocedural so the call-trace is irrelevant. *)
        if List.length taint1.tokens < List.length taint2.tokens then taint1
        else taint2
    | Src src1, Src src2 ->
        let precondition =
          (* We don't pick a precondition, but we merge them! *)
          match (src1.precondition, src2.precondition) with
          | None, _
          | _, None ->
              (* We could have 'None, Some_' or 'Some _, None' here. That could mean
               * that there are two sources for the same label with different 'requires',
               * or perhaps the same 'requires' but one was trivially true in the context?
               *
               * In any case, if this happens, we just assume that no precondition
               * is needed to generate the label. *)
              None
          | Some (ts1, p1), Some (ts2, _p2) ->
              (* p1 = p2 given compare_precondition
               * Then we merge both lists of "incoming" taints, this also removes
               * from each individual taint list.
               *
               * Otherwise we end up with taint sets where most of the taints are
               * essentially the same. This is probably due to
               * [see note "Taint-tracking via ranges" in Match_tainting_mode],
               * and not having "Top_sources" [see note "Top sinks" in Dataflow_tainting].
               *)
              let ts = union (of_list ts1) (of_list ts2) |> elements in
              Some (ts, p1)
        in
        let taint1 = { taint1 with orig = Src { src1 with precondition } } in
        let taint2 = { taint2 with orig = Src { src2 with precondition } } in
        let call_trace_cmp =
          Int.compare
            (length_of_call_trace src1.call_trace)
            (length_of_call_trace src2.call_trace)
        in
        if call_trace_cmp < 0 then taint1
        else if call_trace_cmp > 0 then taint2
        else if
          (* same length *)
          List.length taint1.tokens < List.length taint2.tokens
        then taint1
        else taint2
    | (Src _ | Arg _ | Control), (Src _ | Arg _ | Control) ->
        logger#error "Taint_set.pick_taint: Ooops, the impossible happened!";
        taint2

  let diff set1 set2 =
    set1 |> Taint_map.filter (fun k _ -> not (Taint_map.mem k set2))

  let singleton taint = add taint empty

  (* Because `Taint_set` is internally represented with a map, we cannot just
     map the codomain taint, using the internal provided `map` function. We
     want to map the keys too.
     Unfortunately, the keys and values are different types, so it's not as
     straightforward.
     Fortunately, we can exploit a property of the map, which is that the
     `orig` of the domain and codomain should be the same. So it should be fine
     to simply map the codomain taint, and then take its `orig` as the key.
  *)
  let map f set =
    let bindings = Taint_map.bindings set in
    bindings
    (* Here, we assume the invariant that the orig must be
       the same in the domain and codomain.
    *)
    |> Common.map (fun (_, t2) ->
           let new_taint = f t2 in
           (new_taint.orig, new_taint))
    |> List.to_seq |> Taint_map.of_seq

  let iter f set = Taint_map.iter (fun _k -> f) set
  let fold f set acc = Taint_map.fold (fun _k -> f) set acc
  let filter f set = Taint_map.filter (fun _k -> f) set

  let concat_map f set =
    let bindings = Taint_map.bindings set in
    bindings
    |> List.concat_map (fun (_, t2) ->
           f t2 |> elements |> Common.map (fun t -> (t.orig, t)))
    |> List.to_seq |> Taint_map.of_seq
end

type taints = Taint_set.t

let show_taints taints =
  taints |> Taint_set.elements |> Common.map _show_taint |> String.concat ", "
  |> fun str -> "{ " ^ str ^ " }"

(*****************************************************************************)
(* Taint labels *)
(*****************************************************************************)

let labels_in_precondition pre =
  let rec loop = function
    | R.PBool _ -> LabelSet.empty
    | R.PLabel l -> LabelSet.singleton l
    | R.PAnd pres
    | R.POr pres ->
        pres |> Common.map loop |> List.fold_left LabelSet.union LabelSet.empty
    | R.PNot pre -> loop pre
  in
  loop pre

(* We need to be able to solve preconditions even with partial information, so we can
 * simplify away trivially true/false preconditions, and remove taint labels that we
 * know can't be present.
 *
 * Polymorphic taint is tricky, let's say we have:
 *
 *     def foo(y):
 *       x = "taint:A"
 *       sink(x, y)
 *
 * and the 'requires' of `sink` is `A and not B`. We don't know if 'B' could be
 * passed via the arguemnt `y`. Should we report a finding here? If we choose
 * 'ignore_poly_taint' then we will consider that 'B' is not present, and we would
 * solve the precondition as being true (i.e. 'Some true'); otherwise we will consider
 * that 'B' could be present, and the precondition would be considered unsolvable
 * (i.e. 'None').
 *
 * TODO: Perhaps we should "simplify" the precondition _and_ the incoming taints,
 *   so e.g. if we got taint labels 'A' and '?1' (poly label), and the precondition
 *   is `A and not B`, we can reduce that to having taint '?1' and precondition
 *   `not B`.
 *)
let rec solve_precondition ~ignore_poly_taint ~taints pre : bool option =
  let open Common in
  let sure_labels, maybe_labels, has_poly_taint = labels_in_taints taints in
  let rec loop = function
    | R.PBool b -> Some b
    | R.PLabel l ->
        if LabelSet.mem l sure_labels then Some true
        else if
          (not (LabelSet.mem l maybe_labels))
          && ((not has_poly_taint) || ignore_poly_taint)
        then Some false
        else None
    | R.PNot p -> loop p |> Option.map not
    | R.PAnd ps ->
        ps
        |> List.fold_left
             (fun acc p ->
               let* v1 = acc in
               let* v2 = loop p in
               Some (v1 && v2))
             (Some true)
    | R.POr ps ->
        ps
        |> List.fold_left
             (fun acc p ->
               let* v1 = acc in
               let* v2 = loop p in
               Some (v1 || v2))
             (Some false)
  in
  loop pre

and labels_in_taints taints =
  let has_poly_taint = ref false in
  let sure_labels = ref LabelSet.empty in
  let maybe_labels = ref LabelSet.empty in
  taints
  |> Taint_set.iter (fun taint ->
         match taint.orig with
         | Arg _
         | Control ->
             has_poly_taint := true
         | Src { label; precondition = None; _ } ->
             sure_labels := LabelSet.add label !sure_labels
         | Src { label; precondition = Some (incoming, pre); _ } -> (
             match
               solve_precondition ~ignore_poly_taint:false
                 ~taints:(Taint_set.of_list incoming)
                 pre
             with
             | Some true -> sure_labels := LabelSet.add label !sure_labels
             | Some false -> ()
             | None -> maybe_labels := LabelSet.add label !maybe_labels));
  (!sure_labels, !maybe_labels, !has_poly_taint)

let taints_satisfy_requires taints pre =
  (* This is used when deciding whether to report a finding, so if we had:
   *
   *     def foo(y):
   *       x = "taint:A"
   *       sink(x, y)
   *
   * with the 'requires' of `sink` being `A and not B`, we don't know whether
   * 'B' may be coming through the argument `y`. For now, to keep it simple, we
   * choose to ignore the polymorphic taint and report a finding. This is
   * because `not B` is typically used to encode a sanitizer, and it is
   * weird that the actual taint 'A' is present in `foo` but the sanitizer
   * label comes from an argument.
   *
   * TODO: We should check whether `foo` is being called from somewhere else.
   *   If there is no call to `foo` then the safest is to produce the finding,
   *   but if there are calls to `foo` such as `foo("safe")` then we could
   *   wait and not report any finding here.
   *)
  match
    solve_precondition ~ignore_poly_taint:true
      ~taints:(Taint_set.of_list taints) pre
  with
  | Some b -> b
  | None ->
      (* If we set 'ignore_poly_taint' then we expect to be able to solve
       * the precondition! *)
      logger#error "Could not solve taint label precondition";
      false

let filter_relevant_taints requires taints =
  let labels = labels_in_precondition requires in
  (* If the precondition is 'A' we don't care about taint with label 'B' or 'C'. *)
  taints
  |> Taint_set.filter (fun t ->
         match t.orig with
         | Src src -> LabelSet.mem src.label labels
         | Arg _
         | Control ->
             true)

(* Just a straightforward bottom-up map on preconditions. *)
let rec map_preconditions f taint =
  match taint.orig with
  | Arg _
  | Control ->
      Some taint
  | Src { precondition = None; _ } -> Some taint
  | Src ({ precondition = Some (incoming, expr); _ } as src) -> (
      let new_incoming =
        incoming
        |> Common.map_filter (map_preconditions f)
        |> f |> Taint_set.of_list
      in
      let new_incoming = filter_relevant_taints expr new_incoming in
      match
        solve_precondition ~ignore_poly_taint:false ~taints:new_incoming expr
      with
      | Some false -> None
      | Some true ->
          Some { taint with orig = Src { src with precondition = None } }
      | None ->
          let new_incoming = new_incoming |> Taint_set.elements in
          let new_precondition = Some (new_incoming, expr) in
          Some
            {
              taint with
              orig = Src { src with precondition = new_precondition };
            })

(*****************************************************************************)
(* New taints *)
(*****************************************************************************)

let src_of_pm ~incoming (pm, (x : Rule.taint_source)) =
  let relevant_incoming = filter_relevant_taints x.source_requires incoming in
  (* We don't expect to be able to solve preconditions here, but we need to try
   * in order to simplify away the trivial cases. Otherwise if we had e.g. a pattern
   * source like `pattern: $X` that matches tons of things, with label 'B' and a
   * 'requires' like `A`, we could be generating lots of 'B's in places where we know
   * for sure that we don't have any 'A'!
   *)
  match
    solve_precondition ~ignore_poly_taint:false ~taints:relevant_incoming
      x.source_requires
  with
  | Some false -> None
  | Some true ->
      Some
        (Src { call_trace = PM (pm, x); label = x.label; precondition = None })
  | None ->
      let taints_list = Taint_set.elements relevant_incoming in
      let precondition =
        match x.source_requires with
        | Rule.PBool true -> None
        | other -> Some (taints_list, other)
      in
      Some (Src { call_trace = PM (pm, x); label = x.label; precondition })

let taint_of_pm ~incoming pm =
  match src_of_pm ~incoming pm with
  | Some orig -> Some { orig; tokens = [] }
  | None -> None

let taints_of_pms ~incoming pms =
  pms |> Common.map_filter (taint_of_pm ~incoming) |> Taint_set.of_list
