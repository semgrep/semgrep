(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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

(** Taint results.

    CE's only taint result is a taint source reaching a sink: CE has no
    interprocedural taint, so it never builds a 'ToReturn'/'ToLval'/
    'ToSinkInCall' effect the way Pro does for its interprocedural signature
    inference. *)

open Common
module R = Rule
module T = Taint

module Effect : sig
  type sink_requires =
    | UniReq of R.precondition
    | MultiReq of (Taint.taints * R.precondition) list  (** non-empty *)

  type sink = {
    pm : Core_match.t;
    requires : sink_requires;
    rule_sink : R.taint_sink;
  }
  (** A sink match with its corresponding sink specification (one of the `pattern-sinks`). *)

  type taint_to_sink_item = {
    taint : Taint.taint;
    sink_trace : unit Taint.call_trace;
        (** This trace is from the current calling context of the taint finding,
          to the sink.
          It's a `unit` call_trace because we don't actually need the item at the
          end, and we need to be able to dispatch on the particular variant of taint
          (source or arg).
          *)
  }

  type t =
    | ToSink of {
        taints_with_trace : taint_to_sink_item list;
            (** Taints reaching the sink and the precondition for the sink to apply. *)
        sink : sink;
        merged_env : Metavariable.bindings;
            (** The metavariable environment that results of merging the environment from
            matching the source and the one from matching the sink. *)
      }

  val compare : t -> t -> int
  val show : t -> string

  (* Mainly for debugging *)
  val show_sink : sink -> string
  val show_taint_to_sink_item : taint_to_sink_item -> string
end = struct
  module Taints = Taint.Taint_set

  type sink_requires =
    | UniReq of R.precondition
    | MultiReq of (Taint.taints * R.precondition) list

  type sink = {
    pm : Core_match.t;
    requires : sink_requires;
    rule_sink : R.taint_sink;
  }

  type taint_to_sink_item = { taint : T.taint; sink_trace : unit T.call_trace }

  type t =
    | ToSink of {
        (* These taints were incoming to the sink, under a certain
           REQUIRES expression.
           When we discharge the taint signature, we will produce
           a certain number of findings suitable to how the sink was
           reached.
        *)
        taints_with_trace : taint_to_sink_item list;
        sink : sink;
        merged_env : Metavariable.bindings;
      }

  (*************************************)
  (* Comparison *)
  (*************************************)

  let compare_sink_requires req1 req2 =
    match (req1, req2) with
    | UniReq precond1, UniReq precond2 ->
        R.compare_precondition precond1 precond2
    | MultiReq taints_w_preconds1, MultiReq taints_w_preconds2 ->
        List.compare
          (fun (taints1, precond1) (taints2, precond2) ->
            match R.compare_precondition precond1 precond2 with
            | 0 -> Taints.compare taints1 taints2
            | other -> other)
          taints_w_preconds1 taints_w_preconds2
    | UniReq _, MultiReq _ -> -1
    | MultiReq _, UniReq _ -> 1

  let compare_sink { pm = pm1; requires = requires1; rule_sink = sink1 }
      { pm = pm2; requires = requires2; rule_sink = sink2 } =
    match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
    | 0 -> (
        match T.compare_matches pm1 pm2 with
        | 0 -> compare_sink_requires requires1 requires2
        | other -> other)
    | other -> other

  let compare_taint_to_sink_item { taint = taint1; sink_trace = _ }
      { taint = taint2; sink_trace = _ } =
    T.compare_taint taint1 taint2

  let compare
      (ToSink
         {
           taints_with_trace = taints_w_trace1;
           sink = sink1;
           merged_env = env1;
         })
      (ToSink
         {
           taints_with_trace = taints_w_trace2;
           sink = sink2;
           merged_env = env2;
         }) =
    match compare_sink sink1 sink2 with
    | 0 -> (
        match
          List.compare compare_taint_to_sink_item taints_w_trace1
            taints_w_trace2
        with
        | 0 -> T.compare_metavar_env env1 env2
        | other -> other)
    | other -> other

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

  let show_sink_requires req =
    match req with
    | UniReq precond -> R.show_precondition precond
    | MultiReq taints_w_preconds ->
        taints_w_preconds
        |> List.map (fun (taints, pre) ->
            spf "%s|%s" (T.show_taints taints) (R.show_precondition pre))
        |> String.concat "; "

  let show_sink { rule_sink; requires; pm } =
    let matched_str =
      let tok1, tok2 = pm.range_loc in
      let r = Range.range_of_token_locations tok1 tok2 in
      Range.content_at_range pm.path.internal_path_to_content r
    in
    let matched_line =
      let loc1, _ = pm.range_loc in
      loc1.Loc.pos.line
    in
    spf "(%s at l.%d by %s)[requires:%s]" matched_str matched_line
      rule_sink.R.sink_id
      (show_sink_requires requires)

  let show_taint_to_sink_item { taint; sink_trace } =
    let sink_trace_str =
      match sink_trace with
      | T.PM _ -> ""
      | T.Call _ -> spf "@{%s}" (Taint.show_call_trace [%show: unit] sink_trace)
    in
    Printf.sprintf "%s%s" (T.show_taint taint) sink_trace_str

  let show_taints_with_trace taints =
    Common2.string_of_list show_taint_to_sink_item taints

  let show (ToSink { taints_with_trace; sink; _ }) =
    Common.spf "%s ~~~> %s"
      (show_taints_with_trace taints_with_trace)
      (show_sink sink)
end

module Effects = struct
  include Set.Make (struct
    type t = Effect.t

    let compare effect1 effect2 = Effect.compare effect1 effect2
  end)

  let show s =
    s |> to_seq |> List.of_seq |> List.map Effect.show |> String.concat "; "

  let add_list elts t = List.fold_left (fun set e -> add e set) t elts
  let union_list ts = List.fold_left union empty ts
end
