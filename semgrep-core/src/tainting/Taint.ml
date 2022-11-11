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

type tainted_tokens = G.tok list [@@deriving show]
(* TODO: Given that the analysis is path-insensitive, the trace should capture
 * all potential paths. So a set of tokens seems more appropriate than a list.
 * TODO: May have to annotate each tainted token with a `call_trace` that explains
 * how it got tainted.
 *)

type 'a call_trace =
  | PM of PM.t * 'a
  | Call of G.expr * tainted_tokens * 'a call_trace
[@@deriving show]

let rec _show_call_trace show_thing = function
| PM (pm, x) ->
    let toks = Lazy.force pm.PM.tokens |> List.filter Parse_info.is_origintok in
    toks
    |> Common.map Parse_info.str_of_info
    |> String.concat " "
    |> fun s -> Printf.sprintf "%s [%s]" s (show_thing x)
| Call (_e, _, trace) -> Printf.sprintf "Call(... %s)" (_show_call_trace show_thing trace)

type source = Rule.taint_source call_trace [@@deriving show]
type sink = Rule.taint_sink call_trace [@@deriving show]
type arg_pos = string * int [@@deriving show]

type source_to_sink = {
  source : source;
  tokens : tainted_tokens;
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

let _show_source_to_sink { source; sink; _ } =
  Printf.sprintf "%s ----> %s" (_show_call_trace (fun ts -> ts.Rule.label) source) (_show_call_trace (fun _ -> "sink") sink)

type finding =
  | SrcToSink of source_to_sink
  | SrcToReturn of source * tainted_tokens * G.tok
  | ArgToSink of arg_pos * tainted_tokens * sink
  | ArgToReturn of arg_pos * tainted_tokens * G.tok
[@@deriving show]

let _show_finding = function
  | SrcToSink x -> _show_source_to_sink x
  | SrcToReturn(src, _, _) -> Printf.sprintf "return (%s)" (_show_call_trace (fun ts -> ts.Rule.label) src)
  | ArgToSink (a, _, _) -> Printf.sprintf "%s ----> sink" (show_arg_pos a)
  | ArgToReturn (a, _, _) -> Printf.sprintf "return (%s)" (show_arg_pos a)

type signature = finding list
type orig = Src of source | Arg of arg_pos [@@deriving show]
type taint = { orig : orig; tokens : tainted_tokens } [@@deriving show]

let rec pm_of_trace = function
  | PM (pm, x) -> (pm, x)
  | Call (_, _, trace) -> pm_of_trace trace

(* We use a set simply to avoid duplicate findings.
 * THINK: Should we just let them pass here and be filtered out later on? *)
module Taint_set = Set.Make (struct
  type t = taint

  let compare_pm pm1 pm2 =
    (* If the pattern matches are obviously different (have different ranges),
     * we are done. If their ranges are the same, we compare their metavariable
     * environments. This is not robust to reordering metavariable environments,
     * e.g.: [("$A",e1);("$B",e2)] is not equal to [("$B",e2);("$A",e1)]. This
     * is a potential source of duplicate findings, but that is OK.
     *)
    match compare pm1.PM.range_loc pm2.PM.range_loc with
    | 0 -> compare pm1.PM.env pm2.PM.env
    | c -> c

  let compare_dm dm1 dm2 =
    match (pm_of_trace dm1, pm_of_trace dm2) with
    |  (p, x), (q, y) ->
        let pq_cmp = compare_pm p q in
        if pq_cmp <> 0 then pq_cmp else Stdlib.compare x y

  (* TODO: Rely on ppx_deriving.ord ? *)
  let compare_orig t1 t2 =
    match (t1, t2) with
    | Arg (s, i), Arg (s', j) -> (
        match String.compare s s' with
        | 0 -> Int.compare i j
        | other -> other)
    | Src p, Src q -> compare_dm p q
    | Arg _, Src _ -> -1
    | Src _, Arg _ -> 1

  (* TODO: Right now we disregard the trace so we only keep one potential path.
   *       This may have to become a map (orig -> trace) rather than a set, so
   *       that we can merge the traces when merging taint at join points. *)
  let compare t1 t2 = compare_orig t1.orig t2.orig
end)

type taints = Taint_set.t

let trace_of_pm (pm, x) = PM (pm, x)
let src_of_pm (pm, x) = Src (PM (pm, x))
let taint_of_pm pm = { orig = src_of_pm pm; tokens = [] }
let taints_of_pms pms = pms |> Common.map taint_of_pm |> Taint_set.of_list

(* USEFUL FOR DEBUGGING *)
let _show_taint_label taint =
  match taint.orig with
  | Arg (s, i) -> Printf.sprintf "arg(%s)#%d" s i
  | Src src ->
      let _, ts = pm_of_trace src in
      ts.label

let _show_taint taint =
  let rec depth acc = function
  | PM _ -> acc
  | Call(_,_,x) -> depth (acc+1) x
  in
  match taint.orig with
  | Src src ->
      let pm, ts = pm_of_trace src in
      let tok1, tok2 = pm.range_loc in
      let r = Range.range_of_token_locations tok1 tok2 in
      Printf.sprintf "(%d,%d)#%s|%d|" r.start r.end_ ts.label (depth 0 src)
  | Arg (s, i) -> Printf.sprintf "arg(%s)#%d" s i

let show_taints taints =
  taints |> Taint_set.elements
  |> Common.map _show_taint
  |> String.concat ", "
  |> fun str -> "{ " ^ str ^ " }"
