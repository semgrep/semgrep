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

let logger = Logging.get_logger [ __MODULE__ ]

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

type source = Rule.taint_source call_trace [@@deriving show]
type sink = Rule.taint_sink call_trace [@@deriving show]

let rec pm_of_trace = function
  | PM (pm, x) -> (pm, x)
  | Call (_, _, trace) -> pm_of_trace trace

let trace_of_pm (pm, x) = PM (pm, x)

let rec _show_call_trace show_thing = function
  | PM (pm, x) ->
      let toks =
        Lazy.force pm.PM.tokens |> List.filter Parse_info.is_origintok
      in
      let s = toks |> Common.map Parse_info.str_of_info |> String.concat " " in
      Printf.sprintf "%s [%s]" s (show_thing x)
  | Call (_e, _, trace) ->
      Printf.sprintf "Call(... %s)" (_show_call_trace show_thing trace)

(*****************************************************************************)
(* Signatures *)
(*****************************************************************************)

type arg_pos = string * int [@@deriving show]

type source_to_sink = {
  source : source;
  tokens : tainted_tokens;
  sink : sink;
  merged_env : Metavariable.bindings;
}
[@@deriving show]

type finding =
  | SrcToSink of source_to_sink
  | SrcToReturn of source * tainted_tokens * G.tok
  | ArgToSink of arg_pos * tainted_tokens * sink
  | ArgToReturn of arg_pos * tainted_tokens * G.tok
[@@deriving show]

type signature = finding list

let _show_source_to_sink { source; sink; _ } =
  Printf.sprintf "%s ~~~> %s"
    (_show_call_trace (fun ts -> ts.Rule.label) source)
    (_show_call_trace (fun _ -> "sink") sink)

let _show_finding = function
  | SrcToSink x -> _show_source_to_sink x
  | SrcToReturn (src, _, _) ->
      Printf.sprintf "return (%s)"
        (_show_call_trace (fun ts -> ts.Rule.label) src)
  | ArgToSink (a, _, _) -> Printf.sprintf "%s ----> sink" (show_arg_pos a)
  | ArgToReturn (a, _, _) -> Printf.sprintf "return (%s)" (show_arg_pos a)

(*****************************************************************************)
(* Taint *)
(*****************************************************************************)

type orig = Src of source | Arg of arg_pos [@@deriving show]
type taint = { orig : orig; tokens : tainted_tokens } [@@deriving show]

let src_of_pm (pm, x) = Src (PM (pm, x))
let taint_of_pm pm = { orig = src_of_pm pm; tokens = [] }

let compare_sources s1 s2 =
  (* Comparing metavariable environments this way is not robust, e.g.:
   * [("$A",e1);("$B",e2)] is not considered equal to [("$B",e2);("$A",e1)].
   * For our purposes, this is OK.
   *)
  let pm1, ts1 = pm_of_trace s1 and pm2, ts2 = pm_of_trace s2 in
  Stdlib.compare
    (pm1.rule_id, pm1.range_loc, pm1.env, ts1.Rule.label)
    (pm2.rule_id, pm2.range_loc, pm2.env, ts2.Rule.label)

let compare_orig orig1 orig2 =
  match (orig1, orig2) with
  | Arg (s, i), Arg (s', j) -> (
      match String.compare s s' with
      | 0 -> Int.compare i j
      | other -> other)
  | Src p, Src q -> compare_sources p q
  | Arg _, Src _ -> -1
  | Src _, Arg _ -> 1

let compare_taint taint1 taint2 =
  (* THINK: Right now we disregard the trace because we just want to keep one
   * potential path. *)
  compare_orig taint1.orig taint2.orig

let _show_taint_label taint =
  match taint.orig with
  | Arg (s, i) -> Printf.sprintf "arg(%s)#%d" s i
  | Src src ->
      let _, ts = pm_of_trace src in
      ts.label

let _show_taint taint =
  let rec depth acc = function
    | PM _ -> acc
    | Call (_, _, x) -> depth (acc + 1) x
  in
  match taint.orig with
  | Src src ->
      let pm, ts = pm_of_trace src in
      let tok1, tok2 = pm.range_loc in
      let r = Range.range_of_token_locations tok1 tok2 in
      Printf.sprintf "(%d,%d)#%s|%d|" r.start r.end_ ts.label (depth 0 src)
  | Arg (s, i) -> Printf.sprintf "arg(%s)#%d" s i

(*****************************************************************************)
(* Taint sets *)
(*****************************************************************************)

let pick_taint taint1 taint2 =
  (* Here we assume that 'compare taint1 taint2 = 0' so we could keep any
     * of them, but we want the one with the shortest trace. *)
  match (taint1.orig, taint1.orig) with
  | Arg _, Arg _ -> taint2
  | Src src1, Src src2 ->
      let call_trace_cmp =
        Int.compare (length_of_call_trace src1) (length_of_call_trace src2)
      in
      if call_trace_cmp < 0 then taint1
      else if call_trace_cmp > 0 then taint2
      else if
        (* same length *)
        List.length taint1.tokens < List.length taint2.tokens
      then taint1
      else taint2
  | Src _, Arg _
  | Arg _, Src _ ->
      logger#error "Taint_set.pick_taint: Ooops, the impossible happened!";
      taint2

module Taint_set = struct
  module Taint_map = Map.Make (struct
    type t = orig

    let compare k1 k2 =
      match (k1, k2) with
      | Arg _, Src _ -> -1
      | Src _, Arg _ -> 1
      | Arg a1, Arg a2 -> Stdlib.compare a1 a2
      | Src s1, Src s2 -> compare_sources s1 s2
  end)

  type t = taint Taint_map.t

  let empty = Taint_map.empty
  let is_empty set = Taint_map.is_empty set

  let equal set1 set2 =
    let eq t1 t2 = compare_taint t1 t2 = 0 in
    Taint_map.equal eq set1 set2

  let add taint set =
    (* We only want to keep one trace per taint source.
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
     * but this has not been investigated in detail.
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
         | Some taint' -> Some (pick_taint taint taint'))

  let union set1 set2 =
    Taint_map.union
      (fun _k taint1 taint2 -> Some (pick_taint taint1 taint2))
      set1 set2

  let singleton taint = add taint empty
  let map f set = Taint_map.map f set
  let iter f set = Taint_map.iter (fun _k -> f) set
  let fold f set acc = Taint_map.fold (fun _k -> f) set acc

  let of_list taints =
    List.fold_left (fun set taint -> add taint set) Taint_map.empty taints

  let to_seq set = set |> Taint_map.to_seq |> Seq.map snd
  let elements set = set |> to_seq |> List.of_seq
end

type taints = Taint_set.t

let taints_of_pms pms = pms |> Common.map taint_of_pm |> Taint_set.of_list

let show_taints taints =
  taints |> Taint_set.elements |> Common.map _show_taint |> String.concat ", "
  |> fun str -> "{ " ^ str ^ " }"
