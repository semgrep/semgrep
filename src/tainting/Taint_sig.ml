open Common
module G = AST_generic
module R = Rule
module T = Taint
module S = Taint_shape

type sink = { pm : Pattern_match.t; rule_sink : R.taint_sink }

let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
    =
  match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
  | 0 -> T.compare_matches pm1 pm2
  | other -> other

let show_sink { rule_sink; _ } = rule_sink.R.sink_id

type taint_to_sink_item = { taint : T.taint; sink_trace : unit T.call_trace }

let show_taint_to_sink_item { taint; sink_trace } =
  Printf.sprintf "%s@{%s}" (T.show_taint taint)
    (Taint.show_call_trace [%show: unit] sink_trace)

let show_taints_and_traces taints =
  Common2.string_of_list show_taint_to_sink_item taints

let compare_taint_to_sink_item { taint = taint1; sink_trace = _ }
    { taint = taint2; sink_trace = _ } =
  T.compare_taint taint1 taint2

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

let compare_taints_to_sink
    { taints_with_precondition = ttsis1, pre1; sink = sink1; merged_env = env1 }
    { taints_with_precondition = ttsis2, pre2; sink = sink2; merged_env = env2 }
    =
  match compare_sink sink1 sink2 with
  | 0 -> (
      match List.compare compare_taint_to_sink_item ttsis1 ttsis2 with
      | 0 -> (
          match R.compare_precondition pre1 pre2 with
          | 0 -> T.compare_metavar_env env1 env2
          | other -> other)
      | other -> other)
  | other -> other

type result =
  | ToSink of taints_to_sink
  | ToReturn of T.taint list * S.shape * G.tok
  | ToLval of T.taint list * T.lval (* TODO: CleanArg ? *)

let show_taints_to_sink { taints_with_precondition = taints, _; sink; _ } =
  Common.spf "%s ~~~> %s" (show_taints_and_traces taints) (show_sink sink)

let show_result = function
  | ToSink x -> show_taints_to_sink x
  | ToReturn (taints, shape, _) ->
      Printf.sprintf "return (%s & %s)"
        (Common2.string_of_list T.show_taint taints)
        (S.show_shape shape)
  | ToLval (taints, lval) ->
      Printf.sprintf "%s ----> %s"
        (Common2.string_of_list T.show_taint taints)
        (T.show_lval lval)

let compare_result r1 r2 =
  match (r1, r2) with
  | ToSink tts1, ToSink tts2 -> compare_taints_to_sink tts1 tts2
  | ToReturn (ts1, shape1, _tok1), ToReturn (ts2, shape2, _tok2) -> (
      match List.compare T.compare_taint ts1 ts2 with
      | 0 -> S.compare_shape shape1 shape2
      | other -> other)
  | ToLval (ts1, lv1), ToLval (ts2, lv2) -> (
      match List.compare T.compare_taint ts1 ts2 with
      | 0 -> T.compare_lval lv1 lv2
      | other -> other)
  | ToSink _, (ToReturn _ | ToLval _) -> -1
  | ToReturn _, ToLval _ -> -1
  | ToReturn _, ToSink _ -> 1
  | ToLval _, (ToSink _ | ToReturn _) -> 1

module Results = Set.Make (struct
  type t = result

  let compare = compare_result
end)

module Results_tbl = Hashtbl.Make (struct
  type t = result

  let equal r1 r2 = compare_result r1 r2 =|= 0
  let hash = Hashtbl.hash
end)

type signature = Results.t

let show_signature s =
  s |> Results.to_seq |> List.of_seq |> List_.map show_result
  |> String.concat " + "
