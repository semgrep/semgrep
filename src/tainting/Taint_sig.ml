open Common
module G = AST_generic
module R = Rule
module T = Taint
module S = Taint_shape
module Taints = T.Taint_set

type sink = { pm : Pattern_match.t; rule_sink : R.taint_sink }

let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
    =
  match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
  | 0 -> T.compare_matches pm1 pm2
  | other -> other

let show_sink { rule_sink; pm } =
  let matched_str =
    let tok1, tok2 = pm.range_loc in
    let r = Range.range_of_token_locations tok1 tok2 in
    Range.content_at_range pm.path.internal_path_to_content r
  in
  let matched_line =
    let loc1, _ = pm.Pattern_match.range_loc in
    loc1.Tok.pos.line
  in
  spf "(%s at l.%d by %s)" matched_str matched_line rule_sink.R.sink_id

type taint_to_sink_item = { taint : T.taint; sink_trace : unit T.call_trace }

let show_taint_to_sink_item { taint; sink_trace } =
  let sink_trace_str =
    match sink_trace with
    | T.PM _ -> ""
    | T.Call _ -> spf "@{%s}" (Taint.show_call_trace [%show: unit] sink_trace)
  in
  Printf.sprintf "%s%s" (T.show_taint taint) sink_trace_str

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

type taints_to_return = {
  data_taints : Taint.taint list;
  data_shape : Taint_shape.shape;
  control_taints : Taint.taint list;
  return_tok : AST_generic.tok;
}

let compare_taints_to_return
    {
      data_taints = data_taints1;
      data_shape = data_shape1;
      control_taints = control_taints1;
      return_tok = _;
    }
    {
      data_taints = data_taints2;
      data_shape = data_shape2;
      control_taints = control_taints2;
      return_tok = _;
    } =
  match List.compare T.compare_taint data_taints1 data_taints2 with
  | 0 -> (
      match S.compare_shape data_shape1 data_shape2 with
      | 0 -> List.compare T.compare_taint control_taints1 control_taints2
      | other -> other)
  | other -> other

type result =
  | ToSink of taints_to_sink
  | ToReturn of taints_to_return
  | ToLval of T.taint list * T.lval (* TODO: CleanArg ? *)

let show_taints_to_sink { taints_with_precondition = taints, _; sink; _ } =
  Common.spf "%s ~~~> %s" (show_taints_and_traces taints) (show_sink sink)

let show_result = function
  | ToSink x -> show_taints_to_sink x
  | ToReturn { data_taints; data_shape; control_taints; return_tok = _ } ->
      Printf.sprintf "return (%s & %s & CTRL:%s)"
        (Common2.string_of_list T.show_taint data_taints)
        (S.show_shape data_shape)
        (Common2.string_of_list T.show_taint control_taints)
  | ToLval (taints, lval) ->
      Printf.sprintf "%s ----> %s"
        (Common2.string_of_list T.show_taint taints)
        (T.show_lval lval)

let compare_result r1 r2 =
  match (r1, r2) with
  | ToSink tts1, ToSink tts2 -> compare_taints_to_sink tts1 tts2
  | ToReturn ttr1, ToReturn ttr2 -> compare_taints_to_return ttr1 ttr2
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

(*****************************************************************************)
(* Instantiation *)
(*****************************************************************************)

(* Try to get an idnetifier from a callee/function expression, to be used in
 * a taint trace. *)
let get_ident_of_callee callee =
  match callee with
  | { IL.e = Fetch f; eorig = _ } -> (
      match f with
      (* Case `f()` *)
      | { base = Var { ident; _ }; rev_offset = []; _ }
      (* Case `obj. ... .m()` *)
      | { base = _; rev_offset = { o = Dot { ident; _ }; _ } :: _; _ } ->
          Some ident
      | __else__ -> None)
  | __else__ -> None

(* TODO: Move to 'Taint' module ? *)
let subst_in_precondition ~inst_var ~inst_ctrl taint =
  let subst taints =
    taints
    |> List.concat_map (fun t ->
           match t.T.orig with
           | Src _ -> [ t ]
           | Var lval -> (
               match inst_var lval with
               | None -> []
               | Some (var_taints, var_shape) ->
                   var_taints
                   (* Taint here is only used to resolve preconditions for taint
                    * variables affected by labels. *)
                   |> Taints.union (S.gather_all_taints_in_shape var_shape)
                   |> Taints.elements)
           | Control -> inst_ctrl () |> Taints.elements)
  in
  T.map_preconditions subst taint

(* TODO: Move to 'Taint' module ? *)
let instantiate_taint ~callee ~inst_var ~inst_ctrl taint =
  let inst_taint_var taint =
    match taint.T.orig with
    | Src _ -> None
    | Var lval -> inst_var lval
    | Control -> Some (inst_ctrl (), S.Bot)
  in
  match taint.T.orig with
  | Src src -> (
      let taint =
        (* Update taint trace.
         *
         * E.g. the call to 'bar' in:
         *
         *     1 def bar():
         *     2     x = taint
         *     3     return x
         *     4
         *     5 def foo():
         *     6     bar()
         *     7     ...
         *
         * would result in this call trace:
         *
         *     Call('bar' @l.6, ["x" @l.2], "taint" @l.2)
         *)
        match callee with
        | { IL.e = _; eorig = SameAs orig_callee } ->
            let call_trace =
              T.Call (orig_callee, taint.tokens, src.call_trace)
            in
            { T.orig = Src { src with call_trace }; tokens = [] }
        | __else__ -> taint
      in
      match subst_in_precondition ~inst_var ~inst_ctrl taint with
      | None ->
          (* substitution made preconditon false, so no taint here! *)
          Taints.empty
      | Some taint -> Taints.singleton taint)
  | Var _
  (* 'Control' is pretty much like a taint variable so we handle both together. *)
  | Control -> (
      match inst_taint_var taint with
      | None -> Taints.empty
      | Some (var_taints, var_shape) ->
          let taints =
            (* TODO: We should be separating taint and shapes to maintain precison,
             * but this isn't possible without first having shape variables. *)
            var_taints |> Taints.union (S.gather_all_taints_in_shape var_shape)
          in
          (* Update taint trace.
           *
           * E.g. the call to 'bar' in:
           *
           *     1 def bar(x):
           *     2     y = x
           *     3     return y
           *     4
           *     5 def foo():
           *     6     t = bar(taint)
           *     7     ...
           *
           * would result in this list of tokens (note that is reversed):
           *
           *     ["t" @l.6; "y" @l.2; "x" @l.1; "bar" @l.6]
           *
           * This is a hack we use because taint traces aren't general enough,
           * this should be represented with a call trace.
           *)
          let extra_tokens =
            (match get_ident_of_callee callee with
            | None -> []
            | Some ident -> [ snd ident ])
            @ List.rev taint.tokens
          in
          taints
          |> Taints.map (fun taint' ->
                 {
                   taint' with
                   tokens = List.rev_append extra_tokens taint'.tokens;
                 }))

let instantiate_taints ~callee ~inst_var ~inst_ctrl taints =
  taints |> Taints.elements
  |> List.fold_left
       (fun acc taint ->
         acc
         |> Taints.union (instantiate_taint ~callee ~inst_var ~inst_ctrl taint))
       Taints.empty

let instantiate_shape ~callee ~inst_var ~inst_ctrl shape =
  S.instantiate_shape (instantiate_taints ~callee ~inst_var ~inst_ctrl) shape
