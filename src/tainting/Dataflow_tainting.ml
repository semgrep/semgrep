(* Yoann Padioleau, Iago Abal
 *
 * Copyright (C) 2019-2022 r2c
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
open IL
module G = AST_generic
module F = IL
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module PM = Pattern_match
module R = Rule
module LV = IL_helpers
module T = Taint
module Lval_env = Taint_lval_env
module Taints = T.Taint_set

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tainting dataflow analysis.
 *
 * - This is a rudimentary taint analysis in some ways, but rather complex in
 *   other ways... We don't do alias analysis, and inter-procedural support
 *   (for DeepSemgrep) still doesn't cover some common cases. On the other hand,
 *   almost _anything_ can be a source/sanitizer/sink, we have taint propagators,
 *   etc.
 * - It is a MAY analysis, it finds *potential* bugs (the tainted path could not
 *   be feasible in practice).
 * - Field sensitivity is limited to l-values of the form x.a.b.c, see module
 *   Taint_lval_env and check_tainted_lval for more details. Very coarse grained
 *   otherwise, e.g. `x[i] = tainted` will taint the whole array,
 *
 * old: This was originally in src/analyze, but it now depends on
 *      Pattern_match, so it was moved to src/engine.
 *)

module DataflowX = Dataflow_core.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

module SMap = Map.Make (String)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type var = Var_env.var
type overlap = float

type 'spec tmatch = {
  spec : 'spec;
  spec_pm : PM.t;
  range : Range.t;
  overlap : overlap;
}

type a_propagator = {
  kind : [ `From | `To ];
  prop : R.taint_propagator;
  var : var;
}

type config = {
  filepath : Common.filename;
  rule_id : R.rule_id;
  is_source : G.any -> R.taint_source tmatch list;
  is_propagator : AST_generic.any -> a_propagator tmatch list;
  is_sink : G.any -> R.taint_sink tmatch list;
  is_sanitizer : G.any -> R.taint_sanitizer tmatch list;
      (* NOTE [is_sanitizer]:
       * A sanitizer is more "extreme" than you may expect. When a piece of code is
       * "sanitized" Semgrep will just not check it. For example, something like
       * `sanitize(sink(tainted))` will not yield any finding.
       * *)
  unify_mvars : bool;
  handle_findings : var option -> T.finding list -> Lval_env.t -> unit;
}

type mapping = Lval_env.t D.mapping

(* HACK: Tracks tainted functions intrafile. *)
type fun_env = (var, Taints.t) Hashtbl.t

(* NOTE "Top sinks":
 * (See note "Taint-tracking via ranges" in 'Match_tainting_mode.ml' for context.)
 *
 * We use sub-range checks to determine whether a piece of code is a sink, and this
 * can lead to unintuitive results. For example, `sink(if tainted then ok1 else ok2)`
 * is reported as a tainted sink despite `if tainted then ok1 else ok2` is actually
 * not tainted. The problem is that `tainted` is inside what `sink(...)` matches,
 * and using a sub-range check we end up considering `tainted` itself as a sink.
 *
 * Unfortunately simply checking for exact matches is fragile, because sometimes we
 * are not able to match sinks exactly. For example, if you want `echo ...;` to be a
 * sink in PHP, you cannot omit the ';' as `echo` is not an expression. But in the
 * IL, `echo` is represented as a `Call` instruction and the ';' is not part of the
 * `iorig`.
 *
 * A simple alternative would have been to add an `exact: true` option to request
 * exact matches. This is trivial to implement but it is more a "patch" than a
 * proper fix. It breaks some taint rules, and users would often not understand when
 * and why they need to set (or unset) this option to make their rules work.
 *
 * So, instead, we went for a solution that requires no extra option, it just works,
 * but it is a lot more complex to implement. Given a sink specification, we
 * check whether the sink matches any of the top-level nodes in the CFG (see
 * 'IL.node_kind'). Given two sink matches, if one is contained inside the other,
 * then we consider them the same match and we take the larger one as the canonical.
 * We store the canonical sink matches in a 'Top_sinks' data structure, and we use
 * these "top sinks" matches as a practical definition of what an "exact match" is.
 * For example, if the sink specification is `echo ...;`, and the code we have is
 * `echo $_GET['foo'];`, then this method will determine that `echo $_GET['foo']`
 * is the best match we can get, and that becomes the definition of exact. Then,
 * when we check whether an expression or instruction is a sink, if its range is a
 * strict sub-range of one of these top sinks, we simply disregard it (because it
 * is not an exact match). In our example above the top sink match will be
 * `sink(if tainted then ok1 else ok2)`, so we will disregard `tainted` as a sink
 * because we know there is a better match.
 *)
module Top_sinks = struct
  (* For m, m' in S.t, not (m.range $<=$ m'.range) && not (m'.range $<=$ m.range) *)
  module S = Set.Make (struct
    type t = R.taint_sink tmatch

    (* This compare function is subtle but it allows both `add` and `is_best_match`
     * to be simple and quite efficient. *)
    let compare m1 m2 =
      let sink_id_cmp = String.compare m1.spec.R.sink_id m2.spec.R.sink_id in
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

  let _debug sinks =
    sinks |> S.elements
    |> Common.map (fun m -> Range.content_at_range m.spec_pm.file m.range)
    |> String.concat " ; "

  let rec add m' sinks =
    (* We check if we have another match for the *same* sink specification
     * (i.e., same 'sink_id'), and if so we must keep the best match and drop
     * the other one. *)
    match S.find_opt m' sinks with
    | None -> S.add m' sinks
    | Some m ->
        let r = m.range in
        let r' = m'.range in
        (* Note that by `S`s definition, either `r` is contained in `r'` or vice-versa. *)
        if r'.start > r.start || r'.end_ < r.end_ then
          (* The new match is a worse fit so we keep the current one. *)
          sinks
        else
          (* We found a better (larger) match! *)
          (* There may be several matches in `sinks` that are subsumed by `m'`.
           * E.g. we could have found sinks at ranges (1,5) and (6,10), and then
           * we find that there is better sink match at range (1,10). This
           * new larger match subsumes both (1,5) and (6, 10) matches.
           * Thus, before we try adding `m'` to `sinks`, we need to make sure
           * that there is no other match `m` that is included in `m'`.
           * Otherwise `m'` would be considered a duplicate and it would not
           * be added (e.g., if we try adding the range (1,10) to a set that
           * still contains the range (6,10), given our `compare` function above
           * the (1,10) range will be considered a duplicate), hence the
           * recursive call to `add` here. *)
          add m' (S.remove m sinks)

  let is_best_match sinks m' =
    match S.find_opt m' sinks with
    | None -> true
    | Some m -> m.range =*= m'.range
end

(* THINK: Separate read-only enviroment into a new a "cfg" type? *)
type env = {
  lang : Lang.t;
  options : Rule_options.t;
  config : config;
  fun_name : var option;
  lval_env : Lval_env.t;
  top_sinks : Top_sinks.t;
}

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_function_taint_signature = ref None

(*****************************************************************************)
(* Options *)
(*****************************************************************************)

let propagate_through_functions env =
  (not env.options.taint_assume_safe_functions)
  && not env.options.taint_only_propagate_through_assignments

let propagate_through_indexes env =
  (not env.options.taint_assume_safe_indexes)
  && not env.options.taint_only_propagate_through_assignments

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let+ ) x f =
  match x with
  | None -> []
  | Some x -> f x

let ( let& ) x f =
  match x with
  | None -> Taints.empty
  | Some x -> f x

let _show_fun_exp fun_exp =
  match fun_exp with
  | { e = Fetch { base = Var func; rev_offset = [] }; _ } -> fst func.ident
  | { e = Fetch { base = Var obj; rev_offset = [ { o = Dot method_; _ } ] }; _ }
    ->
      Printf.sprintf "%s.%s" (fst obj.ident) (fst method_.ident)
  | _ -> "<FUNC>"

let status_to_taints = function
  | `None (* no info *)
  | `Clean (* clean, from previous sanitization *)
  | `Sanitized (* clean, because a sanitizer matched "right now" *) ->
      Taints.empty
  | `Tainted taints -> taints

let is_exact x = x.overlap > 0.99

let union_map_taints_and_vars env f xs =
  let taints, lval_env =
    xs
    |> List.fold_left
         (fun (taints1, lval_env) x ->
           let taints2, lval_env = f { env with lval_env } x in
           (Taints.union taints1 taints2, lval_env))
         (Taints.empty, env.lval_env)
  in
  let taints =
    if env.options.taint_only_propagate_through_assignments then Taints.empty
    else taints
  in
  (taints, lval_env)

let str_of_name name = spf "%s:%s" (fst name.ident) (G.SId.show name.sid)
let orig_is_source config orig = config.is_source (any_of_orig orig)
let orig_is_sanitized config orig = config.is_sanitizer (any_of_orig orig)
let orig_is_sink config orig = config.is_sink (any_of_orig orig)

let orig_is_best_sink env orig : R.taint_sink tmatch list =
  orig_is_sink env.config orig
  |> List.filter (Top_sinks.is_best_match env.top_sinks)

let any_of_lval lval =
  match lval with
  | { rev_offset = { oorig; _ } :: _; _ } -> any_of_orig oorig
  | { base = Var var; rev_offset = [] } ->
      let _, tok = var.ident in
      G.Tk tok
  | { base = VarSpecial (_, tok); rev_offset = [] } -> G.Tk tok
  | { base = Mem e; rev_offset = [] } -> any_of_orig e.eorig

let lval_is_source config lval = config.is_source (any_of_lval lval)
let lval_is_sanitized config lval = config.is_sanitizer (any_of_lval lval)
let lval_is_sink config lval = config.is_sink (any_of_lval lval)
let sink_of_match x = { T.pm = x.spec_pm; rule_sink = x.spec }

let taints_of_matches ~incoming xs =
  xs |> Common.map (fun x -> (x.spec_pm, x.spec)) |> T.taints_of_pms ~incoming

let report_findings env findings =
  if findings <> [] then
    env.config.handle_findings env.fun_name findings env.lval_env

let top_level_sinks_in_nodes config flow =
  (* We traverse the CFG and we check whether the top-level expressions match
   * any sink specification. Those that do match a sink are potential
   * "top-level sinks". See NOTE "Top sinks". *)
  (* TODO: This handles the common cases that people have more often complained
   * about, it doesn't yet handle e.g. a sink specification like `sink([$SINK, ...])`
   * (with `focus-metavariable: $SINK`), and code like `sink([ok1 if tainted else ok2])`.
   * For that, we would need to visit subexpressions. *)
  flow.CFG.reachable |> CFG.NodeiSet.to_seq
  |> Stdcompat.Seq.concat_map (fun ni ->
         let origs_of_args args =
           Seq.map (fun a -> (IL_helpers.exp_of_arg a).eorig) (List.to_seq args)
         in
         let node = flow.CFG.graph#nodes#assoc ni in
         match node.n with
         | NInstr instr ->
             let top_expr_origs : orig Seq.t =
               Seq.cons instr.iorig
                 (match instr.i with
                 | Call (_, c, args) -> Seq.cons c.eorig (origs_of_args args)
                 | New (_, ty, _, args) ->
                     let ty_origs =
                       ty.exps |> List.to_seq |> Seq.map (fun e -> e.eorig)
                     in
                     Seq.append ty_origs (origs_of_args args)
                 | CallSpecial (_, _, args) -> origs_of_args args
                 | Assign (_, e) -> List.to_seq [ e.eorig ]
                 | AssignAnon _
                 | FixmeInstr _ ->
                     Seq.empty)
             in
             top_expr_origs
             |> Stdcompat.Seq.concat_map (fun o ->
                    orig_is_sink config o |> List.to_seq)
         | NCond (_, exp)
         | NReturn (_, exp)
         | NThrow (_, exp) ->
             orig_is_sink config exp.eorig |> List.to_seq
         | Enter
         | Exit
         | TrueNode
         | FalseNode
         | Join
         | NGoto _
         | NLambda _
         | NOther _
         | NTodo _ ->
             Seq.empty)
  |> Seq.fold_left (fun s x -> Top_sinks.add x s) Top_sinks.empty

(* Checks whether the sink corresponds has the shape
 *
 *     patterns:
 *     - pattern: <func>(<args>)
 *     - focus-metavariable: $MVAR
 *
 * In which case we know that the function call itself is not the sink, but
 * either the <func> or one (or more) of the <args>.
 *)
let is_func_sink_with_focus taint_sink =
  match taint_sink.Rule.sink_formula with
  | Rule.And
      ( _,
        {
          conjuncts = [ P { pat = Sem ((lazy (E { e = Call _; _ })), _); _ } ];
          focus = [ _focus ];
          _;
        } ) ->
      true
  | __else__ -> false

let unify_mvars_sets env mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        xs >>= fun xs ->
        match List.assoc_opt mvar mvars2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' ->
            if Matching_generic.equal_ast_bound_code env.options mval mval' then
              Some ((mvar, mval) :: xs)
            else None)
      (Some []) mvars1
  in
  let ys =
    List.filter (fun (mvar, _) -> not @@ List.mem_assoc mvar mvars1) mvars2
  in
  Option.map (fun xs -> xs @ ys) xs

let sink_biased_union_mvars source_mvars sink_mvars =
  let source_mvars' =
    List.filter
      (fun (mvar, _) -> not @@ List.mem_assoc mvar sink_mvars)
      source_mvars
  in
  Some (source_mvars' @ sink_mvars)

(* Takes the bindings of multiple taint sources and filters the bindings ($MVAR, MVAL)
 * such that either $MVAR is bound by a single source, or all MVALs bounds to $MVAR
 * can be unified. *)
let merge_source_mvars env bindings =
  let flat_bindings = List.concat bindings in
  let bindings_tbl =
    flat_bindings
    |> Common.map (fun (mvar, _) -> (mvar, None))
    |> List.to_seq |> Hashtbl.of_seq
  in
  flat_bindings
  |> List.iter (fun (mvar, mval) ->
         match Hashtbl.find_opt bindings_tbl mvar with
         | None ->
             (* This should only happen if we've previously found that
                there is a conflict between bound values at `mvar` in
                the sources.
             *)
             ()
         | Some None ->
             (* This is our first time seeing this value, let's just
                add it in.
             *)
             Hashtbl.replace bindings_tbl mvar (Some mval)
         | Some (Some mval') ->
             if
               not
                 (Matching_generic.equal_ast_bound_code env.options mval mval')
             then Hashtbl.remove bindings_tbl mvar);
  (* After this, the only surviving bindings should be those where
     there was no conflict between bindings in different sources.
  *)
  bindings_tbl |> Hashtbl.to_seq |> List.of_seq
  |> Common.map_filter (fun (mvar, mval_opt) ->
         match mval_opt with
         | None ->
             (* This actually shouldn't really be possible, every
                binding should either not exist, or contain a value
                if there's no conflict. But whatever. *)
             None
         | Some mval -> Some (mvar, mval))

(* Merge source's and sink's bound metavariables. *)
let merge_source_sink_mvars env source_mvars sink_mvars =
  if env.config.unify_mvars then
    (* This used to be the default, but it turned out to be confusing even for
     * r2c's security team! Typically you think of `pattern-sources` and
     * `pattern-sinks` as independent. We keep this option mainly for
     * backwards compatibility, it may be removed later on if no real use
     * is found. *)
    unify_mvars_sets env source_mvars sink_mvars
  else
    (* The union of both sets, but taking the sink mvars in case of collision. *)
    sink_biased_union_mvars source_mvars sink_mvars

let partition_mutating_sources sources_matches =
  sources_matches
  |> List.partition (fun (m : R.taint_source tmatch) ->
         m.spec.source_by_side_effect && is_exact m)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let type_of_expr env e =
  match e.eorig with
  | SameAs eorig -> Typing.type_of_expr env.lang eorig |> fst
  | __else__ -> Type.NoType

let type_of_lval env lval =
  match lval with
  | { base = Var x; rev_offset = [] } ->
      Typing.resolved_type_of_id_info env.lang x.id_info
  | { base = _; rev_offset = { o = Dot fld; _ } :: _ } ->
      Typing.resolved_type_of_id_info env.lang fld.id_info
  | __else__ -> Type.NoType

(* We only check this at a few key places to avoid calling `type_of_expr` too
 * many times which could be bad for perf (but haven't properly benchmarked):
 * - assignments
 * - return's
 * - function calls
 * TODO: Ideally we add an `e_type` field and have a type-inference pass to
 *  fill it in, so that every expression has its known type available without
 *  extra cost.
 *)
let drop_taints_if_bool_or_number (options : Rule_options.t) taints ty =
  match ty with
  | Type.(Builtin Bool) when options.taint_assume_safe_booleans -> Taints.empty
  | Type.(Builtin (Int | Float | Number)) when options.taint_assume_safe_numbers
    ->
      Taints.empty
  | __else__ -> taints

(* Calls to 'type_of_expr' seem not to be cheap and even though we tried to limit the
 * number of these calls being made, doing them unconditionally caused a slowdown of
 * ~25% in a ~dozen repos in our stress-test-monorepo. We should just not call
 * 'type_of_expr' unless at least one of the taint_assume_safe_{booleans,numbers} has
 * been set, so rules that do not use these options remain unaffected. Long term we
 * should make type_of_expr less costly.
 *)
let check_type_and_drop_taints_if_bool_or_number env taints type_of_x x =
  if
    (env.options.taint_assume_safe_booleans
   || env.options.taint_assume_safe_numbers)
    && not (Taints.is_empty taints)
  then
    match type_of_x env x with
    | Type.Function (_, return_ty) ->
        drop_taints_if_bool_or_number env.options taints return_ty
    | ty -> drop_taints_if_bool_or_number env.options taints ty
  else taints

(*****************************************************************************)
(* Labels *)
(*****************************************************************************)

(* This function is used to convert some taint thing we're holding
   to one which has been propagated to a new label.
   See [handle_taint_propagators] for more.
*)
let propagate_taint_to_label replace_labels label (taint : T.taint) =
  let new_orig =
    match (taint.orig, replace_labels) with
    (* if there are no replaced labels specified, we will replace
       indiscriminately
    *)
    | Src src, None -> T.Src { src with label }
    | Src src, Some replace_labels when List.mem src.T.label replace_labels ->
        T.Src { src with label }
    | Src src, _ -> Src src
    | Arg arg, _ -> Arg arg
  in
  { taint with orig = new_orig }

(*****************************************************************************)
(* Reporting findings *)
(*****************************************************************************)

(* Potentially produces a finding from incoming taints + call traces to a sink.
   Note that, while this sink has a `requires` and incoming labels,
   we decline to solve this now!
   We will figure out how many actual Semgrep findings are generated
   when this information is used, later.
*)
let findings_of_tainted_sink env taints_with_traces (sink : T.sink) :
    T.finding list =
  match taints_with_traces with
  | [] -> []
  | _ :: _ -> (
      (* We cannot check whether we satisfy the `requires` here.
         This is because this sink may be inside of a function, meaning that
         argument taint can reach it, which can only be instantiated at the
         point where we call the function.
         So we record the `requires` within the taint finding, and evaluate
         the formula later, when we extract the PMs
      *)
      let { T.pm = sink_pm; rule_sink = ts } = sink in
      let taints_and_bindings =
        taints_with_traces
        |> Common.map (fun ({ T.taint; _ } as item) ->
               let bindings =
                 match taint.T.orig with
                 | T.Arg _ -> []
                 | Src source ->
                     let src_pm, _ = T.pm_of_trace source.call_trace in
                     src_pm.PM.env
               in
               let new_taint = { taint with tokens = List.rev taint.tokens } in
               ({ item with taint = new_taint }, bindings))
      in
      (* If `unify_mvars` is set, then we will just do the previous behavior,
         and emit a finding for every single source coming into the sink.
         This will mean we don't regress on `taint_unify_mvars: true` rules.

         This is problematic because there may be many sources, all of which do not
         unify with each other, but which unify with the sink.
         If we did as below and unified them all with each other, we would sometimes
         produce no findings when we should.
      *)
      (* The same will happen if our sink does not have an explicit `requires`.

         This is because our behavior in the second case will remove metavariables
         from the finding, if they conflict in the sources.

         This can lead to a loss of metavariable interpolation in the finding message,
         even for "vanilla" taint mode rules that don't use labels, for instance if
         we had two instances of the source

         foo($X)

         reaching a sink, where in both instances, `$X` is not the same. The current
         behavior is that one of the `$X` bindings is chosen arbitrarily. We will
         try to keep this behavior here.
      *)
      if env.config.unify_mvars || Option.is_none (snd ts.sink_requires) then
        taints_and_bindings
        |> Common.map_filter (fun (t, bindings) ->
               let* merged_env =
                 merge_source_sink_mvars env sink_pm.PM.env bindings
               in
               Some
                 (T.ToSink
                    {
                      taints_with_precondition = ([ t ], R.get_sink_requires ts);
                      sink;
                      merged_env;
                    }))
      else
        match
          taints_and_bindings |> Common.map snd |> merge_source_mvars env
          |> merge_source_sink_mvars env sink_pm.PM.env
        with
        | None -> []
        | Some merged_env ->
            [
              T.ToSink
                {
                  taints_with_precondition =
                    (Common.map fst taints_and_bindings, R.get_sink_requires ts);
                  sink;
                  merged_env;
                };
            ])

(* Produces a finding for every unifiable source-sink pair. *)
let findings_of_tainted_sinks env taints sinks : T.finding list =
  if Taints.is_empty taints then []
  else
    sinks
    |> List.concat_map (fun sink ->
           (* This is where all taint findings start. If it's interproc,
              the call trace will be later augmented into the Call variant,
              but it starts out here as just a PM variant.
           *)
           let taints_with_traces =
             taints |> Taints.elements
             |> Common.map (fun t ->
                    { T.taint = t; sink_trace = T.PM (sink.T.pm, ()) })
           in
           findings_of_tainted_sink env taints_with_traces sink)

let findings_of_tainted_return taints return_tok : T.finding list =
  if Taints.is_empty taints then []
  else
    let taint_list =
      taints |> Taints.elements
      |> Common.map (fun t -> { t with T.tokens = List.rev t.T.tokens })
    in
    [ T.ToReturn (taint_list, return_tok) ]

let check_orig_if_sink env ?filter_sinks orig taints =
  let sinks = orig_is_best_sink env orig in
  let sinks =
    match filter_sinks with
    | None -> sinks
    | Some sink_pred -> sinks |> List.filter sink_pred
  in
  let sinks = sinks |> Common.map sink_of_match in
  let findings = findings_of_tainted_sinks env taints sinks in
  report_findings env findings

(*****************************************************************************)
(* Miscellaneous large functions *)
(*****************************************************************************)

let find_pos_in_actual_args args_taints fparams =
  let pos_args_taints, named_args_taints =
    List.partition_map
      (function
        | Unnamed taints -> Left taints
        | Named (id, taints) -> Right (id, taints))
      args_taints
  in
  let named_arg_map =
    named_args_taints
    |> List.fold_left
         (fun map ((s, _), taint) -> SMap.add s taint map)
         SMap.empty
  in
  let name_to_taints = Hashtbl.create 10 in
  let idx_to_taints = Hashtbl.create 10 in
  (* We first process the named arguments, and then positional arguments.
   *)
  let remaining_params =
    (* Here, we take all the named arguments and remove them from the list of parameters.
     *)
    List.fold_right
      (fun param acc ->
        match param with
        | G.Param { pname = Some (s', _); _ } -> (
            match SMap.find_opt s' named_arg_map with
            | Some taints ->
                (* If this parameter is one of our arguments, insert a mapping and then remove it
                   from the list of remaining parameters.*)
                Hashtbl.add name_to_taints s' taints;
                acc
                (* Otherwise, it has not been consumed, so keep it in the remaining parameters.*)
            | None -> param :: acc (* Same as above. *))
        | __else__ -> param :: acc)
      (Tok.unbracket fparams) []
  in
  let _ =
    (* We then process all of the positional arguments in order of the remaining parameters.
     *)
    pos_args_taints
    |> List.fold_left
         (fun (i, remaining_params) taints ->
           match remaining_params with
           | [] ->
               logger#error
                 "More args to function than there are positional arguments in \
                  function signature";
               (i + 1, [])
           | _ :: rest ->
               Hashtbl.add idx_to_taints i taints;
               (i + 1, rest))
         (0, remaining_params)
  in
  fun (s, i) ->
    let taint_opt =
      match
        (Hashtbl.find_opt name_to_taints s, Hashtbl.find_opt idx_to_taints i)
      with
      | Some taints, _ -> Some taints
      | _, Some taints -> Some taints
      | __else__ -> None
    in
    if Option.is_none taint_opt then
      logger#error
        "cannot match taint variable with function arguments (%i: %s)" i s;
    taint_opt

let fix_poly_taint_with_field env lval st =
  if env.lang =*= Lang.Java || Lang.is_js env.lang then
    match lval.rev_offset with
    | { o = Dot n; _ } :: _ -> (
        match st with
        | `Sanitized
        | `Clean
        | `None ->
            st
        | `Tainted taints -> (
            match !(n.id_info.id_type) with
            | Some { t = TyFun _; _ } ->
                (* We have an l-value like `o.f` where `f` has a function type,
                 * so it's a method call, we do nothing here. *)
                st
            | __else__ ->
                (* Not a method call (to the best of our knowledge) or
                 * an unresolved Java `getX` method. *)
                let taints' =
                  taints
                  |> Taints.map (fun taint ->
                         match taint.orig with
                         | Arg ({ offset; _ } as arg)
                           when (* If the offset we are trying to take is already in the
                                   list of offsets, don't append it! This is so we don't
                                   never-endingly loop the dataflow and make it think the
                                   Arg taint is never-endingly changing.

                                   For instance, this code example would previously loop,
                                   if `x` started with an `Arg` taint:
                                   while (true) { x = x.getX(); }
                                *)
                                (not (List.mem n offset))
                                && (* For perf reasons we don't allow offsets to get too long.
                                    * Otherwise in a long chain of function calls where each
                                    * function adds some offset, we could end up a very large
                                    * amount of polymorphic taint.
                                    * This actually happened with rule
                                    * semgrep.perf.rules.express-fs-filename from the Pro
                                    * benchmarks, and file
                                    * WebGoat/src/main/resources/webgoat/static/js/libs/ace.js.
                                    *
                                    * TODO: This is way less likely to happen if we had better
                                    *   type info and we used to remove taint, e.g. if Boolean
                                    *   and integer expressions didn't propagate taint. *)
                                List.length offset
                                < Limits_semgrep.taint_MAX_LVAL_OFFSET ->
                             let arg' =
                               { arg with offset = arg.offset @ [ n ] }
                             in
                             { taint with orig = Arg arg' }
                         | Arg _
                         | Src _ ->
                             taint)
                in
                `Tainted taints'))
    | _ :: _
    | [] ->
        st
  else st

(*****************************************************************************)
(* Tainted *)
(*****************************************************************************)

let sanitize_lval_by_side_effect lval_env sanitizer_pms lval =
  let lval_is_now_safe =
    (* If the l-value is an exact match (overlap > 0.99) for a sanitizer
     * annotation, then we infer that the l-value itself has been updated
     * (presumably by side-effect) and is no longer tainted. We will update
     * the environment (i.e., `lval_env') accordingly. *)
    List.exists
      (fun (m : R.taint_sanitizer tmatch) ->
        m.spec.sanitizer_by_side_effect && is_exact m)
      sanitizer_pms
  in
  if lval_is_now_safe then Lval_env.clean lval_env lval else lval_env

(* Check if an expression is sanitized, if so returns `Some' and otherise `None'.
   If the expression is of the form `x.a.b.c` then we try to sanitize it by
   side-effect, in which case this function will return a new lval_env. *)
let exp_is_sanitized env exp =
  match orig_is_sanitized env.config exp.eorig with
  (* See NOTE [is_sanitizer] *)
  | [] -> None
  | sanitizer_pms -> (
      match exp.e with
      | Fetch lval ->
          Some (sanitize_lval_by_side_effect env.lval_env sanitizer_pms lval)
      | __else__ -> Some env.lval_env)

(* Checks if `thing' is a propagator `from' and if so propagates `taints' through it.
   Checks if `thing` is a propagator `'to' and if so fetches any taints that had been
   previously propagated. Returns *only* the newly propagated taint. *)
let handle_taint_propagators env thing taints =
  (* We propagate taints via an auxiliary variable (the propagator id). This is
   * simple but it has limitations, we can only propagate "forward" and, within
   * an instruction node, we can only propagate in the order in which we visit
   * the subexpressions. E.g. in `x.f(y,z)` we can propagate taint from `y` or
   * `z` to `x`, or from `y` to `z`; but we cannot propagate taint from `x` to
   * `y` or `z`, or from `z` to `y`. *)
  let lval_env = env.lval_env in
  let propagators =
    let any =
      match thing with
      | `Lval lval -> any_of_lval lval
      | `Exp exp -> any_of_orig exp.eorig
      | `Ins ins -> any_of_orig ins.iorig
    in
    env.config.is_propagator any
  in
  let propagate_froms, propagate_tos =
    List.partition (fun p -> p.spec.kind =*= `From) propagators
  in
  let lval_env =
    (* `thing` is the source (the "from") of propagation, we add its taints to
     * the environment. *)
    List.fold_left
      (fun lval_env prop ->
        (* Only propagate if the current set of taint labels can satisfy the
           propagator's requires precondition.
        *)
        (* TODO(brandon): Interprocedural propagator labels
           This is trickier than I thought. You have to augment the Arg taints
           with preconditions as well, and allow conjunction, because when you
           replace an Arg taint with a precondition, all the produced taints
           inherit the precondition. There's not an easy way to express this
           in the type right now.

           More concretely, the existence of labeled propagators means that
           preconditions can be attached to arbitrary taint. This is because
           if we have a taint that is being propagated with a `requires`, then
           that taint now has a precondition on that `requires` being true. This
           taint might also be an `Arg` taint, meaning that `Arg` taints can
           have preconditions.

           This is more than just a simple type-level change because when `Arg`s
           have preconditions, what happens for substitution? Say I want to
           replace an `Arg x` taint with [t], that is, a single taint. Well,
           that taint `t` might itself have a precondition. That means that we
           now have a taint which is `t`, substituted for `Arg x`, but also
           inheriting `Arg x`'s precondition. Our type for preconditions doesn't
           allow arbitrary conjunction of preconditions like that, so this is
           more pervasive of a change.

           I'll come back to this later.
        *)
        match
          T.solve_precondition ~taints prop.spec.prop.propagator_requires
        with
        | Some true ->
            (* If we have an output label, change the incoming taints to be
               of the new label.
               Otherwise, keep them the same.
            *)
            let new_taints =
              match prop.spec.prop.propagator_label with
              | None -> taints
              | Some label ->
                  Taints.map
                    (propagate_taint_to_label
                       prop.spec.prop.propagator_replace_labels label)
                    taints
            in
            Lval_env.propagate_to prop.spec.var new_taints lval_env
        | Some false
        | None ->
            lval_env)
      lval_env propagate_froms
  in
  let taints_propagated, lval_env =
    (* `thing` is the destination (the "to") of propagation. we collect all the
     * incoming taints by looking for the propagator ids in the environment. *)
    List.fold_left
      (fun (taints_in_acc, lval_env) prop ->
        let taints_from_prop =
          match Lval_env.propagate_from prop.spec.var lval_env with
          | None -> Taints.empty
          | Some taints -> taints
        in
        let lval_env =
          if prop.spec.prop.propagator_by_side_effect then
            match thing with
            (* If `thing` is an l-value of the form `x.a.b.c`, then taint can be propagated
               * by side-effect. A pattern-propagator may use this to e.g. propagate taint
               * from `x` to `y` in `f(x,y)`, so that subsequent uses of `y` are tainted
               * if `x` was previously tainted. *)
            | `Lval lval -> Lval_env.add lval_env lval taints_from_prop
            | `Exp _
            | `Ins _ ->
                lval_env
          else lval_env
        in
        (Taints.union taints_in_acc taints_from_prop, lval_env))
      (Taints.empty, lval_env) propagate_tos
  in
  (taints_propagated, lval_env)

let find_lval_taint_sources env incoming_taints lval =
  let source_pms = lval_is_source env.config lval in
  let mut_source_pms, reg_source_pms =
    (* If the lvalue is an exact match (overlap > 0.99) for a source
       * annotation, then we infer that the lvalue itself is now tainted
       * (presumably by side-effect) and we will update the `lval_env`
       * accordingly. Otherwise the lvalue belongs to a piece of code that
       * is a source of taint, but it is not tainted on its own. *)
    partition_mutating_sources source_pms
  in
  let taints_sources_reg =
    reg_source_pms |> taints_of_matches ~incoming:incoming_taints
  and taints_sources_mut =
    mut_source_pms |> taints_of_matches ~incoming:incoming_taints
  in
  let lval_env = Lval_env.add env.lval_env lval taints_sources_mut in
  (Taints.union taints_sources_reg taints_sources_mut, lval_env)

let rec check_tainted_lval env (lval : IL.lval) : Taints.t * Lval_env.t =
  let new_taints, lval_in_env, lval_env = check_tainted_lval_aux env lval in
  let taints_from_env = status_to_taints lval_in_env in
  let taints = Taints.union new_taints taints_from_env in
  let taints =
    check_type_and_drop_taints_if_bool_or_number env taints type_of_lval lval
  in
  let sinks =
    lval_is_sink env.config lval
    |> List.filter (Top_sinks.is_best_match env.top_sinks)
    |> Common.map sink_of_match
  in
  let findings = findings_of_tainted_sinks { env with lval_env } taints sinks in
  report_findings { env with lval_env } findings;
  (taints, lval_env)

and propagate_taint_via_unresolved_java_getters_and_setters env e args
    all_args_taints =
  match e with
  | {
   e =
     Fetch
       ({
          base = Var _obj;
          rev_offset =
            [ { o = Dot { IL.ident = method_str, method_tok; id_info; _ }; _ } ];
        } as lval);
   _;
  }
    when env.lang =*= Lang.Java
         && Option.is_none !(id_info.id_resolved)
         && String.length method_str > 3 -> (
      (* e.g. getFooBar/setFooBar -> fooBar *)
      let prop_str =
        String.uncapitalize_ascii (Str.string_after method_str 3)
      in
      (* TODO: Fix this with naming info coming from Pro via some hook. *)
      let prop_name =
        {
          ident = (prop_str, method_tok);
          sid = G.SId.unsafe_default;
          id_info = G.empty_id_info ();
        }
      in
      let prop_lval =
        { lval with rev_offset = [ { o = Dot prop_name; oorig = NoOrig } ] }
      in
      match args with
      | [] when Stdcompat.String.(starts_with ~prefix:"get" method_str) ->
          check_tainted_lval env prop_lval
      | [ _ ] when Stdcompat.String.starts_with ~prefix:"set" method_str ->
          if not (Taints.is_empty all_args_taints) then
            (Taints.empty, Lval_env.add env.lval_env prop_lval all_args_taints)
          else (Taints.empty, env.lval_env)
      | __else__ -> (Taints.empty, env.lval_env))
  | __else__ -> (Taints.empty, env.lval_env)

and check_tainted_lval_aux env (lval : IL.lval) :
    Taints.t
    (* `Sanitized means that the lval matched a sanitizer "right now", whereas
     * `Clean means that the lval has been _previously_ sanitized. They are
     * handled differently, see NOTE [lval/sanitized]. *)
    * [ `Sanitized | `Clean | `None | `Tainted of Taints.t ]
    * Lval_env.t =
  (* Recursively checks an l-value bottom-up.
   *
   *  This check needs to combine matches from pattern-{sources,sanitizers,sinks}
   *  with the info we have stored in `env.lval_env`. This can be subtle, see
   *  comments below.
   *)
  match lval_is_sanitized env.config lval with
  (* See NOTE [is_sanitizer] *)
  (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
  | _ :: _ as sanitizer_pms ->
      (* NOTE [lval/sanitized]:
       *  If lval is sanitized, then we will "bubble up" the `Sanitized status, so
       *  any taint recorded in lval_env for any extension of lval will be discarded.
       *
       *  So, if we are checking `x.a.b.c` and `x.a` is sanitized then any extension
       *  of `x.a` is considered sanitized as well, and we do look for taint info in
       *  the environment.
       *
       *  *IF* sanitization is side-effectful then any taint info will be removed
       *  from lval_env by sanitize_lval, but that is not guaranteed.
       *)
      let lval_env =
        sanitize_lval_by_side_effect env.lval_env sanitizer_pms lval
      in
      (Taints.empty, `Sanitized, lval_env)
  | [] ->
      (* Recursive call, check sub-lvalues first.
       *
       * It needs to be done bottom-up because any sub-lvalue can be a source and a
       * sink by itself, even if an extension of lval is not. For example, given
       * `x.a.b`, this lvalue may be considered sanitized, but at the same time `x.a`
       * could be tainted and considered a sink in some context. We cannot just check
       * `x.a.b` and forget about the sub-lvalues.
       *)
      let sub_new_taints, sub_in_env, lval_env =
        match lval with
        | { base; rev_offset = [] } ->
            (* Base case, no offset. *)
            let taints, lval_env = check_tainted_lval_base env base in
            (taints, `None, lval_env)
        | { base = _; rev_offset = _ :: rev_offset' } ->
            (* Recursive case, given `x.a.b` we must first check `x.a`. *)
            check_tainted_lval_aux env { lval with rev_offset = rev_offset' }
      in
      (* Check the status of lval in the environemnt. *)
      let lval_in_env =
        match sub_in_env with
        | `Sanitized ->
            (* See NOTE [lval/sanitized] *)
            `Sanitized
        | (`Clean | `None | `Tainted _) as st -> (
            match Lval_env.dumb_find lval_env lval with
            | (`Clean | `Tainted _) as st' -> st'
            | `None ->
                (* HACK(field-sensitivity): If we encounter `obj.x` and `obj` has
                   * polymorphic taint, and we know nothing specific about `obj.x`, then
                   * we add the same offset `.x` to the polymorphic taint coming from `obj`.
                   * (See also 'propagate_taint_via_unresolved_java_getters_and_setters'.)
                   *
                   * For example, given `function foo(o) { sink(o.x); }`, and being '0 the
                   * polymorphic taint of `o`, this allows us to record that what goes into
                   * the sink is '0.x (and not just '0). So if later we encounter `foo(obj)`
                   * where `obj.y` is tainted but `obj.x` is not tainted, we will not
                   * produce a finding.
                *)
                fix_poly_taint_with_field env lval st)
      in
      let taints_from_env = status_to_taints lval_in_env in
      (* Find taint sources matching lval. *)
      let current_taints = Taints.union sub_new_taints taints_from_env in
      let taints_from_sources, lval_env =
        find_lval_taint_sources { env with lval_env } current_taints lval
      in
      (* Check sub-expressions in the offset. *)
      let taints_from_offset, lval_env =
        match lval.rev_offset with
        | [] -> (Taints.empty, lval_env)
        | offset :: _ -> check_tainted_lval_offset { env with lval_env } offset
      in
      (* Check taint propagators. *)
      let taints_incoming (* TODO: find a better name *) =
        if env.options.taint_only_propagate_through_assignments then
          taints_from_sources
        else
          sub_new_taints
          |> Taints.union taints_from_sources
          |> Taints.union taints_from_offset
      in
      let taints_propagated, lval_env =
        handle_taint_propagators { env with lval_env } (`Lval lval)
          (taints_incoming |> Taints.union taints_from_env)
      in
      let new_taints = taints_incoming |> Taints.union taints_propagated in
      let sinks =
        lval_is_sink env.config lval
        (* For sub-lvals we require sinks to be exact matches. Why? Let's say
           * we have `sink(x.a)` and `x' is tainted but `x.a` is clean...
           * with the normal subset semantics for sinks we would consider `x'
           * itself to be a sink, and we would report a finding!
        *)
        |> List.filter is_exact
        |> Common.map sink_of_match
      in
      let all_taints = Taints.union taints_from_env new_taints in
      let findings =
        findings_of_tainted_sinks { env with lval_env } all_taints sinks
      in
      report_findings { env with lval_env } findings;
      (new_taints, lval_in_env, lval_env)

and check_tainted_lval_base env base =
  match base with
  | Var _
  | VarSpecial _ ->
      (Taints.empty, env.lval_env)
  | Mem e ->
      let taints, lval_env = check_tainted_expr env e in
      (taints, lval_env)

and check_tainted_lval_offset env offset =
  match offset.o with
  | Dot _n ->
      (* THINK: Allow fields to be taint sources, sanitizers, or sinks ??? *)
      (Taints.empty, env.lval_env)
  | Index e ->
      let taints, lval_env = check_tainted_expr env e in
      let taints =
        if propagate_through_indexes env then taints
        else (* Taints from the index should be ignored. *)
          Taints.empty
      in
      (taints, lval_env)

(* Test whether an expression is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
and check_tainted_expr env exp : Taints.t * Lval_env.t =
  let check env = check_tainted_expr env in
  let check_subexpr exp =
    match exp.e with
    | Fetch lval -> check_tainted_lval env lval
    | FixmeExp (_, _, Some e) -> check env e
    | Literal _
    | FixmeExp (_, _, None) ->
        (Taints.empty, env.lval_env)
    | Composite (_, (_, es, _)) -> union_map_taints_and_vars env check es
    | Operator ((op, _), es) ->
        let args_taints, lval_env =
          es
          |> Common.map IL_helpers.exp_of_arg
          |> union_map_taints_and_vars env check
        in
        let op_taints =
          match op with
          | G.Eq
          | G.NotEq
          | G.PhysEq
          | G.NotPhysEq
          | G.Lt
          | G.LtE
          | G.Gt
          | G.GtE
          | G.Cmp
          | G.RegexpMatch
          | G.NotMatch
          | G.In
          | G.NotIn
          | G.Is
          | G.NotIs ->
              if env.options.taint_assume_safe_comparisons then Taints.empty
              else args_taints
          | G.And
          | G.Or
          | G.Xor
          | G.Not
          | G.LSL
          | G.LSR
          | G.ASR
          | G.BitOr
          | G.BitXor
          | G.BitAnd
          | G.BitNot
          | G.BitClear
          | G.Plus
          | G.Minus
          | G.Mult
          | G.Div
          | G.Mod
          | G.Pow
          | G.FloorDiv
          | G.MatMult
          | G.Concat
          | G.Append
          | G.Range
          | G.RangeInclusive
          | G.NotNullPostfix
          | G.Length
          | G.Elvis
          | G.Nullish
          | G.Background
          | G.Pipe ->
              args_taints
        in
        (op_taints, lval_env)
    | Record fields ->
        union_map_taints_and_vars env
          (fun env -> function
            | Field (_, e)
            | Spread e ->
                check env e)
          fields
    | Cast (_, e) -> check env e
  in
  match exp_is_sanitized env exp with
  | Some lval_env ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, lval_env)
  | None ->
      let taints_exp, lval_env = check_subexpr exp in
      let taints_sources =
        orig_is_source env.config exp.eorig
        |> taints_of_matches ~incoming:taints_exp
      in
      let taints = Taints.union taints_exp taints_sources in
      let taints_propagated, var_env =
        handle_taint_propagators { env with lval_env } (`Exp exp) taints
      in
      let taints = Taints.union taints taints_propagated in
      check_orig_if_sink env exp.eorig taints;
      (taints, var_env)

let check_tainted_var env (var : IL.name) : Taints.t * Lval_env.t =
  check_tainted_lval env (LV.lval_of_var var)

(* Given a function/method call 'fun_exp'('args_exps'), and an argument
 * spec 'sig_arg' from the taint signature of the called function/method,
 * determine what lvalue corresponds to 'sig_arg'.
 *
 * In the simplest case this just obtains the actual argument:
 * E.g. `lval_of_sig_arg f [x;y;z] [a;b;c] (x,0) = a`
 *
 * The 'sig_arg' may refer to `this` and also have an offset:
 * E.g. `lval_of_sig_arg o.f [] [] (this,-1).x = o.x`
 *)
let lval_of_sig_arg fun_exp fparams args_exps (sig_arg : T.arg) =
  let os =
    sig_arg.offset |> Common.map (fun x -> { o = Dot x; oorig = NoOrig })
  in
  let* lval, obj =
    match sig_arg.pos with
    | "<this>", -1 -> (
        match fun_exp with
        | {
         e = Fetch { base = Var obj; rev_offset = [ { o = Dot _method; _ } ] };
         _;
        } ->
            Some ({ base = Var obj; rev_offset = List.rev os }, obj)
        | __else__ -> None)
    | pos -> (
        let* arg_exp = find_pos_in_actual_args args_exps fparams pos in
        match (arg_exp.e, sig_arg.offset) with
        | Fetch ({ base = Var obj; _ } as arg_lval), _ ->
            let lval =
              {
                arg_lval with
                rev_offset = List.rev_append os arg_lval.rev_offset;
              }
            in
            Some (lval, obj)
        | Record fields, [ o ] -> (
            (* JS: The argument of a function call may be a record expression such as
             * `{x="tainted"l, y="safe"}`, if 'sig_arg' refers to the `x` field then
             * we want to resolve it to `"tainted"`. *)
            match
              fields
              |> List.find_opt (function
                   (* The 'o' is the offset that 'sig_arg' is referring to, here
                    * we look for a `fld=lval` field in the record object such that
                    * 'fld' has the same name as 'o'. *)
                   | Field (fld, _) -> fst fld = fst o.ident
                   | Spread _ -> false)
            with
            | Some (Field (_, { e = Fetch ({ base = Var obj; _ } as lval); _ }))
              ->
                (* Actual argument is of the form {..., fld=lval, ...} and the offset is 'fld',
                 * we return 'lval'. *)
                Some (lval, obj)
            | Some _
            | None ->
                None)
        | __else__ -> None)
  in
  Some (lval, obj)

(* What is the taint denoted by 'sig_arg' ? *)
let taints_of_sig_arg env fparams fun_exp args_exps args_taints
    (sig_arg : T.arg) =
  match sig_arg.offset with
  | [] when snd sig_arg.pos >= 0 (* not `this`/`self` *) ->
      find_pos_in_actual_args args_taints fparams sig_arg.pos
  | __else__ ->
      (* We want to know what's the taint carried by 'arg_exp.x1. ... .xN'. *)
      let* lval, _obj = lval_of_sig_arg fun_exp fparams args_exps sig_arg in
      let arg_taints = check_tainted_lval env lval |> fst in
      Some arg_taints

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any findings that occur within the function due to taints being
      input into the function body, from the calling context?
*)
let check_function_signature env fun_exp args args_taints =
  match (!hook_function_taint_signature, fun_exp) with
  | Some hook, { e = Fetch f; eorig = SameAs eorig } ->
      let* fparams, fun_sig = hook env.config eorig in
      (* This function simply produces the corresponding taints to the
          given argument, within the body of the function.
      *)
      (* Our first pass will be to substitute the args for taints.
         We can't do this indiscriminately at the beginning, because
         we might need to use some of the information of the pre-substitution
         taints and the post-substitution taints, for instance the tokens.

         So we will isolate this as a specific step to be applied as necessary.
      *)
      let arg_to_taints arg =
        taints_of_sig_arg env fparams fun_exp args args_taints arg
      in
      let subst_in_precondition taint =
        let subst taints =
          taints
          |> List.concat_map (fun t ->
                 match t.T.orig with
                 | Src _ -> [ t ]
                 | Arg arg ->
                     let+ arg_taints = arg_to_taints arg in
                     Taints.elements arg_taints)
        in
        T.map_preconditions subst taint
      in
      let process_sig :
          T.finding ->
          [ `Return of Taints.t
          | (* ^ Taints flowing through the function's output *)
            `UpdateEnv of
            lval * Taints.t
            (* ^ Taints flowing through function's arguments (or the callee object) by side-effect *)
          ]
          list = function
        | T.ToReturn (taints, _return_tok) ->
            taints
            |> Common.map_filter (fun t ->
                   match t.T.orig with
                   | Src src ->
                       let call_trace =
                         T.Call (eorig, t.tokens, src.call_trace)
                       in
                       Some
                         (`Return
                           (Taints.singleton
                              ({
                                 orig = Src { src with call_trace };
                                 tokens = [];
                               }
                              |> subst_in_precondition)))
                   | Arg arg ->
                       let* arg_taints = arg_to_taints arg in
                       (* Get the token of the function *)
                       let* ident =
                         match f with
                         (* Case `$F()` *)
                         | { base = Var { ident; _ }; rev_offset = []; _ }
                         (* Case `$X. ... .$F()` *)
                         | {
                             base = _;
                             rev_offset = { o = Dot { ident; _ }; _ } :: _;
                             _;
                           } ->
                             Some ident
                         | __else__ -> None
                       in
                       Some
                         (`Return
                           (arg_taints
                           |> Taints.map (fun taint ->
                                  let tokens =
                                    List.rev_append t.tokens
                                      (snd ident :: taint.tokens)
                                  in
                                  { taint with tokens }))))
        | T.ToSink { taints_with_precondition = taints, _requires; sink; _ } ->
            let incoming_taints =
              taints
              |> List.concat_map (fun { T.taint; sink_trace } ->
                     match taint.T.orig with
                     | T.Src _ ->
                         (* Here, we do not modify the call trace or the taint.
                            This is because this means that, without our intervention, a
                            source of taint reaches the sink upon invocation of this function.
                            As such, we don't need to touch its call trace.
                         *)
                         (* Additionally, we keep this taint around, as compared to before,
                            when we assumed that only a single taint was necessary to produce
                            a finding.
                            Before, we assumed we could get rid of it because a
                            previous `findings_of_tainted_sink` call would have already
                            reported on this source. However, with interprocedural taint labels,
                            a finding may now be dependent on multiple such taints. If we were
                            to get rid of this source taint now, we might fail to report a
                            finding from a function call, because we failed to store the information
                            of this source taint within that function's taint signature.

                            e.g.

                            def bar(y):
                              foo(y)

                            def foo(x):
                              a = source_a
                              sink_of_a_and_b(a, x)

                            Here, we need to keep the source taint around, or our `bar` function
                            taint signature will fail to realize that the taint of `source_a` is
                            going into `sink_of_a_and_b`, and we will fail to produce a finding.
                         *)
                         [
                           {
                             T.taint = taint |> subst_in_precondition;
                             sink_trace;
                           };
                         ]
                     | Arg arg ->
                         (* Here, we modify the call trace associated to the argument,
                            and then we replace it by all the taints that correspond to it.
                         *)
                         let sink_trace =
                           T.Call (eorig, taint.tokens, sink_trace)
                         in
                         let+ arg_taints = arg_to_taints arg in
                         Taints.elements arg_taints
                         |> Common.map (fun x -> { T.taint = x; sink_trace }))
            in
            findings_of_tainted_sink env incoming_taints sink
            |> report_findings env;
            []
        | T.ToArg (taints, dst_arg) ->
            (* Taints 'taints' go into an argument of the call, by side-effect.
             * Right now this is mainly used to track taint going into specific
             * fields of the callee object, like `this.x = "tainted"`. *)
            let+ dst_lval, dst_obj =
              (* 'dst_lval' is the actual argument/l-value that corresponds
                 * to the formal argument 'dst_arg'. *)
              lval_of_sig_arg fun_exp fparams args dst_arg
            in
            taints
            |> List.concat_map (fun t ->
                   let dst_taints =
                     match t.T.orig with
                     | Src _ -> Taints.singleton (t |> subst_in_precondition)
                     | Arg src_arg ->
                         (* Taint is flowing from one argument to another argument
                          * (or possibly the callee object). Given the formal poly
                          * taint 'src_arg', we compute the actual taint in the
                          * context of this function call. *)
                         let& res = arg_to_taints src_arg in
                         res
                         |> Taints.map (fun taint ->
                                let tokens =
                                  List.rev_append t.tokens
                                    (snd dst_obj.ident :: taint.T.tokens)
                                in
                                { taint with tokens })
                   in
                   if Taints.is_empty dst_taints then []
                   else [ `UpdateEnv (dst_lval, dst_taints) ])
      in
      Some
        (fun_sig
        |> List.concat_map process_sig
        |> List.fold_left
             (fun (taints_acc, lval_env) fsig ->
               match fsig with
               | `Return taints -> (Taints.union taints taints_acc, lval_env)
               | `UpdateEnv (lval, taints) ->
                   (taints_acc, Lval_env.add lval_env lval taints))
             (Taints.empty, env.lval_env))
  | None, _
  | Some _, _ ->
      None

let check_function_call_callee env e = check_tainted_expr env e

(* Check the actual arguments of a function call. This also handles left-to-right
 * taint propagation by chaining the 'lval_env's returned when checking the arguments.
 * For example, given `foo(x.a)` we'll check whether `x.a` is tainted or whether the
 * argument is a sink. *)
let check_function_call_arguments env args =
  let args_taints, (lval_env, all_taints) =
    args
    |> List.fold_left_map
         (fun (lval_env, all_taints) arg ->
           let taints, lval_env =
             check_tainted_expr { env with lval_env }
               (IL_helpers.exp_of_arg arg)
           in
           let new_acc = (lval_env, taints :: all_taints) in
           match arg with
           | Unnamed _ -> (new_acc, Unnamed taints)
           | Named (id, _) -> (new_acc, Named (id, taints)))
         (env.lval_env, [])
    |> Common2.swap
  in
  let all_args_taints = List.fold_left Taints.union Taints.empty all_taints in
  (args_taints, all_args_taints, lval_env)

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_instr env instr : Taints.t * Lval_env.t =
  let check_expr env = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) ->
        let taints, lval_env = check_expr env e in
        let taints =
          check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
        in
        (taints, lval_env)
    | AssignAnon _ -> (Taints.empty, env.lval_env) (* TODO *)
    | Call (_, e, args) ->
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        let e_taints, lval_env =
          check_function_call_callee { env with lval_env } e
        in
        (* After we introduced Top_sinks, we need to explicitly support sinks like
         * `sink(...)` by considering that all of the parameters are sinks. To make
         * sure that we are backwards compatible, we do this for any sink that does
         * not match the `is_func_sink_with_focus` pattern.
         *)
        check_orig_if_sink { env with lval_env } instr.iorig all_args_taints
          ~filter_sinks:(fun m -> not (is_func_sink_with_focus m.spec));
        let call_taints, lval_env =
          match
            check_function_signature { env with lval_env } e args args_taints
          with
          | Some (call_taints, lval_env) -> (call_taints, lval_env)
          | None ->
              let call_taints =
                if not (propagate_through_functions env) then Taints.empty
                else
                  (* Otherwise assume that the function will propagate
                     * the taint of its arguments. *)
                  all_args_taints
              in
              let getter_taints, lval_env =
                (* HACK: Java: If we encounter `obj.setX(arg)` we interpret it as
                 * `obj.x = arg`, if we encounter `obj.getX()` we interpret it as
                 * `obj.x`. *)
                propagate_taint_via_unresolved_java_getters_and_setters
                  { env with lval_env } e args all_args_taints
              in
              let call_taints = Taints.union call_taints getter_taints in
              (call_taints, lval_env)
        in
        (* We add the taint of the function itselt (i.e., 'e_taints') too. *)
        let all_call_taints = Taints.union e_taints call_taints in
        let all_call_taints =
          check_type_and_drop_taints_if_bool_or_number env all_call_taints
            type_of_expr e
        in
        (all_call_taints, lval_env)
    | New (_lval, _ty, Some constructor, args) -> (
        (* 'New' with reference to constructor, although it doesn't mean it has been resolved. *)
        let args_taints, all_args_taints, lval_env =
          check_function_call_arguments env args
        in
        match
          check_function_signature { env with lval_env } constructor args
            args_taints
        with
        | Some (call_taints, lval_env) -> (call_taints, lval_env)
        | None -> (all_args_taints, lval_env))
    | New (_, ty, None, args) ->
        (* 'New' without reference to constructor *)
        let exps = ty.exps @ (args |> Common.map IL_helpers.exp_of_arg) in
        exps |> union_map_taints_and_vars env check_expr
    | CallSpecial (_, _, args) ->
        args
        |> Common.map IL_helpers.exp_of_arg
        |> union_map_taints_and_vars env check_expr
    | FixmeInstr _ -> (Taints.empty, env.lval_env)
  in
  let sanitizer_pms = orig_is_sanitized env.config instr.iorig in
  match sanitizer_pms with
  (* See NOTE [is_sanitizer] *)
  | _ :: _ ->
      (* TODO: We should check that taint and sanitizer(s) are unifiable. *)
      (Taints.empty, env.lval_env)
  | [] ->
      let taints_instr, lval_env' = check_instr instr.i in
      let taint_sources =
        orig_is_source env.config instr.iorig
        |> taints_of_matches ~incoming:taints_instr
      in
      let taints = Taints.union taints_instr taint_sources in
      let taints_propagated, lval_env' =
        handle_taint_propagators
          { env with lval_env = lval_env' }
          (`Ins instr) taints
      in
      let taints = Taints.union taints taints_propagated in
      check_orig_if_sink env instr.iorig taints;
      (taints, lval_env')

(* Test whether a `return' is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
let check_tainted_return env tok e : Taints.t * Lval_env.t =
  let sinks =
    env.config.is_sink (G.Tk tok) @ orig_is_sink env.config e.eorig
    |> List.filter (Top_sinks.is_best_match env.top_sinks)
    |> Common.map sink_of_match
  in
  let taints, var_env' = check_tainted_expr env e in
  let taints =
    check_type_and_drop_taints_if_bool_or_number env taints type_of_expr e
  in
  let findings = findings_of_tainted_sinks env taints sinks in
  report_findings env findings;
  (taints, var_env')

let findings_from_arg_updates_at_exit enter_env exit_env : T.finding list =
  (* TOOD: We need to get a map of `lval` to `Taint.arg`, and if an extension
   * of `lval` has new taints, then we can compute its correspoding `Taint.arg`
   * extension and generate an `ArgToArg` finding too. *)
  enter_env |> Lval_env.seq_of_tainted |> List.of_seq
  |> List.concat_map (fun (lval, enter_taints) ->
         (* For each lval in the enter_env, we get its `T.arg`, and check
          * if it got new taints at the exit_env. If so, we generate an
          * ArgToArg. *)
         match
           enter_taints |> Taints.elements
           |> Common.map_filter (fun taint ->
                  match taint.T.orig with
                  | T.Arg arg -> Some arg
                  | _ -> None)
         with
         | []
         | _ :: _ :: _ ->
             []
         | [ arg ] ->
             let exit_taints =
               Lval_env.dumb_find exit_env lval |> status_to_taints
             in
             let new_taints = Taints.diff exit_taints enter_taints in
             (* TODO: Also report if taints are _cleaned_. *)
             if not (Taints.is_empty new_taints) then
               [ T.ToArg (new_taints |> Taints.elements, arg) ]
             else [])

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

let input_env ~enter_env ~(flow : F.cfg) mapping ni =
  let node = flow.graph#nodes#assoc ni in
  match node.F.n with
  | Enter -> enter_env
  | _else -> (
      let pred_envs =
        CFG.predecessors flow ni
        |> Common.map (fun (pi, _) -> mapping.(pi).D.out_env)
      in
      match pred_envs with
      | [] -> Lval_env.empty
      | [ penv ] -> penv
      | penv1 :: penvs -> List.fold_left Lval_env.union penv1 penvs)

let transfer :
    Lang.t ->
    Rule_options.t ->
    config ->
    Lval_env.t ->
    string option ->
    flow:F.cfg ->
    top_sinks:Top_sinks.t ->
    Lval_env.t D.transfn =
 fun lang options config enter_env opt_name ~flow ~top_sinks
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  (* DataflowX.display_mapping flow mapping show_tainted; *)
  let in' : Lval_env.t = input_env ~enter_env ~flow mapping ni in
  let node = flow.graph#nodes#assoc ni in
  let out' : Lval_env.t =
    let env =
      { lang; options; config; fun_name = opt_name; lval_env = in'; top_sinks }
    in
    match node.F.n with
    | NInstr x ->
        let taints, lval_env' = check_tainted_instr env x in
        let opt_lval = LV.lval_of_instr_opt x in
        let lval_env' =
          match opt_lval with
          | Some lval ->
              (* We call `check_tainted_lval` here because the assigned `lval`
               * itself could be annotated as a source of taint. *)
              let taints, lval_env' =
                check_tainted_lval { env with lval_env = lval_env' } lval
              in
              (* We check if the instruction is a sink, and if so the taints
               * from the `lval` could make a finding. *)
              check_orig_if_sink env x.iorig taints;
              lval_env'
          | None -> lval_env'
        in
        let lval_env' =
          let has_taints = not (Taints.is_empty taints) in
          match opt_lval with
          | Some lval -> (
              if has_taints then
                (* Instruction returns tainted data, add taints to lval.
                 * See [Taint_lval_env] for details. *)
                Lval_env.add lval_env' lval taints
              else
                (* Instruction returns safe data, remove taints from lval.
                 * See [Taint_lval_env] for details. *)
                match x.i with
                | New _ ->
                    (* Pro/HACK: `x = new T(args)` is interpreted as `x.T(args)` where `T`
                     * is the constructor. But `x.T` does not return any taint, taint is
                     * propagated to `x` by side-effect, and in a field-sensitivity way.
                     * If we clean `x` here, then we would be removing that taint.
                     *
                     * TODO: `new T(args)` should return an "object taint signature". *)
                    lval_env'
                | _ -> Lval_env.clean lval_env' lval)
          | None ->
              (* Instruction returns 'void' or its return value is ignored. *)
              lval_env'
        in
        lval_env'
    | NCond (_tok, e)
    | NThrow (_tok, e) ->
        let _, lval_env' = check_tainted_expr env e in
        lval_env'
    | NReturn (tok, e) ->
        (* TODO: Move most of this to check_tainted_return. *)
        let taints, lval_env' = check_tainted_return env tok e in
        let findings = findings_of_tainted_return taints tok in
        report_findings env findings;
        lval_env'
    | NLambda params ->
        params
        |> List.fold_left
             (fun lval_env var ->
               let _, lval_env = check_tainted_var { env with lval_env } var in
               lval_env)
             in'
    | NGoto _
    | Enter
    | Exit
    | TrueNode
    | FalseNode
    | Join
    | NOther _
    | NTodo _ ->
        in'
  in
  { D.in_env = in'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint :
      ?in_env:Lval_env.t ->
      ?name:Var_env.var ->
      Lang.t ->
      Rule_options.t ->
      config ->
      F.cfg ->
      mapping) =
 fun ?in_env ?name:opt_name lang options config flow ->
  let init_mapping = DataflowX.new_node_array flow Lval_env.empty_inout in
  let enter_env =
    match in_env with
    | None -> Lval_env.empty
    | Some in_env -> in_env
  in
  let top_sinks =
    (* Here we compute the "canonical" or "top" sink matches, for each sink we check
     * whether there is a "best match" among the top nodes in the CFG.
     * See NOTE "Top sinks" *)
    top_level_sinks_in_nodes config flow
  in
  (* THINK: Why I cannot just update mapping here ? if I do, the mapping gets overwritten later on! *)
  (* DataflowX.display_mapping flow init_mapping show_tainted; *)
  let end_mapping =
    DataflowX.fixpoint ~eq_env:Lval_env.equal ~init:init_mapping
      ~trans:(transfer lang options config enter_env opt_name ~flow ~top_sinks)
        (* tainting is a forward analysis! *)
      ~forward:true ~flow
  in
  let exit_env = end_mapping.(flow.exit).D.out_env in
  ( findings_from_arg_updates_at_exit enter_env exit_env |> fun findings ->
    if findings <> [] then config.handle_findings opt_name findings exit_env );
  end_mapping
