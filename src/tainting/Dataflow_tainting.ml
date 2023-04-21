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

module LabelSet = Set.Make (String)
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
  rule_id : string;
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
  options : Config_semgrep.t; (* rule options *)
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
let trace_of_match x = T.trace_of_pm (x.spec_pm, x.spec)

let taints_of_matches xs =
  xs |> Common.map (fun x -> (x.spec_pm, x.spec)) |> T.taints_of_pms

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

let unify_mvars_sets mvars1 mvars2 =
  let xs =
    List.fold_left
      (fun xs (mvar, mval) ->
        xs >>= fun xs ->
        match List.assoc_opt mvar mvars2 with
        | None -> Some ((mvar, mval) :: xs)
        | Some mval' ->
            if Metavariable.equal_mvalue mval mval' then
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

let merge_source_mvars bindings =
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
             if not (Metavariable.equal_mvalue mval mval') then
               Hashtbl.remove bindings_tbl mvar);
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
    unify_mvars_sets source_mvars sink_mvars
  else
    (* The union of both sets, but taking the sink mvars in case of collision. *)
    sink_biased_union_mvars source_mvars sink_mvars

let partition_mutating_sources sources_matches =
  sources_matches
  |> List.partition (fun (m : R.taint_source tmatch) ->
         m.spec.source_by_side_effect && is_exact m)

let labels_of_taints taints : LabelSet.t =
  taints |> Taints.to_seq
  |> Seq.filter_map (fun (t : T.taint) ->
         match t.orig with
         | Src src -> Some src.label
         | Arg _ -> None)
  |> LabelSet.of_seq

(* coupling: if you modify the code here, you may need to modify 'Parse_rule.parse_taint_requires' too. *)
let rec eval_label_requires ~labels e =
  match e.G.e with
  | G.L (G.Bool (v, _)) -> v
  | G.N (G.Id (id, _)) ->
      let str, _ = id in
      LabelSet.mem str labels
  | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg e1 ], _)) ->
      not (eval_label_requires ~labels e1)
  | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
      match (op, eval_label_args ~labels args) with
      | G.And, xs -> List.fold_left ( && ) true xs
      | G.Or, xs -> List.fold_left ( || ) false xs
      | __else__ ->
          logger#error "Unexpected Boolean operator";
          false)
  | G.ParenExpr (_, e, _) -> eval_label_requires ~labels e
  | ___else__ ->
      logger#error "Unexpected `requires' expression";
      false

and eval_label_args ~labels args =
  match args with
  | [] -> []
  | G.Arg e :: args' ->
      eval_label_requires ~labels e :: eval_label_args ~labels args'
  | _ :: args' ->
      logger#error "Unexpected argument kind";
      false :: eval_label_args ~labels args'

let taints_satisfy_requires taints expr =
  let labels = labels_of_taints taints in
  eval_label_requires ~labels expr

(* Potentially produces a finding from incoming taints to a sink.
   Note that, while this sink has a `requires` and incoming labels,
   we decline to solve this now!
   We will figure out how many actual Semgrep findings are generated
   when this information is used, later.
*)
let findings_of_tainted_sink env taints (sink : T.sink) : T.finding list =
  (* We cannot check whether we satisfy the `requires` here.
     This is because this sink may be inside of a function, meaning that
     argument taint can reach it, which can only be instantiated at the
     point where we call the function.
     So we record the `requires` within the taint finding, and evaluate
     the formula later, when we extract the PMs
  *)
  let sink_pm, ts = T.pm_of_trace sink in
  let taints_and_bindings =
    Taints.elements taints
    |> Common.map (fun t ->
           let bindings =
             match t.T.orig with
             | T.Arg _ -> []
             | Src source ->
                 let src_pm, _ = T.pm_of_trace source.call_trace in
                 src_pm.PM.env
           in
           ({ t with tokens = List.rev t.tokens }, bindings))
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
      taints_and_bindings |> Common.map snd |> merge_source_mvars
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
        ]

(* Produces a finding for every unifiable source-sink pair. *)
let findings_of_tainted_sinks env taints sinks : T.finding list =
  sinks |> List.concat_map (findings_of_tainted_sink env taints)

let finding_of_tainted_return taints return_tok : T.finding =
  let taints = taints |> Taints.elements in
  T.ToReturn
    ( taints |> Common.map (fun t -> { t with T.tokens = List.rev t.T.tokens }),
      return_tok )

let check_orig_if_sink env ?filter_sinks orig taints =
  let sinks = orig_is_best_sink env orig in
  let sinks =
    match filter_sinks with
    | None -> sinks
    | Some sink_pred -> sinks |> List.filter sink_pred
  in
  let sinks = sinks |> Common.map trace_of_match in
  let findings = findings_of_tainted_sinks env taints sinks in
  report_findings env findings

let filter_new_taint_sources_by_labels labels taints =
  Taints.fold
    (fun new_taint taints ->
      match new_taint.orig with
      | Arg _ -> Taints.add new_taint taints
      | Src src ->
          let _, ts = T.pm_of_trace src.call_trace in
          let req = eval_label_requires ~labels ts.source_requires in
          if req then Taints.add new_taint taints else taints)
    taints Taints.empty

let union_new_taint_sources_filtering_labels ~new_ curr =
  let labels = labels_of_taints curr in
  let new_filtered = filter_new_taint_sources_by_labels labels new_ in
  Taints.union new_filtered curr

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

let propagate_taint_via_java_setter env e args all_args_taints =
  match (e, args) with
  | ( {
        e =
          Fetch
            ({ base = Var _obj; rev_offset = [ ({ o = Dot m; _ } as offset) ] }
            as lval);
        _;
      },
      [ _ ] )
    when env.lang =*= Lang.Java
         && Stdcompat.String.starts_with ~prefix:"set" (fst m.IL.ident) ->
      let prop_name = "get" ^ Str.string_after (fst m.IL.ident) 3 in
      let prop_lval =
        let o = Dot { m with ident = (prop_name, snd m.IL.ident) } in
        { lval with rev_offset = [ { offset with o } ] }
      in
      if not (Taints.is_empty all_args_taints) then
        Lval_env.add env.lval_env prop_lval all_args_taints
      else env.lval_env
  | __else__ -> env.lval_env

let resolve_poly_taint_for_java_getters env lval st =
  (* NOTE: This is a hack and it doesn't handle all cases, but it's mean to handle
   * the most basic ones. It does work for more than just getters. However, it
   * needs some testing and for now it's safer to restrict it to Java and getX. *)
  if env.lang =*= Java then
    match lval.rev_offset with
    | { o = Dot n; _ } :: _
      when Stdcompat.String.starts_with ~prefix:"get" (fst n.ident) -> (
        match st with
        | `Sanitized
        | `Clean
        | `None ->
            st
        | `Tainted taints ->
            let taints' =
              taints
              |> Taints.map (fun taint ->
                     match taint.orig with
                     | Arg arg ->
                         let arg' = { arg with offset = arg.offset @ [ n ] } in
                         { taint with orig = Arg arg' }
                     | Src _ -> taint)
            in
            `Tainted taints')
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
  (* These are all the labels flowing in to the current thing we're looking at. *)
  let labels = labels_of_taints taints in
  let lval_env =
    (* `thing` is the source (the "from") of propagation, we add its taints to
     * the environment. *)
    List.fold_left
      (fun lval_env prop ->
        (* Only propagate if the current set of taint labels can satisfy the
           propagator's requires precondition.
        *)
        if eval_label_requires ~labels prop.spec.prop.propagator_requires then
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
        else lval_env)
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

let find_lval_taint_sources env ~labels lval =
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
    reg_source_pms |> taints_of_matches
    |> filter_new_taint_sources_by_labels labels
  and taints_sources_mut =
    mut_source_pms |> taints_of_matches
    |> filter_new_taint_sources_by_labels labels
  in
  let lval_env = Lval_env.add env.lval_env lval taints_sources_mut in
  (Taints.union taints_sources_reg taints_sources_mut, lval_env)

let rec check_tainted_lval env (lval : IL.lval) : Taints.t * Lval_env.t =
  let new_taints, lval_in_env, lval_env = check_tainted_lval_aux env lval in
  let taints_from_env = status_to_taints lval_in_env in
  let taints = Taints.union new_taints taints_from_env in
  let sinks =
    lval_is_sink env.config lval
    |> List.filter (Top_sinks.is_best_match env.top_sinks)
    |> Common.map trace_of_match
  in
  let findings = findings_of_tainted_sinks { env with lval_env } taints sinks in
  report_findings { env with lval_env } findings;
  (taints, lval_env)

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
                (* HACK(field-sensitivity): Java: If we encounter `obj.getX` and `obj` has
                   * polymorphic taint,  and we know nothing specific about `obj.getX`, then
                   * we add the same offset `.getX` to the polymorphic taint coming from `obj`.
                   * See also 'propagate_taint_via_java_setter'. *)
                resolve_poly_taint_for_java_getters env lval st)
      in
      let taints_from_env = status_to_taints lval_in_env in
      (* Find taint sources matching lval. *)
      let current_taints = Taints.union sub_new_taints taints_from_env in
      let labels = labels_of_taints current_taints in
      let taints_from_sources, lval_env =
        find_lval_taint_sources { env with lval_env } ~labels lval
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
        |> Common.map trace_of_match
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
      let taints_sources =
        orig_is_source env.config exp.eorig |> taints_of_matches
      in
      let taints_exp, lval_env = check_subexpr exp in
      let taints =
        taints_exp
        |> union_new_taint_sources_filtering_labels ~new_:taints_sources
      in
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
 * But 'sig_arg' may also specify an offset.
 *)
let lval_of_sig_arg fun_exp fparams args_exps (sig_arg : T.arg) =
  let* base_lval, obj =
    match sig_arg.pos with
    | "<this>", -1 -> (
        match fun_exp with
        | {
         e = Fetch { base = Var obj; rev_offset = [ { o = Dot _method; _ } ] };
         _;
        } ->
            Some ({ base = Var obj; rev_offset = [] }, obj)
        | __else__ -> None)
    | pos -> (
        let* arg_exp = find_pos_in_actual_args args_exps fparams pos in
        match arg_exp.e with
        | Fetch ({ base = Var obj; _ } as arg_lval) -> Some (arg_lval, obj)
        | __else__ -> None)
  in
  let os =
    sig_arg.offset |> Common.map (fun x -> { o = Dot x; oorig = NoOrig })
  in
  let lval =
    { base_lval with rev_offset = List.rev_append os base_lval.rev_offset }
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

let check_function_signature env fun_exp args args_taints =
  match (!hook_function_taint_signature, fun_exp) with
  | Some hook, { e = Fetch f; eorig = SameAs eorig } ->
      let* fparams, fun_sig = hook env.config eorig in
      let process_sig : T.finding -> _ list = function
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
                              {
                                orig = Src { src with call_trace };
                                tokens = [];
                              }))
                   | Arg arg ->
                       let* arg_taints =
                         taints_of_sig_arg env fparams fun_exp args args_taints
                           arg
                       in
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
            (* TODO(brandon): use arg taints once interproc taint labels are a thing
            *)
            taints
            |> Common.map_filter (fun t ->
                   match t.T.orig with
                   | Src _ ->
                       (* THINK: Should we report something here? *)
                       None
                   (* TODO(brandon): this is wrong
                       this assumes a world where only one arg taint is ever relevant to
                       a given sink
                       when we refactor to allow sinks of multiple labeled taints,
                       we need to change this case. probably, it will involve producing
                       a single ToSink "finding" which uses all the taints, including
                       the argument taints induced here.
                   *)
                   | Arg arg ->
                       let sink = T.Call (eorig, t.tokens, sink) in
                       let* arg_taints =
                         taints_of_sig_arg env fparams fun_exp args args_taints
                           arg
                       in
                       arg_taints
                       |> Taints.iter (fun t ->
                              findings_of_tainted_sinks env (Taints.singleton t)
                                [ sink ]
                              |> report_findings env);
                       None)
        | T.ArgToArg (src_arg, tokens, dst_arg) ->
            let+ src_taints =
              taints_of_sig_arg env fparams fun_exp args args_taints src_arg
            in
            let+ dst_lval, dst_obj =
              lval_of_sig_arg fun_exp fparams args dst_arg
            in
            let dst_taints =
              src_taints
              |> Taints.map (fun taint ->
                     let tokens =
                       List.rev_append tokens (snd dst_obj.ident :: taint.tokens)
                     in
                     { taint with tokens })
            in
            if Taints.is_empty dst_taints then []
            else [ `UpdateEnv (dst_lval, dst_taints) ]
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

(* Test whether an instruction is tainted, and if it is also a sink,
 * report the finding too (by side effect). *)
(* TODO: This should return a new var_env rather than just taint, it
 * makes more sense given that an instruction may have side-effects.
 * It Also makes simpler to handle sanitization by side-effect. *)
let check_tainted_instr env instr : Taints.t * Lval_env.t =
  let check_expr env = check_tainted_expr env in
  let check_instr = function
    | Assign (_, e) -> check_expr env e
    | AssignAnon _ -> (Taints.empty, env.lval_env) (* TODO *)
    | Call (_, e, args) ->
        let args_taints, (lval_env, all_taints) =
          args
          |> List.fold_left_map
               (fun (lval_env, all_taints) arg ->
                 let taints, lval_env =
                   check_expr { env with lval_env } (IL_helpers.exp_of_arg arg)
                 in
                 let new_acc = (lval_env, taints :: all_taints) in
                 match arg with
                 | Unnamed _ -> (new_acc, Unnamed taints)
                 | Named (id, _) -> (new_acc, Named (id, taints)))
               (env.lval_env, [])
          |> Common2.swap
        in
        let e_taints, lval_env = check_expr { env with lval_env } e in
        let all_args_taints =
          List.fold_left Taints.union Taints.empty all_taints
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
              let lval_env =
                (* HACK: Java: If we encounter `obj.setX(arg)` we interpret this as `obj.getX = arg`. *)
                propagate_taint_via_java_setter { env with lval_env } e args
                  all_args_taints
              in
              (call_taints, lval_env)
        in
        (* We add the taint of the function itselt (i.e., 'e_taints') too.
         * DEEP: In DeepSemgrep this also helps identifying `x.foo()` as tainted
         * when `x` is tainted, because the call is represented in IL as `(x.foo)()`.
         * TODO: Properly track taint through objects. *)
        (Taints.union e_taints call_taints, lval_env)
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
      let taint_sources =
        orig_is_source env.config instr.iorig |> taints_of_matches
      in
      let taints_instr, lval_env' = check_instr instr.i in
      let taints =
        taints_instr
        |> union_new_taint_sources_filtering_labels ~new_:taint_sources
      in
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
    |> Common.map trace_of_match
  in
  let taints, var_env' = check_tainted_expr env e in
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
             new_taints |> Taints.elements
             |> Common.map_filter (fun taint ->
                    match taint.T.orig with
                    | T.Arg t -> Some (T.ArgToArg (t, taint.tokens, arg))
                    | T.Src _ -> None (* TODO SrcToArg *)))

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
    Config_semgrep.t ->
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
          | Some lval ->
              if has_taints then
                (* Instruction returns tainted data, add taints to lval.
                 * See [Taint_lval_env] for details. *)
                Lval_env.add lval_env' lval taints
              else
                (* Instruction returns safe data, remove taints from lval.
                 * See [Taint_lval_env] for details. *)
                Lval_env.clean lval_env' lval
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
        let finding = finding_of_tainted_return taints tok in
        report_findings env [ finding ];
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
      Config_semgrep.t ->
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
