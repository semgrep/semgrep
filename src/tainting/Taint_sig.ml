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

open Common
module Log = Log_tainting.Log
module G = AST_generic
module R = Rule
module T = Taint
module Taints = T.Taint_set

(*****************************************************************************)
(* Taint shapes *)
(*****************************************************************************)
(*
 * Record the "shape" of the things we track, taint shapes are a bit like types.
 * Right now this is mainly to support field- and index-sensitivity. Shapes also
 * provide a good foundation to later add alias analysis.
 *
 * This is somewhat inspired by
 * "Polymorphic type, region and effect inference"
 * by Jean-Pierre Talpin and Pierre Jouvelot.
 *
 * Previously, we had a flat environment from l-values to their taint, and we had
 * to "reconstruct" the shape of objects when needed. For example, to check if a
 * variable was a struct, we looked for l-values in the environment that were an
 * "extension" of that variable. By recording shapes explicitly, implementing
 * field-sensitivity becomes more natural.
 *
 * TODO: We want `Dataflow_tainting.check_tainted_xyz` to return a taint shape.
 * TODO: Add 'Ptr' shapes and track aliasing.
 *)

(*********************************************************)
(* Types *)
(*********************************************************)

module Fields = Map.Make (struct
  type t = T.offset

  let compare = T.compare_offset
end)

(* THINK: To assign a proper taint signature to a function like:
 *
 *      def foo(t):
 *          return t
 *
 *  we need shape variables... but then we need to handle unification
 *  between shape variables too.
 *)
type shape = Bot | Obj of obj | Arg of T.arg
and cell = Cell of Xtaint.t * shape
and obj = cell Fields.t

(*********************************************************)
(* Helpers *)
(*********************************************************)

(* UNSAFE: Violates INVARIANT(cell), see 'internal_UNSAFE_find_offset_in_obj' *)
let cell_none_bot = Cell (`None, Bot)

(* Temporarily breaks INVARIANT(cell) by initializing a field with the shape
 * 'cell<0>(_|_)', but right away the field should be either tainted or cleaned.
 * The caller must restore the invariant. *)
let internal_UNSAFE_find_offset_in_obj o obj =
  match Fields.find_opt o obj with
  | Some _ -> (o, obj)
  | None ->
      let num_fields = Fields.cardinal obj in
      if num_fields <= Limits_semgrep.taint_MAX_OBJ_FIELDS then
        let obj = Fields.add o cell_none_bot obj in
        (o, obj)
      else (
        Log.warn (fun m ->
            m "Already tracking too many fields, will not track %s"
              (T.show_offset o));
        (Oany, obj))

(*********************************************************)
(* Misc *)
(*********************************************************)

let taints_and_shape_are_relevant taints shape =
  match (Taints.is_empty taints, shape) with
  | true, Bot -> false
  | __else__ ->
      (* Either 'taints' is non-empty, or 'shape' is non-'Bot' and hence
       * by INVARIANT(cell) it contains some taint or has field marked clean. *)
      true

(*********************************************************)
(* Equality *)
(*********************************************************)

let rec equal_cell cell1 cell2 =
  let (Cell (taints1, shape1)) = cell1 in
  let (Cell (taints2, shape2)) = cell2 in
  Xtaint.equal taints1 taints2 && equal_shape shape1 shape2

and equal_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, Bot -> true
  | Obj obj1, Obj obj2 -> equal_obj obj1 obj2
  | Arg arg1, Arg arg2 -> T.equal_arg arg1 arg2
  | Bot, _
  | Obj _, _
  | Arg _, _ ->
      false

and equal_obj obj1 obj2 = Fields.equal equal_cell obj1 obj2

(*********************************************************)
(* Comparison *)
(*********************************************************)

let rec compare_cell cell1 cell2 =
  let (Cell (taints1, shape1)) = cell1 in
  let (Cell (taints2, shape2)) = cell2 in
  match Xtaint.compare taints1 taints2 with
  | 0 -> compare_shape shape1 shape2
  | other -> other

and compare_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, Bot -> 0
  | Obj obj1, Obj obj2 -> compare_obj obj1 obj2
  | Arg arg1, Arg arg2 -> T.compare_arg arg1 arg2
  | Bot, Obj _
  | Bot, Arg _
  | Obj _, Arg _ ->
      -1
  | Obj _, Bot
  | Arg _, Bot
  | Arg _, Obj _ ->
      1

and compare_obj obj1 obj2 = Fields.compare compare_cell obj1 obj2

(*********************************************************)
(* Pretty-printing *)
(*********************************************************)

let rec show_cell cell =
  let (Cell (xtaint, shape)) = cell in
  spf "cell<%s>(%s)" (Xtaint.show xtaint) (show_shape shape)

and show_shape = function
  | Bot -> "_|_"
  | Obj obj -> spf "obj {|%s|}" (show_obj obj)
  | Arg arg -> "'{" ^ T.show_arg arg ^ "}"

and show_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, o_cell) ->
         spf "%s: %s" (T.show_offset o) (show_cell o_cell))
  |> List.of_seq |> String.concat "; "

(*********************************************************)
(* Object shapes *)
(*********************************************************)

let tuple_like_obj taints_and_shapes : obj =
  let _index, obj =
    taints_and_shapes
    |> List.fold_left
         (fun (i, obj) (taints, shape) ->
           match (Xtaint.of_taints taints, shape) with
           | `None, Bot ->
               (* We skip this index to maintain INVARIANT(cell). *)
               (i + 1, obj)
           | xtaint, shape ->
               let cell = Cell (xtaint, shape) in
               (i + 1, Fields.add (T.Oint i) cell obj))
         (0, Fields.empty)
  in
  obj

(*********************************************************)
(* Unification (merging shapes) *)
(*********************************************************)

let rec unify_cell cell1 cell2 =
  let (Cell (xtaint1, shape1)) = cell1 in
  let (Cell (xtaint2, shape2)) = cell2 in
  (* TODO: Apply 'Flag_semgrep.max_taint_set_size' here too ? *)
  let xtaint = Xtaint.union xtaint1 xtaint2 in
  let shape = unify_shape shape1 shape2 in
  Cell (xtaint, shape)

and unify_shape shape1 shape2 =
  match (shape1, shape2) with
  | Bot, shape
  | shape, Bot ->
      shape
  | Obj obj1, Obj obj2 -> Obj (unify_obj obj1 obj2)
  | Arg _, (Obj _ as obj)
  | (Obj _ as obj), Arg _ ->
      obj
  | Arg arg1, Arg arg2 ->
      if T.equal_arg arg1 arg2 then shape1
      else (
        (* TODO: We do not handle this right now, we would need to record and
         *   solve constraints. It can happen with code like e.g.
         *
         *     def foo(a, b):
         *       tup = (a,)
         *       tup[0] = b
         *       return tup
         *
         * Then the consequence would be that the signature of `foo` would ignore
         * the shape of `b`.
         *)
        Log.warn (fun m ->
            m "Trying to unify two different arg shapes: %s ~ %s"
              (T.show_arg arg1) (T.show_arg arg2));
        shape1)

and unify_obj obj1 obj2 =
  (* THINK: Apply taint_MAX_OBJ_FIELDS limit ? *)
  Fields.union (fun _ x y -> Some (unify_cell x y)) obj1 obj2

(*********************************************************)
(* Collect/union all taints *)
(*********************************************************)

(* THINK: Generalize to "fold" ? *)
let rec gather_all_taints_in_cell_acc acc cell =
  let (Cell (xtaint, shape)) = cell in
  match xtaint with
  | `Clean ->
      (* Due to INVARIANT(cell) we can just stop here. *)
      acc
  | `None -> gather_all_taints_in_shape_acc acc shape
  | `Tainted taints ->
      gather_all_taints_in_shape_acc (Taints.union taints acc) shape

and gather_all_taints_in_shape_acc acc = function
  | Bot -> acc
  | Obj obj -> gather_all_taints_in_obj_acc acc obj
  | Arg arg ->
      let taint = { T.orig = T.Shape_var (T.lval_of_arg arg); tokens = [] } in
      Taints.add taint acc

and gather_all_taints_in_obj_acc acc obj =
  Fields.fold
    (fun _ o_cell acc -> gather_all_taints_in_cell_acc acc o_cell)
    obj acc

let gather_all_taints_in_cell = gather_all_taints_in_cell_acc Taints.empty
let gather_all_taints_in_shape = gather_all_taints_in_shape_acc Taints.empty

(*********************************************************)
(* Find an offset *)
(*********************************************************)

let rec find_in_cell offset cell =
  let (Cell (_xtaint, shape)) = cell in
  match offset with
  | [] -> Some cell
  | _ :: _ -> find_in_shape offset shape

and find_in_shape offset = function
  (* offset <> [] *)
  | Bot -> None
  | Obj obj -> find_in_obj offset obj
  | Arg _ ->
      (* TODO: Here we should "refine" the arg shape, it should be an Obj shape. *)
      None

and find_in_obj (offset : T.offset list) obj =
  (* offset <> [] *)
  match offset with
  | [] ->
      Log.err (fun m -> m "fix_xtaint_obj: Impossible happened: empty offset");
      None
  | o :: offset -> (
      match o with
      | Oany (* arbitrary index [*] *) ->
          (* consider all fields/indexes *)
          Fields.fold
            (fun _ cell acc ->
              match (acc, find_in_cell offset cell) with
              | None, None -> None
              | Some cell, None
              | None, Some cell ->
                  Some cell
              | Some cell1, Some cell2 -> Some (unify_cell cell1 cell2))
            obj None
      | Ofld _
      | Oint _
      | Ostr _ ->
          let* o_cell = Fields.find_opt o obj in
          find_in_cell offset o_cell)

(*********************************************************)
(* Update the xtaint and shape of an offset *)
(*********************************************************)

(* Finds an 'offset' within a 'cell' and updates it via 'f'. *)
let rec update_offset_in_cell ~f offset cell =
  let xtaint, shape =
    match (cell, offset) with
    | Cell (xtaint, shape), [] -> f xtaint shape
    | Cell (xtaint, shape), _ :: _ ->
        let shape = update_offset_in_shape ~f offset shape in
        (xtaint, shape)
  in
  match (xtaint, shape) with
  (* Restore INVARIANT(cell).1 *)
  | `None, Bot -> None
  | `Tainted taints, Bot when Taints.is_empty taints -> None
  (* Restore INVARIANT(cell).2 *)
  | `Clean, (Obj _ | Arg _) ->
      (* If we are tainting an offset of this cell, the cell cannot be
         considered clean anymore. *)
      Some (Cell (`None, shape))
  | `Clean, Bot
  | `None, (Obj _ | Arg _)
  | `Tainted _, (Bot | Obj _ | Arg _) ->
      Some (Cell (xtaint, shape))

and update_offset_in_shape ~f offset = function
  | Bot
  | Arg _ ->
      let shape = Obj Fields.empty in
      update_offset_in_shape ~f offset shape
  | Obj obj -> (
      match update_offset_in_obj ~f offset obj with
      | None -> Bot
      | Some obj -> Obj obj)

and update_offset_in_obj ~f offset obj =
  let obj' =
    match offset with
    | [] ->
        Log.err (fun m ->
            m "internal_UNSAFE_update_obj: Impossible happened: empty offset");
        obj
    | o :: offset -> (
        let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
        match o with
        | Oany (* arbitrary index [*] *) ->
            (* consider all fields/indexes *)
            Fields.filter_map (fun _o' -> update_offset_in_cell ~f offset) obj
        | Ofld _
        | Oint _
        | Ostr _ ->
            obj
            |> Fields.update o (fun opt_cell ->
                   let* cell = opt_cell in
                   update_offset_in_cell ~f offset cell))
  in
  if Fields.is_empty obj' then None else Some obj'

(*********************************************************)
(* Updating an offset *)
(*********************************************************)

let update_offset_and_unify new_taints new_shape offset opt_cell =
  if taints_and_shape_are_relevant new_taints new_shape then
    let new_xtaint =
      (* THINK: Maybe Dataflow_tainting 'check_xyz' should be returning 'Xtaint.t'? *)
      Xtaint.of_taints new_taints
    in
    let cell = opt_cell ||| cell_none_bot in
    let add_new_taints xtaint shape =
      let shape = unify_shape new_shape shape in
      match xtaint with
      | `None
      | `Clean ->
          (* Since we're adding taint we cannot have `Clean here. *)
          (new_xtaint, shape)
      | `Tainted taints as xtaint ->
          if
            !Flag_semgrep.max_taint_set_size =|= 0
            || Taints.cardinal taints < !Flag_semgrep.max_taint_set_size
          then (Xtaint.union new_xtaint xtaint, shape)
          else (
            Log.warn (fun m ->
                m
                  "Already tracking too many taint sources for %s, will not \
                   track more"
                  (offset |> List_.map T.show_offset |> String.concat ""));
            (xtaint, shape))
    in
    update_offset_in_cell ~f:add_new_taints offset cell
  else
    (* To maintain INVARIANT(cell) we cannot return 'cell_none_bot'! *)
    opt_cell

(*********************************************************)
(* Clean taint *)
(*********************************************************)

(* TODO: Reformulate in terms of 'update_offset_in_cell' *)
let rec clean_cell (offset : T.offset list) cell =
  let (Cell (xtaint, shape)) = cell in
  match offset with
  | [] ->
      (* See INVARIANT(cell)
       *
       * THINK: If we had aliasing, we would have to keep the previous shape
       *  and just clean it all ? And we would also need to remove the 'Clean'
       *  mark from other cells that may be pointing to this cell in order to
       *  maintain the invariant ? *)
      Cell (`Clean, Bot)
  | [ Oany ] ->
      (* If an object is tainted, and we clean all its fields/indexes, then we
       * just clean the object itself. For example, if we assume that an array `a`
       * is tainted, and then we see `a[*]` being sanitized, then we assume that
       * `a` itself is being sanitized; otherwise `sink(a)` could be reported. *)
      Cell (`Clean, Bot)
  | _ :: _ ->
      let shape = clean_shape offset shape in
      Cell (xtaint, shape)

and clean_shape offset = function
  | Bot
  | Arg _ ->
      let shape = Obj Fields.empty in
      clean_shape offset shape
  | Obj obj -> Obj (clean_obj offset obj)

and clean_obj offset obj =
  match offset with
  | [] ->
      Log.err (fun m -> m "clean_obj: Impossible happened: empty offset");
      obj
  | o :: offset -> (
      let o, obj = internal_UNSAFE_find_offset_in_obj o obj in
      match o with
      | Oany -> Fields.map (clean_cell offset) obj
      | o ->
          Fields.update o (Option.map (fun cell -> clean_cell offset cell)) obj)

(*********************************************************)
(* Enumerate tainted offsets *)
(*********************************************************)

let rec enum_in_cell cell : (T.offset list * Taints.t) Seq.t =
  let (Cell (taints, shape)) = cell in
  let x =
    match taints with
    | `Tainted taints -> Seq.cons ([], taints) Seq.empty
    | `Clean
    | `None ->
        Seq.empty
  in
  Seq.append x (enum_in_shape shape)

and enum_in_shape = function
  | Bot -> Seq.empty
  | Obj obj -> enum_in_obj obj
  | Arg _ ->
      (* TODO: First need to record taint shapes in 'ToLval'.  *)
      Seq.empty

and enum_in_obj obj =
  obj |> Fields.to_seq
  |> Seq.map (fun (o, cell) ->
         enum_in_cell cell
         |> Seq.map (fun (offset, taints) -> (o :: offset, taints)))
  |> Seq.concat

(*****************************************************************************)
(* Taint results *)
(*****************************************************************************)

type sink = { pm : Pattern_match.t; rule_sink : R.taint_sink }
type taint_to_sink_item = { taint : T.taint; sink_trace : unit T.call_trace }

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

type taints_to_return = {
  data_taints : Taint.taint list;
  data_shape : shape;
  control_taints : Taint.taint list;
  return_tok : AST_generic.tok;
}

type result =
  | ToSink of taints_to_sink
  | ToReturn of taints_to_return
  | ToLval of T.taint list * T.lval (* TODO: CleanArg ? *)

(*********************************************************)
(* Comparison *)
(*********************************************************)

let compare_sink { pm = pm1; rule_sink = sink1 } { pm = pm2; rule_sink = sink2 }
    =
  match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
  | 0 -> T.compare_matches pm1 pm2
  | other -> other

let compare_taint_to_sink_item { taint = taint1; sink_trace = _ }
    { taint = taint2; sink_trace = _ } =
  T.compare_taint taint1 taint2

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
      match compare_shape data_shape1 data_shape2 with
      | 0 -> List.compare T.compare_taint control_taints1 control_taints2
      | other -> other)
  | other -> other

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

(*********************************************************)
(* Pretty-printing *)
(*********************************************************)

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

let show_taint_to_sink_item { taint; sink_trace } =
  let sink_trace_str =
    match sink_trace with
    | T.PM _ -> ""
    | T.Call _ -> spf "@{%s}" (Taint.show_call_trace [%show: unit] sink_trace)
  in
  Printf.sprintf "%s%s" (T.show_taint taint) sink_trace_str

let show_taints_and_traces taints =
  Common2.string_of_list show_taint_to_sink_item taints

let show_taints_to_sink { taints_with_precondition = taints, _; sink; _ } =
  Common.spf "%s ~~~> %s" (show_taints_and_traces taints) (show_sink sink)

let show_result = function
  | ToSink x -> show_taints_to_sink x
  | ToReturn { data_taints; data_shape; control_taints; return_tok = _ } ->
      Printf.sprintf "return (%s & %s & CTRL:%s)"
        (Common2.string_of_list T.show_taint data_taints)
        (show_shape data_shape)
        (Common2.string_of_list T.show_taint control_taints)
  | ToLval (taints, lval) ->
      Printf.sprintf "%s ----> %s"
        (Common2.string_of_list T.show_taint taints)
        (T.show_lval lval)

(*****************************************************************************)
(* Taint signatures *)
(*****************************************************************************)

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
let subst_in_precondition ~inst_lval ~inst_ctrl taint =
  let subst taints =
    taints
    |> List.concat_map (fun t ->
           match t.T.orig with
           | Src _ -> [ t ]
           | Var lval -> (
               match inst_lval lval with
               | None -> []
               | Some (var_taints, _var_shape) -> var_taints |> Taints.elements)
           | Shape_var lval -> (
               match inst_lval lval with
               | None -> []
               | Some (_var_taints, var_shape) ->
                   gather_all_taints_in_shape var_shape |> Taints.elements)
           | Control -> inst_ctrl () |> Taints.elements)
  in
  T.map_preconditions subst taint

let instantiate_taint_var ~inst_lval ~inst_ctrl taint =
  match taint.T.orig with
  | Src _ -> None
  | Var lval -> inst_lval lval
  | Shape_var lval ->
      (* This is just a delayed 'gather_all_taints_in_shape'. *)
      let* taints =
        inst_lval lval
        |> Option.map (fun (_taints, shape) -> gather_all_taints_in_shape shape)
      in
      Some (taints, Bot)
  | Control ->
      (* 'Control' is pretty much like a taint variable so we handle all together. *)
      Some (inst_ctrl (), Bot)

(* TODO: Move to 'Taint' module ? *)
let instantiate_taint ~callee ~inst_lval ~inst_ctrl taint =
  let inst_taint_var taint =
    instantiate_taint_var ~inst_lval ~inst_ctrl taint
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
      match subst_in_precondition ~inst_lval ~inst_ctrl taint with
      | None ->
          (* substitution made preconditon false, so no taint here! *)
          Taints.empty
      | Some taint -> Taints.singleton taint)
  (* Taint variables *)
  | Var _
  | Shape_var _
  | Control -> (
      match inst_taint_var taint with
      | None -> Taints.empty
      | Some (var_taints, _var_shape) ->
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
          var_taints
          |> Taints.map (fun taint' ->
                 {
                   taint' with
                   tokens = List.rev_append extra_tokens taint'.tokens;
                 }))

let instantiate_taints ~callee ~inst_lval ~inst_ctrl taints =
  taints |> Taints.elements
  |> List.fold_left
       (fun acc taint ->
         acc
         |> Taints.union (instantiate_taint ~callee ~inst_lval ~inst_ctrl taint))
       Taints.empty

let instantiate_shape ~callee ~inst_lval ~inst_ctrl shape =
  let inst_taints = instantiate_taints ~callee ~inst_lval ~inst_ctrl in
  let rec inst_shape = function
    | Bot -> Bot
    | Obj obj ->
        let obj =
          obj
          |> Fields.filter_map (fun _o cell ->
                 (* This is essentially a recursive call to 'instantiate_shape'!
                  * We rely on 'update_offset_in_cell' to maintain INVARIANT(cell). *)
                 update_offset_in_cell ~f:inst_xtaint [] cell)
        in
        if Fields.is_empty obj then Bot else Obj obj
    | Arg arg -> (
        match inst_lval (T.lval_of_arg arg) with
        | Some (_taints, shape) -> shape
        | None ->
            Log.warn (fun m ->
                m "Could not instantiate arg shape: %s" (T.show_arg arg));
            Arg arg)
  and inst_xtaint xtaint shape =
    (* This may break INVARIANT(cell) but 'update_offset_in_cell' will restore it. *)
    let xtaint =
      match xtaint with
      | `None
      | `Clean ->
          xtaint
      | `Tainted taints -> `Tainted (inst_taints taints)
    in
    let shape = inst_shape shape in
    (xtaint, shape)
  in
  inst_shape shape
