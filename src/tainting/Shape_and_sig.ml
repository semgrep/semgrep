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

(** Taint types. *)

open Common
module R = Rule
module T = Taint

(*****************************************************************************)
(* Taint shapes *)
(*****************************************************************************)

module Fields = Map.Make (struct
  type t = T.offset

  let compare o1 o2 = T.compare_offset o1 o2
end)

(** A shape approximates an object or data structure, and tracks the taint
 * associated with its fields and indexes.
 *
 * Taint shapes are a bit like types. Right now this is mainly to support
 * field- and index-sensitivity, but hapes also provide a good foundation to
 * later add alias analysis.  This is somewhat inspired by
 *
 *     "Polymorphic type, region and effect inference"
 *     by Jean-Pierre Talpin and Pierre Jouvelot
 *
 * History
 * -------
 * Previously, we had a flat environment from l-values to their taint, and we had
 * to "reconstruct" the shape of objects when needed. For example, to check if a
 * variable was a struct, we looked for l-values in the environment that were an
 * "extension" of that variable. By recording shapes explicitly, implementing
 * field-sensitivity becomes more natural.
 *
 * Example
 * -------
 * For example, a record expression `{ a: "taint", b: "safe" }` would have
 * the shape `Obj { .a -> Cell({"taint"}, _|_) }`, recording that the field `a`
 * is tainted by the string literal `"taint"`. A field like '.a' (the dot '.'
 * indicates that it's a field) or an index like '[0]' will always have a 'cell'
 * shape, because they denote l-values. The first argument of a 'Cell' is its
 * xtaint or "taint status" (see 'Xtaint.t'). For each field and index, we track
 * its xtaint individually (field- and index-sensitivity). Field '.a' in
 * `Obj { .a -> Cell({"taint"}, _|_) }` has the the taint set {"taint"} attached.
 * The second argument of 'Cell' is the shape of the objects stored in that cell.
 * The shape of field '.a' is '_|_' ("bottom") which is given to primitive types,
 * or whenever we "don't care" (or to act as "to-do" as well).
 *
 * TODO: Add 'Ptr' shapes and track aliasing.
 *)
module rec Shape : sig
  type shape =
    | Bot  (** _|_, don't know or don't care *)
    | Obj of obj
        (** An "object" or struct-like thing.
          *
          * Tuples or lists are also represented by 'Obj' shapes! We just treat
          * constant indexes as if they were fields, and use 'Oany' to capture the
          * non-constant indexes.
          *)
    | Arg of Taint.arg
        (** Represents the yet-unknown shape of a function/method parameter. It is
          * a polymorphic shape variable that is meant to be instantiated at call
          * site. Before adding 'Arg' we assumed parameters had shape 'Bot', and
          * 'Arg' still acts like 'Bot' in some places. *)

  and cell =
    | Cell of Xtaint.t * shape
        (** A cell or "reference" represents the "storage" of a value, like
          * a variable in C.
          *
          * A cell may be explicitly tainted ('`Tainted'), not explicitly tainted
          * ('`None' / "0"),  or explicitly clean ('`Clean' / "C").
          *
          * A cell that is not explicitly tainted inherits any taints from "parent"
          * refs. A cell that is explicitly clean it is clean regardless.
          *
          * For example, given a variable `x` and the following statements:
          *
          *     x.a := "taint";
          *     x.a.u := "clean";
          *
          * We could assign the following shape to `x`:
          *
          *     Cell(`None, Obj {
          *             .a -> Cell({"taint"}, Obj {
          *                     .u -> Cell(`Clean, _|_)
          *                     })
          *             })
          *
          * We have that `x` itself has no taint directly assigned to it, but `x.a` is
          * tainted (by the string `"taint"`). Other fields like `x.b` are not tainted.
          * When it comes to `x.a`, we have that `x.a.u` has been explicitly marked clean,
          * so `x.a.u` will be considered clean despite `x.a` being tainted. Any other field
          * of `x.a` such as `x.a.v` will inherit the same taint as `x.a`.
          *
          * INVARIANT(cell): To keep shapes minimal:
          *   1. If the xtaint is '`None', then the shape is not 'Bot' and we can reach
          *      another 'cell' whose xtaint is either '`Tainted' or '`Clean'.
          *   2. If the xtaint is '`Clean', then the shape is 'Bot'.
          *      (If we add aliasing we may need to revisit this, and instead just mark
          *       every reachable 'cell' as clean too.)
          *
          * TODO: We can attach "region ids" to refs and assign taints to regions rather than
          *   to refs directly, then we can have alias analysis.
          *)

  and obj = cell Fields.t
  (**
      * This a mapping from a 'Taint.offset' to a shape 'cell'.
      *
      * If an 'Obj' shape tracks an 'Oany' offset (an arbitrary index,
      * see 'Taint.offset'), then the taint and shape given to 'Oany' would
      * also be the taint and shape given to any field that is not being
      * explicitly tracked. If there is no 'Oany' in the 'Obj' shape, then a
      * field that is not explicitly tracked would just have an arbitrary or
      * "don't care" shape, and the taint that it inherits from its "parent"
      * 'cell's.
      *
      * THINK: Instead of 'Oany' maybe have an explicit field ?
      *
      * For example, given the assignment `x = { a: "taint", b: "safe" }`,
      * the shape of `x` would be `Cell(`None, Obj { .a -> Cell({"taint"}, _|_) })`.
      * The field `b` is omitted in the shape, and if we ask for it's taint and
      * shape we would get the empty taint set (because `x`'s outermost 'Cell'
      * has no taint), and the shape '_|_' because, given that we are not
      * tracking `b`, it means we don't care about it's shape. In a shape like
      * `{ [*] -> Cell({"taint"}, _|_) }}` where `[*]` denotes 'Oany', the taint
      * and shape  of any concrete index would be given by the taint and shape
      * of '[*]'.
      *)

  val equal_cell : cell -> cell -> bool
  val compare_shape : shape -> shape -> int
  val show_cell : cell -> string
  val show_shape : shape -> string
end = struct
  type shape = Bot | Obj of obj | Arg of T.arg
  and cell = Cell of Xtaint.t * shape
  and obj = cell Fields.t

  (*************************************)
  (* Equality *)
  (*************************************)
  (* TODO: Should we just define these in terms of `compare_*` ? *)

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

  (*************************************)
  (* Comparison *)
  (*************************************)

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
    | Bot, (Obj _ | Arg _)
    | Obj _, Arg _ ->
        -1
    | Obj _, Bot
    | Arg _, (Bot | Obj _) ->
        1

  and compare_obj obj1 obj2 = Fields.compare compare_cell obj1 obj2

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

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
end

(*****************************************************************************)
(* Taint results & signatures *)
(*****************************************************************************)
and Effect : sig
  type sink = { pm : Pattern_match.t; rule_sink : R.taint_sink }
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

  type taints_to_sink = {
    taints_with_precondition : taint_to_sink_item list * Rule.precondition;
        (** Taints reaching the sink and the precondition for the sink to apply. *)
    sink : sink;
    merged_env : Metavariable.bindings;
        (** The metavariable environment that results of merging the environment from
    * matching the source and the one from matching the sink. *)
  }

  type taints_to_return = {
    data_taints : Taint.taint list;
        (** The taints of the data being returned (typical data propagated via data flow). *)
    data_shape : Shape.shape;  (** The shape of the data being returned. *)
    control_taints : Taint.taint list;
        (** The taints propagated via the control flow (cf., `control: true` sources)
   * used for reachability queries. *)
    return_tok : AST_generic.tok;
  }

  (** Function-level result.
  *
  * 'ToSink' results where a taint source reaches a sink are candidates for
  * actual Semgrep findings, although some may be dropped by deduplication.
  *
  * Results are computed for each function/method definition, and formulated
  * using 'lval' taints to act as placeholders of the taint that may be passed
  * by an arbitrary caller via the function arguments. Thus the results are
  * polymorphic/context-sensitive, as the 'lval' taints can be instantiated
  * accordingly at each call site.
  *)
  type t =
    | ToSink of taints_to_sink
        (** Taints reach a sink.
        *
        * For example:
        *
        *     def foo(x):
        *         y = x
        *         sink(y)
        *
        * The parameter `x` could be tainted depending on the calling context,
        * so we infer:
        *
        *     ToSink { taints_with_precondition = (["taint"], PBool true);
        *              sink = "sink(y)";
        *              ... }
        *)
    | ToReturn of taints_to_return
        (** Taints reach a `return` statement.
        *
        * For example:
        *
        *     def foo():
        *         x = "taint"
        *         return x
        *
        * We infer:
        *
        *     ToReturn(["taint"], Bot, ...)
        *)
    | ToLval of Taint.taint list * Taint.lval
        (** Taints reach an l-value in the scope of the function/method.
        *
        * For example:
        *
        *     x = ["ok"]
        *
        *     def foo():
        *         global x
        *         x[0] = "taint"
        *
        * We infer:
        *
        *     ToLval(["taint"], "x[0]")
        *
        * TODO: Record taint shapes.
        *)

  val compare : t -> t -> int
  val show : t -> string
end = struct
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
    data_shape : Shape.shape;
    control_taints : Taint.taint list;
    return_tok : AST_generic.tok;
  }

  type t =
    | ToSink of taints_to_sink
    | ToReturn of taints_to_return
    | ToLval of T.taint list * T.lval (* TODO: CleanArg ? *)

  (*************************************)
  (* Comparison *)
  (*************************************)

  let compare_sink { pm = pm1; rule_sink = sink1 }
      { pm = pm2; rule_sink = sink2 } =
    match String.compare sink1.Rule.sink_id sink2.Rule.sink_id with
    | 0 -> T.compare_matches pm1 pm2
    | other -> other

  let compare_taint_to_sink_item { taint = taint1; sink_trace = _ }
      { taint = taint2; sink_trace = _ } =
    T.compare_taint taint1 taint2

  let compare_taints_to_sink
      {
        taints_with_precondition = ttsis1, pre1;
        sink = sink1;
        merged_env = env1;
      }
      {
        taints_with_precondition = ttsis2, pre2;
        sink = sink2;
        merged_env = env2;
      } =
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
        match Shape.compare_shape data_shape1 data_shape2 with
        | 0 -> List.compare T.compare_taint control_taints1 control_taints2
        | other -> other)
    | other -> other

  let compare r1 r2 =
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

  (*************************************)
  (* Pretty-printing *)
  (*************************************)

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

  let show = function
    | ToSink x -> show_taints_to_sink x
    | ToReturn { data_taints; data_shape; control_taints; return_tok = _ } ->
        Printf.sprintf "return (%s & %s & CTRL:%s)"
          (Common2.string_of_list T.show_taint data_taints)
          (Shape.show_shape data_shape)
          (Common2.string_of_list T.show_taint control_taints)
    | ToLval (taints, lval) ->
        Printf.sprintf "%s ----> %s"
          (Common2.string_of_list T.show_taint taints)
          (T.show_lval lval)
end

(** A (polymorphic) taint signature: simply a set of results for a function.
 *
 * Note that this signature is polymorphic/context-sensitive given that the
 * potential taints coming into the function via its arguments are represented
 * by 'lval' taints, that can be instantiated as needed.
 *
 * For example given:
 *
 *     def foo(x):
 *         sink(x.a)
 *
 * We infer the signature (simplified):
 *
 *     {ToSink {taints_with_precondition = [(x#0).a]; sink = ... ; ...}}
 *
 * where '(x#0).a' is taint variable that denotes the taint of the offset `.a`
 * of the parameter `x` (where '#0' means it is the first argument) of `foo`.
 * The signature tells us that '(x#0).a' will reach a sink.
 *
 * Given a concrete call `foo(obj)`, Semgrep will instantiate this signature with
 * taint assigned to `obj.a` in that calling context. If it is tainted, then
 * Semgrep will report a finding.
 *
 * Also note that, within each function, if there are multiple paths through
 * which a taint source may reach a sink, we do not keep all of them but only
 * the shortest one.
 *
 * THINK: Could we have a "taint shape" for functions/methods ?
 *)
and Signature : sig
  include Set.S with type elt = Effect.t

  val show : t -> string
end = struct
  include Set.Make (struct
    type t = Effect.t

    let compare r1 r2 = Effect.compare r1 r2
  end)

  let show s =
    s |> to_seq |> List.of_seq |> List_.map Effect.show |> String.concat " + "
end

module Effects_tbl = Hashtbl.Make (struct
  type t = Effect.t

  let equal r1 r2 = Effect.compare r1 r2 =|= 0
  let hash r = Hashtbl.hash r
end)
