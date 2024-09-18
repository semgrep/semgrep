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
module T = Taint
module Taints = T.Taint_set
open Shape_and_sig.Shape
module Fields = Shape_and_sig.Fields
module Shape = Taint_shape
module Effect = Shape_and_sig.Effect
module Signature = Shape_and_sig.Signature
module Lval_env = Taint_lval_env

let sigs_tag = Log_tainting.sigs_tag
let bad_tag = Log_tainting.bad_tag

let ( let+ ) x f =
  match x with
  | None -> []
  | Some x -> f x

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
                   Shape.gather_all_taints_in_shape var_shape |> Taints.elements
               )
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
        |> Option.map (fun (_taints, shape) ->
               Shape.gather_all_taints_in_shape shape)
      in
      Some (taints, Bot)
  | Control ->
      (* 'Control' is pretty much like a taint variable so we handle all together. *)
      Some (inst_ctrl (), Bot)

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
                 Shape.update_offset_in_cell ~f:inst_xtaint [] cell)
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

let find_pos_in_actual_args ?(err_ctx = "???") args_taints fparams :
    T.arg -> _ option =
  let pos_args_taints, named_args_taints =
    List.partition_map
      IL.(
        function
        | Unnamed taints -> Left taints
        | Named (id, taints) -> Right (id, taints))
      args_taints
  in
  let named_arg_map =
    named_args_taints
    |> List.fold_left
         (fun xmap ((s, _), taint) -> SMap.add s taint xmap)
         SMap.empty
  in
  let name_to_taints = Hashtbl.create 10 in
  let idx_to_taints = Hashtbl.create 10 in
  (* We first process the named arguments, and then positional arguments.
   *)
  let remaining_params =
    (* Here, we take all the named arguments and remove them from the list of parameters.
     *)
    List_.fold_right
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
               Log.err (fun m ->
                   m
                     "More args to function than there are positional \
                      arguments in function signature (%s)"
                     err_ctx);
               (i + 1, [])
           | _ :: rest ->
               Hashtbl.add idx_to_taints i taints;
               (i + 1, rest))
         (0, remaining_params)
  in
  (* lookup function *)
  fun ({ name = s; index = i } : Taint.arg) ->
    let taint_opt =
      match
        (Hashtbl.find_opt name_to_taints s, Hashtbl.find_opt idx_to_taints i)
      with
      | Some taints, _ -> Some taints
      | _, Some taints -> Some taints
      | __else__ -> None
    in
    if Option.is_none taint_opt then
      Log.debug (fun m ->
          (* TODO: provide more context for debugging *)
          m ~tags:bad_tag
            "Cannot match taint variable with function arguments (%i: %s)" i s);
    taint_opt

(* TODO(shapes): This is needed for stuff that is not yet fully adapted to shapes
 *               such as records and dicts, it will be superseeded by 'taints_of_lval'.
 *
 * Given a function/method call 'fun_exp'('args_exps'), and an argument
 * spec 'sig_lval' from the taint signature of the called function/method,
 * determine what lvalue corresponds to 'sig_lval'.
 *
 * In the simplest case this just obtains the actual argument:
 * E.g. `lval_of_sig_lval f [x;y;z] [a;b;c] (x,0) = a`
 *
 * The 'sig_lval' may refer to `this` and also have an offset:
 * E.g. `lval_of_sig_lval o.f [] [] (this,-1).x = o.x`
 *)
let lval_of_sig_lval (fun_exp : IL.exp) fparams args_exps (sig_lval : T.lval) :
    (* Besides the 'lval', we also return a "tainted token" pointing to an
     * identifier in the actual code that relates to 'sig_lval', to be used
     * in the taint trace.  For example, if we're calling `obj.method` and
     * `this.x` were tainted, then we would record that taint went through
     * `obj`. *)
    (IL.lval * T.tainted_token) option =
  let open IL in
  let ( let* ) opt f =
    match opt with
    | None -> None
    | Some x -> (
        match f x with
        | None ->
            Log.err (fun m ->
                m "Could not instantiate taint signature of %s: %s"
                  (Display_IL.string_of_exp fun_exp)
                  (T.show_lval sig_lval));
            None
        | Some r -> Some r)
  in
  let* rev_offset = T.rev_IL_offset_of_offset sig_lval.offset in
  let* lval, obj =
    match sig_lval.base with
    | BGlob gvar -> Some ({ base = Var gvar; rev_offset }, gvar)
    | BThis -> (
        (* For the call trace, we try to record variables that correspond to objects,
         * but if not possible then we record method names. *)
        match fun_exp with
        | {
         e = Fetch { base = Var obj; rev_offset = [ { o = Dot _method; _ } ] };
         _;
        } ->
            (* We're calling `obj.method(...)`, so `this.x` is actually `obj.x`. *)
            Some ({ base = Var obj; rev_offset }, obj)
        | { e = Fetch { base = Var method_; rev_offset = [] }; _ } ->
            (* We're calling a `method(...)` on the same instace of the caller,
             * and `this.x` is just `this.x`. *)
            let this =
              VarSpecial (This, Tok.fake_tok (snd method_.ident) "this")
            in
            Some ({ base = this; rev_offset }, method_)
        | {
         e =
           Fetch
             {
               base;
               rev_offset = { o = Dot method_; _ } :: exp_obj_rev_offset';
             };
         _;
        } ->
            (* We're calling e.g. `this.obj.method(...)`,
             * so `this.x` is actually `this.obj.x`. *)
            Some
              ({ base; rev_offset = rev_offset @ exp_obj_rev_offset' }, method_)
        | __else__ -> None)
    | BArg pos -> (
        let* arg_exp =
          find_pos_in_actual_args
            ~err_ctx:(Display_IL.string_of_exp fun_exp)
            args_exps fparams pos
        in
        match (arg_exp.e, sig_lval.offset) with
        | Fetch ({ base = Var obj; _ } as arg_lval), _ ->
            let lval =
              { arg_lval with rev_offset = rev_offset @ arg_lval.rev_offset }
            in
            Some (lval, obj)
        | RecordOrDict fields, [ Ofld o ] -> (
            (* JS: The argument of a function call may be a record expression such as
             * `{x="tainted", y="safe"}`, if 'sig_lval' refers to the `x` field then
             * we want to resolve it to `"tainted"`. *)
            match
              fields
              |> List.find_opt (function
                   (* The 'o' is the offset that 'sig_lval' is referring to, here
                    * we look for a `fld=lval` field in the record object such that
                    * 'fld' has the same name as 'o'. *)
                   | Field (fld, _) -> fst fld = fst o.ident
                   | Entry _
                   | Spread _ ->
                       false)
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
  Some (lval, snd obj.ident)

(* HACK(implicit-taint-variables-in-env):
 * We have a function call with a taint variable, corresponding to a global or
 * a field in the same class as the caller, that reaches a sink. However, in
 * the caller we have no taint for the corresponding l-value.
 *
 * Why?
 * In 'find_instance_and_global_variables_in_fdef' we only add to the input-env
 * those globals and fields that occur in the  definition of a method, but just
 * because a global/field is not in there, it does not mean it's not in scope!
 *
 * What to do?
 * We can just propagate the very same taint variable, assuming that it is
 * implicitly in scope.
 *
 * Example (see SAF-1059):
 *
 *     string bad;
 *
 *     void test() {
 *         bad = "taint";
 *         // Thanks to this HACK we will know that calling 'foo'
 *         // here makes "taint" go into a sink.
 *         foo();
 *     }
 *
 *     void foo() {
 *         // We instantiate `bar` and we see 'bad ~~~> sink',
 *         // but `bad` is not in the environment, however we
 *         // know `bad` is a field in the same class as `foo`,
 *         // so we propagate it as-is.
 *         bar();
 *     }
 *
 *     // signature: bad ~~~> sink
 *     void bar() {
 *         sink(bad);
 *     }
 *
 * ALTERNATIVE:
 * In 'Deep_tainting.infer_taint_sigs_of_fdef', when we build
 * the taint input-env, we could collect all the globals and
 * class fields in scope, regardless of whether they occur or
 * not in the method definition. Main concern here is whether
 * input environments could end up being too big.
 *)
let fix_lval_taints_if_global_or_a_field_of_this_class (fun_exp : IL.exp)
    (lval : T.lval) lval_taints =
  let is_method_in_this_class =
    match fun_exp with
    | { e = Fetch { base = Var _method; rev_offset = [] }; _ } ->
        (* We're calling a `method` on the same instance of the caller,
           so `this.x` in the taint signature of the callee corresponds to
           `this.x` in the caller. *)
        true
    | __else__ -> false
  in
  match lval.base with
  | BArg _ -> lval_taints
  | BThis when not is_method_in_this_class -> lval_taints
  | BGlob _
  | BThis
    when not (Taints.is_empty lval_taints) ->
      lval_taints
  | BGlob _
  | BThis ->
      (* 'lval' is either a global variable or a field in the same class
       * as the caller of 'fun_exp', and no taints are found for 'lval':
       * we assume 'lval' is implicitly in the input-environment and
       * return it as a type variable. *)
      Taints.singleton { orig = Var lval; tokens = [] }

let taints_of_lval lval_env fparams (fun_exp : IL.exp) args_taints lval :
    (Taints.t * shape) option =
  let { T.base; offset } = lval in
  let* base, offset =
    match base with
    | T.BArg pos -> Some (`Arg pos, offset)
    | BThis -> (
        match fun_exp with
        | {
         e = Fetch { base = Var obj; rev_offset = [ { o = Dot _method; _ } ] };
         _;
        } ->
            (* We're calling `obj.method`, so `this.x` is actually `obj.x` *)
            Some (`Var obj, offset)
        | { e = Fetch { base = Var _method; rev_offset = [] }; _ } -> (
            (* We're calling a `method` on the same instace of the caller,
             * and `this.x.y` is just `x.y` *)
            (* TODO: We should track `this` in `Lval_env` rather than doing this hack. *)
            match offset with
            | [] -> None
            | Ofld var :: offset -> Some (`Var var, offset)
            | (Oint _ | Ostr _ | Oany) :: _ -> None)
        | __else__ -> None)
    | BGlob var -> Some (`Var var, offset)
  in
  let* base_taints, base_shape =
    match base with
    | `Arg pos ->
        find_pos_in_actual_args
          ~err_ctx:(Display_IL.string_of_exp fun_exp)
          args_taints fparams pos
    | `Var var ->
        let* (Cell (xtaints, shape)) = Lval_env.find_var lval_env var in
        Some (Xtaint.to_taints xtaints, shape)
  in
  match (base_shape, offset) with
  | base_shape, [] -> Some (base_taints, base_shape)
  | Bot, _ :: _ -> None
  | base_shape, _ :: _ ->
      let* (Cell (xtaints, shape)) = Shape.find_in_shape offset base_shape in
      Some (Xtaint.to_taints xtaints, shape)

(* What is the taint denoted by 'sig_lval' ? *)
let taints_of_sig_lval lval_env ~check_lval fparams fun_exp args_exps
    (args_taints : (Taints.t * shape) IL.argument list) (sig_lval : T.lval) =
  match taints_of_lval lval_env fparams fun_exp args_taints sig_lval with
  | Some (taints, shape) -> Some (taints, shape)
  | None ->
      (* We want to know what's the taint carried by 'arg_exp.x1. ... .xN'.
       * TODO: We should not need this when we cover everything with shapes,
       *   see 'lval_of_sig_lval'.
       *)
      let* lval, _obj = lval_of_sig_lval fun_exp fparams args_exps sig_lval in
      let lval_taints, shape = check_lval lval in
      let lval_taints =
        lval_taints
        |> fix_lval_taints_if_global_or_a_field_of_this_class fun_exp sig_lval
      in
      Some (lval_taints, shape)

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any effects that occur within the function due to taints being
      input into the function body, from the calling context?
*)
let instantiate_function_signature lval_env ~check_lval fparams fun_sig fun_exp
    fun_eorig args (args_taints : (Taints.t * shape) IL.argument list) : _ =
  (* This function simply produces the corresponding taints to the
      given argument, within the body of the function.
  *)
  (* Our first pass will be to substitute the args for taints.
     We can't do this indiscriminately at the beginning, because
     we might need to use some of the information of the pre-substitution
     taints and the post-substitution taints, for instance the tokens.

     So we will isolate this as a specific step to be applied as necessary.
  *)
  let lval_to_taints lval =
    let opt_taints_shape =
      taints_of_sig_lval lval_env ~check_lval fparams fun_exp args args_taints
        lval
    in
    Log.debug (fun m ->
        m ~tags:sigs_tag "Instantiating taint signature of %s: %s -> %s"
          (Display_IL.string_of_exp fun_exp)
          (T.show_lval lval)
          (match opt_taints_shape with
          | None -> "nothing :/"
          | Some (taints, shape) ->
              spf "%s & %s" (T.show_taints taints) (show_shape shape)));
    opt_taints_shape
  in
  let taints_in_ctrl () = Lval_env.get_control_taints lval_env in
  let inst_taint_var taint =
    instantiate_taint_var ~inst_lval:lval_to_taints ~inst_ctrl:taints_in_ctrl
      taint
  in
  let subst_in_precondition =
    subst_in_precondition ~inst_lval:lval_to_taints ~inst_ctrl:taints_in_ctrl
  in
  let process_sig : Effect.t -> _ list = function
    | Effect.ToReturn { data_taints; data_shape; control_taints; return_tok } ->
        let inst_taints taints =
          taints
          |> List.fold_left
               (fun return_taints (t : T.taint) ->
                 let taints' =
                   (* TODO: Use 'Taint_inst.instantiate_taint' also for 'ToSink' and
                            'ToLval' cases below. *)
                   instantiate_taint ~callee:fun_exp ~inst_lval:lval_to_taints
                     ~inst_ctrl:taints_in_ctrl t
                 in
                 return_taints |> Taints.union taints')
               Taints.empty
        in
        let data_taints = inst_taints data_taints in
        let data_shape =
          instantiate_shape ~callee:fun_exp ~inst_lval:lval_to_taints
            ~inst_ctrl:taints_in_ctrl data_shape
        in
        let control_taints =
          (* No need to instantiate 'control_taints' because control taint variables
           * do not propagate through function calls... BUT instantiation also fixes
           * the call trace! *)
          inst_taints control_taints
        in
        if
          Shape.taints_and_shape_are_relevant data_taints data_shape
          || not (Taints.is_empty control_taints)
        then [ `ToReturn (data_taints, data_shape, control_taints, return_tok) ]
        else []
    | Effect.ToSink { taints_with_precondition = taints, _requires; sink; _ } ->
        let incoming_taints =
          taints
          |> List.concat_map (fun { Effect.taint; sink_trace } ->
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
                        previous `effects_of_tainted_sink` call would have already
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
                     let+ taint = taint |> subst_in_precondition in
                     [ { Effect.taint; sink_trace } ]
                 | Var _
                 | Shape_var _
                 | Control ->
                     let sink_trace =
                       T.Call (fun_eorig, taint.tokens, sink_trace)
                     in
                     let+ var_taints, var_shape = inst_taint_var taint in
                     (* See NOTE(gather-all-taints) *)
                     let var_taints =
                       var_taints
                       |> Taints.union
                            (Shape.gather_all_taints_in_shape var_shape)
                     in
                     Taints.elements var_taints
                     |> List_.map (fun x -> { Effect.taint = x; sink_trace }))
        in
        if List_.null incoming_taints then []
        else [ `ToSink (incoming_taints, sink) ]
    | Effect.ToLval (taints, dst_sig_lval) ->
        (* Taints 'taints' go into an argument of the call, by side-effect.
         * Right now this is mainly used to track taint going into specific
         * fields of the callee object, like `this.x = "tainted"`. *)
        let+ dst_lval, tainted_tok =
          (* 'dst_lval' is the actual argument/l-value that corresponds
             * to the formal argument 'dst_sig_lval'. *)
          lval_of_sig_lval fun_exp fparams args dst_sig_lval
        in
        taints
        |> List.concat_map (fun t ->
               let dst_taints =
                 match t.T.orig with
                 | Src src -> (
                     let call_trace =
                       T.Call (fun_eorig, t.tokens, src.call_trace)
                     in
                     let t =
                       { Taint.orig = Src { src with call_trace }; tokens = [] }
                     in
                     match t |> subst_in_precondition with
                     | None -> Taints.empty
                     | Some t -> Taints.singleton t)
                 | Var _
                 | Shape_var _ -> (
                     (* Taint is flowing from one argument to another argument
                      * (or possibly the callee object). Given the formal poly
                      * taint 'src_lval', we compute the actual taint in the
                      * context of this function call. *)
                     match inst_taint_var t with
                     | None -> Taints.empty
                     | Some (res, _TODOshape) ->
                         res
                         |> Taints.map (fun taint ->
                                let tokens =
                                  t.tokens @ (tainted_tok :: taint.T.tokens)
                                in
                                { taint with tokens }))
                 | Control ->
                     (* control taints do not propagate to arguments *)
                     Taints.empty
               in
               if Taints.is_empty dst_taints then []
               else [ `ToLval (dst_taints, dst_lval) ])
  in
  Some (fun_sig |> Signature.elements |> List.concat_map process_sig)
