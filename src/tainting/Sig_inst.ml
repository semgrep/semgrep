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
module Effects = Shape_and_sig.Effects
module Signature = Shape_and_sig.Signature
module Lval_env = Taint_lval_env

let sigs_tag = Log_tainting.sigs_tag
let bad_tag = Log_tainting.bad_tag

(*****************************************************************************)
(* Call effets *)
(*****************************************************************************)

type call_effect =
  | ToSink of Effect.taints_to_sink
  | ToReturn of Effect.taints_to_return
  | ToLval of Taint.taints * IL.name * Taint.offset list

type call_effects = call_effect list

let show_call_effect = function
  | ToSink tts -> Effect.show_taints_to_sink tts
  | ToReturn ttr -> Effect.show_taints_to_return ttr
  | ToLval (taints, var, offset) ->
      Printf.sprintf "%s ----> %s%s" (T.show_taints taints) (IL.str_of_name var)
        (T.show_offset_list offset)

let show_call_effects call_effects =
  call_effects |> List_.map show_call_effect |> String.concat "; "

(*****************************************************************************)
(* Instantiation "config" *)
(*****************************************************************************)

type inst_var = {
  inst_lval : T.lval -> (Taints.t * shape) option;
      (** How to instantiate a 'Taint.lval', aka "data taint variable". *)
  inst_ctrl : unit -> Taints.t;
      (** How to instantiate a 'Taint.Control', aka "control taint variable". *)
}

(* TODO: Right now this is only for source traces, not for sink traces...
 * In fact, we should probably not have two traces but just one, but more
 * general. *)
type inst_trace = {
  add_call_to_trace_for_src :
    tainted_tokens:T.tainted_tokens ->
    Rule.taint_source T.call_trace ->
    Rule.taint_source T.call_trace option;
      (** For sources we extend the call trace. *)
  fix_token_trace_for_var :
    var_rev_tokens:T.rev_tainted_tokens -> T.rev_tainted_tokens -> Tok.t list;
      (** For variables we should too, but due to limitations in our call-trace
   * representation, we just record the path as tainted tokens. *)
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let+ ) x f =
  match x with
  | None -> []
  | Some x -> f x

(*****************************************************************************)
(* Instantiating traces *)
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

let add_call_to_trace_if_callee_has_eorig ~callee ~tainted_tokens call_trace =
  (* E.g. (ToReturn) the call to 'bar' in:
   *
   *     1 def bar():
   *     2     x = taint
   *     3     return x
   *     4
   *     5 def foo():
   *     6     y = bar()
   *     7     sink(y)
   *
   * would result in this call trace for the source:
   *
   *     Call('bar' @l.6, ["x" @l.2], "taint" @l.2)
   *
   * E.g. (ToLval) the call to 'bar' in:
   *
   *     1 s = set([])
   *     2
   *     3 def bar():
   *     4    global s
   *     5    s.add(taint)
   *     6
   *     7 def foo():
   *     8    global s
   *     9    bar()
   *    10    sink(s)
   *
   * would result in this call trace for the source:
   *
   *     Call('bar' @l.6, ["s" @l.5], "taint" @l.5)
   *)
  match callee with
  | { IL.e = _; eorig = SameAs orig_callee } ->
      Some (T.Call (orig_callee, tainted_tokens, call_trace))
  | __else__ ->
      (* TODO: Have a better fallback in case we can't get an eorig from 'callee',
       * maybe for that we need to change `Taint.Call` to accept a token. *)
      None

let add_call_to_token_trace ~callee ~var_rev_tokens caller_rev_tokens =
  (* E.g. (ToReturn) the call to 'bar' in:
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
  let call_tokens =
    (match get_ident_of_callee callee with
    | None -> []
    | Some ident -> [ snd ident ])
    @ List.rev var_rev_tokens
  in
  List.rev_append call_tokens caller_rev_tokens

let add_lval_update_to_token_trace ~callee:_TODO lval_tok ~var_rev_tokens
    caller_rev_tokens =
  (* E.g. (ToLval) the call to 'bar' in:
   *
   *     1 s = set([])
   *     2
   *     3 def bar(x):
   *     4    global s
   *     5    s.add(x)
   *     6
   *     7 def foo():
   *     8    global s
   *     9    t = taint
   *    10    bar(t)
   *    11    sink(s)
   *
   * would result in this list of tokens (note that is reversed):
   *
   *     ["s" @l.5; "s" @l.5; "x" @l.3; "s" @l.5; "bar" @l.10; "t" @l.9]
   *
   * This is a hack we use because taint traces aren't general enough,
   * this should be represented with a call trace.
   *)
  let call_tokens =
    (* TODO: Use `get_ident_of_callee callee` to add the callee to the trace. *)
    lval_tok :: List.rev var_rev_tokens
  in
  List.rev_append call_tokens caller_rev_tokens

(*****************************************************************************)
(* Instatiation *)
(*****************************************************************************)

let subst_in_precondition inst_var taint =
  let subst taints =
    taints
    |> List.concat_map (fun t ->
           match t.T.orig with
           | Src _ -> [ t ]
           | Var lval -> (
               match inst_var.inst_lval lval with
               | None -> [ t ]
               | Some (call_taints, _call_shape) ->
                   call_taints |> Taints.elements)
           | Shape_var lval -> (
               match inst_var.inst_lval lval with
               | None -> [ t ]
               | Some (_call_taints, call_shape) ->
                   (* Taint shape-variable, stands for the taints reachable
                    * through the shape of the 'lval', it's like a delayed
                    * call to 'Shape.gather_all_taints_in_shape'. *)
                   Shape.gather_all_taints_in_shape call_shape
                   |> Taints.elements)
           | Control -> inst_var.inst_ctrl () |> Taints.elements)
  in
  T.map_preconditions subst taint

let instantiate_taint_var inst_var taint =
  match taint.T.orig with
  | Src _ -> None
  | Var lval -> inst_var.inst_lval lval
  | Shape_var lval ->
      (* This is just a delayed 'gather_all_taints_in_shape'. *)
      let* taints =
        inst_var.inst_lval lval
        |> Option.map (fun (_taints, shape) ->
               Shape.gather_all_taints_in_shape shape)
      in
      Some (taints, Bot)
  | Control ->
      (* 'Control' is pretty much like a taint variable so we handle all together. *)
      Some (inst_var.inst_ctrl (), Bot)

let instantiate_taint inst_var inst_trace taint =
  let inst_taint_var taint = instantiate_taint_var inst_var taint in
  match taint.T.orig with
  | Src src -> (
      let taint =
        match
          inst_trace.add_call_to_trace_for_src
            ~tainted_tokens:(List.rev taint.rev_tokens)
            src.call_trace
        with
        | Some call_trace ->
            { T.orig = Src { src with call_trace }; rev_tokens = [] }
        | None -> taint
      in
      match subst_in_precondition inst_var taint with
      | None ->
          (* substitution made preconditon false, so no taint here! *)
          Taints.empty
      | Some taint -> Taints.singleton taint)
  (* Taint variables *)
  | Var _
  | Shape_var _
  | Control -> (
      match inst_taint_var taint with
      | None -> Taints.singleton taint
      | Some (call_taints, _Bot_shape) ->
          call_taints
          |> Taints.map (fun taint' ->
                 {
                   taint' with
                   rev_tokens =
                     inst_trace.fix_token_trace_for_var
                       ~var_rev_tokens:taint.rev_tokens taint'.rev_tokens;
                 }))

let instantiate_taints inst_var inst_trace taints =
  taints |> Taints.to_seq
  |> Seq.fold_left
       (fun acc taint ->
         acc |> Taints.union (instantiate_taint inst_var inst_trace taint))
       Taints.empty

let instantiate_shape inst_var inst_trace shape =
  let inst_taints = instantiate_taints inst_var inst_trace in
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
        match inst_var.inst_lval (T.lval_of_arg arg) with
        | Some (_taints, shape) -> shape
        | None ->
            Log.warn (fun m ->
                m "Could not instantiate arg shape: %s" (T.show_arg arg));
            Arg arg)
    | Fun _ as funTODO ->
        (* Right now a function shape can only come from a top-level function,
         * whose shape will not depend on the parameters of another encloding
         * function, so we shouldn't have to instantiate anything here, e.g.:
         *
         *     def bar():
         *       ...
         *
         *     def foo(x):
         *       return bar
         *
         * When instantiating a call like `foo(1)`, the shape of `bar` (that is,
         * its signature) in `return bar` does not depend on `x` at all. (If the
         * function is applied, then its signature will be instantiated as usual.)
         *
         * This will change when we start giving taint signatures to lambdas,
         * as they can capture variables from their enclosing function, so when
         * instantiating the enclosing function we also need to instantiate the
         * shape of the lambda, e.g.:
         *
         *     def foo(x):
         *       return (lambda y: x)
         *
         *)
        funTODO
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
      (fun (param : Signature.param) acc ->
        match param with
        | P n -> (
            match SMap.find_opt (fst n.ident) named_arg_map with
            | Some taints ->
                (* If this parameter is one of our arguments, insert a mapping and then remove it
                   from the list of remaining parameters.*)
                Hashtbl.add name_to_taints n taints;
                acc
                (* Otherwise, it has not been consumed, so keep it in the remaining parameters.*)
            | None -> param :: acc (* Same as above. *))
        | Other -> param :: acc)
      fparams []
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
  fun ({ name; index = i } : Taint.arg) ->
    let taint_opt =
      match
        (Hashtbl.find_opt name_to_taints name, Hashtbl.find_opt idx_to_taints i)
      with
      | Some taints, _ -> Some taints
      | _, Some taints -> Some taints
      | __else__ -> None
    in
    if Option.is_none taint_opt then
      Log.debug (fun m ->
          (* TODO: provide more context for debugging *)
          m ~tags:bad_tag
            "Cannot match taint variable with function arguments (%i: %s)" i
            (IL.str_of_name name));
    taint_opt

(* Given a function/method call 'fun_exp'('args_exps'), and a taint variable 'tlval'
    from the taint signature of the called function/method 'fun_exp', we want to
   determine the actual l-value that corresponds to 'lval' in the caller's context.

    The return value is a triplet '(variable, offset, token)', where 'token' is to
    be added to the taint trace, and it may even be the token of 'variable'.
    For example, if we are calling `obj.method` and `this.x` were tainted, then we
    would record that taint went through `obj`.

    TODO(shapes): This is needed for stuff that is not yet fully adapted to shapes,
             in theory we should only need 'instantiate_lval_using_shape'.
*)
let instantiate_lval_using_actual_exps (fun_exp : IL.exp) fparams args_exps
    (tlval : T.lval) : (IL.name * T.offset list * T.tainted_token) option =
  (* Error handling  *)
  let log_error () =
    Log.err (fun m ->
        m "instantiate_lval_using_actual_exps FAILED: %s(...): %s"
          (Display_IL.string_of_exp fun_exp)
          (T.show_lval tlval))
  in
  let ( let* ) opt f =
    match opt with
    | None -> None
    | Some x -> (
        match f x with
        | None ->
            log_error ();
            None
        | Some r -> Some r)
  in
  match tlval.base with
  | BVar gvar -> Some (gvar, tlval.offset, snd gvar.ident)
  | BArg pos -> (
      (*
          An actual argument from 'args_exps', e.g.

              instantiate_lval_using_actual_exps f [x;y;z] [a.q;b;c]
                                { base = BArg {name = "x"; index = 0}; offset = [.u] }
              = (a, [.q.u], tok)
        *)
      let* args_exps = args_exps in
      let* (arg_exp : IL.exp) =
        find_pos_in_actual_args
          ~err_ctx:(Display_IL.string_of_exp fun_exp)
          args_exps fparams pos
      in
      match (arg_exp.e, tlval.offset) with
      | Fetch ({ base = Var obj; _ } as arg_lval), _ ->
          let* var, offset = Lval_env.normalize_lval arg_lval in
          Some (var, offset @ tlval.offset, snd obj.ident)
      | __else__ -> None)
  | BThis -> (
      (*
          A field of the callee object, e.g.:

              instantiate_lval_using_actual_exps o.f [] []
                                { base = BThis; offset = [.x] }
              = (o, [.x], tok)

          For the call trace, we try to record variables that correspond to objects,
          but if not possible then we record method names.
        *)
      match fun_exp with
      | { e = Fetch { base = Var method_; rev_offset = [] }; _ }
      (* fun_exp = `method(...)` *) -> (
          (* lval = `this.x.y.z` so we assume to be calling a class method, and
             because the call `method(...)` has no explicit receiver object, then
             we assume it is the `this` object in the caller's context. Thus,
             the instantiated l-vale is `x.y.z`. *)
          match tlval.offset with
          | Ofld var :: offset -> Some (var, offset, snd method_.ident)
          | []
          | (Oint _ | Ostr _ | Oany) :: _ ->
              (* we have no 'var' to take here *)
              log_error ();
              None)
      | {
       (* fun_exp = `<base>. ... .method(...)` *)
       e = Fetch { base; rev_offset = { o = Dot method_; _ } :: rev_offset' };
       _;
      } -> (
          match (base, rev_offset', tlval.offset) with
          | Var obj, [], _offset ->
              (* fun_exp = `obj.method(...)`, given lval = `this.x`
                 the instantiated l-value is `obj.x` *)
              Some (obj, tlval.offset, snd obj.ident)
          | VarSpecial (This, _), [], Ofld var :: offset ->
              (* fun_exp = `this.method(...)`, given lval = `this.x.y.z`
                 the instantiated l-value is `x.y.z`. *)
              Some (var, offset, snd method_.ident)
          | __else__ ->
              (* fun_exp = `this.obj.method(...)` (e.g.), given lval = `this.x.y`
                 the instantiated l-value is `obj.x.y`. *)
              let lval = IL.{ base; rev_offset = rev_offset' } in
              let* var, offset = Lval_env.normalize_lval lval in
              Some (var, offset @ tlval.offset, snd method_.ident))
      | __else__ ->
          log_error ();
          None)

let instantiate_lval_using_shape lval_env fparams (fun_exp : IL.exp) args_taints
    lval : (Taints.t * shape) option =
  let { T.base; offset } = lval in
  let* base, offset =
    match base with
    | T.BArg pos -> Some (`Arg pos, offset)
    | BThis -> (
        (* TODO: Should we refactor this with 'instantiate_lval_using_actual_exps' ? *)
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
    | BVar var -> Some (`Var var, offset)
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
  Shape.find_in_shape_poly ~taints:base_taints offset base_shape

(* What is the taint denoted by 'sig_lval' ? *)
let instantiate_lval lval_env fparams fun_exp args_exps
    (args_taints : (Taints.t * shape) IL.argument list) (sig_lval : T.lval) =
  match
    instantiate_lval_using_shape lval_env fparams fun_exp args_taints sig_lval
  with
  | Some (taints, shape) -> Some (taints, shape)
  | None ->
      Log.warn (fun m ->
          m "Cannot find the taint&shape of %s via instantiate_lval_using_shape"
            (T.show_lval sig_lval));
      (* We want to know what's the taint carried by 'arg_exp.x1. ... .xN'.

         THINK: Should we need this when we cover everything with shapes? *)
      let* var, offset, _obj =
        instantiate_lval_using_actual_exps fun_exp fparams args_exps sig_lval
      in
      let* lval_taints, shape = Lval_env.find_poly lval_env var offset in
      Some (lval_taints, shape)

(* This function is consuming the taint signature of a function to determine
   a few things:
   1) What is the status of taint in the current environment, after the function
      call occurs?
   2) Are there any effects that occur within the function due to taints being
      input into the function body, from the calling context?
*)
let rec instantiate_function_signature lval_env (taint_sig : Signature.t)
    ~callee ~(args : _ option)
    (args_taints : (Taints.t * shape) IL.argument list) : call_effects option =
  let lval_to_taints lval =
    (* This function simply produces the corresponding taints to the
        given argument, within the body of the function.
    *)
    (* Our first pass will be to substitute the args for taints.
       We can't do this indiscriminately at the beginning, because
       we might need to use some of the information of the pre-substitution
       taints and the post-substitution taints, for instance the tokens.

       So we will isolate this as a specific step to be applied as necessary.
    *)
    let opt_taints_shape =
      instantiate_lval lval_env taint_sig.params callee args args_taints lval
    in
    Log.debug (fun m ->
        m ~tags:sigs_tag "- Instantiating %s: %s -> %s"
          (Display_IL.string_of_exp callee)
          (T.show_lval lval)
          (match opt_taints_shape with
          | None -> "nothing"
          | Some (taints, shape) ->
              spf "%s & %s" (T.show_taints taints) (show_shape shape)));
    opt_taints_shape
  in
  (* Instantiation helpers *)
  let taints_in_ctrl () = Lval_env.get_control_taints lval_env in
  let inst_var = { inst_lval = lval_to_taints; inst_ctrl = taints_in_ctrl } in
  let inst_taint_var taint =
    instantiate_taint_var inst_var taint ||| (Taints.singleton taint, Bot)
  in
  let subst_in_precondition = subst_in_precondition inst_var in
  let inst_trace =
    {
      add_call_to_trace_for_src = add_call_to_trace_if_callee_has_eorig ~callee;
      fix_token_trace_for_var = add_call_to_token_trace ~callee;
    }
  in
  let inst_taints taints = instantiate_taints inst_var inst_trace taints in
  let inst_shape shape = instantiate_shape inst_var inst_trace shape in
  let inst_taints_and_shape (taints, shape) =
    let taints = inst_taints taints in
    let shape = inst_shape shape in
    (taints, shape)
  in
  (* Instatiate effects *)
  let inst_effect : Effect.t -> call_effect list = function
    | Effect.ToReturn { data_taints; data_shape; control_taints; return_tok } ->
        let data_taints = inst_taints data_taints in
        let data_shape = inst_shape data_shape in
        let control_taints =
          (* No need to instantiate 'control_taints' because control taint variables
           * do not propagate through function calls... BUT instantiation also fixes
           * the call trace! *)
          inst_taints control_taints
        in
        if
          Shape.taints_and_shape_are_relevant data_taints data_shape
          || not (Taints.is_empty control_taints)
        then
          [ ToReturn { data_taints; data_shape; control_taints; return_tok } ]
        else []
    | Effect.ToSink
        { taints_with_precondition = taints, requires; sink; merged_env } ->
        let taints =
          taints
          |> List.concat_map (fun { Effect.taint; sink_trace } ->
                 (* TODO: Use 'instantiate_taint' here too (note differences wrt the call trace). *)
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
                       add_call_to_trace_if_callee_has_eorig ~callee
                         ~tainted_tokens:(List.rev taint.rev_tokens)
                         sink_trace
                       ||| sink_trace
                     in
                     let call_taints, call_shape = inst_taint_var taint in
                     (* See NOTE(gather-all-taints) *)
                     let call_taints =
                       call_taints
                       |> Taints.union
                            (Shape.gather_all_taints_in_shape call_shape)
                     in
                     Taints.elements call_taints
                     |> List_.map (fun x -> { Effect.taint = x; sink_trace }))
        in
        if List_.null taints then []
        else
          [
            ToSink
              {
                taints_with_precondition = (taints, requires);
                sink;
                merged_env;
              };
          ]
    | Effect.ToLval (taints, dst_sig_lval) ->
        (* Taints 'taints' go into an argument of the call, by side-effect.
         * Right now this is mainly used to track taint going into specific
         * fields of the callee object, like `this.x = "tainted"`. *)
        let+ dst_var, dst_offset, tainted_tok =
          (* 'dst_lval' is the actual argument/l-value that corresponds
             * to the formal argument 'dst_sig_lval'. *)
          instantiate_lval_using_actual_exps callee taint_sig.params args
            dst_sig_lval
        in
        let taints =
          taints
          |> instantiate_taints
               {
                 inst_lval = lval_to_taints;
                 (* Note that control taints do not propagate to l-values. *)
                 inst_ctrl = (fun _ -> Taints.empty);
               }
               {
                 add_call_to_trace_for_src =
                   add_call_to_trace_if_callee_has_eorig ~callee;
                 fix_token_trace_for_var =
                   add_lval_update_to_token_trace ~callee tainted_tok;
               }
        in

        if Taints.is_empty taints then []
        else [ ToLval (taints, dst_var, dst_offset) ]
    | Effect.ToSinkInCall
        { callee = fun_exp; arg = fun_arg; args_taints = fun_args_taints } -> (
        Log.debug (fun m ->
            m ~tags:sigs_tag "- Instantiating %s: Call to function arg '%s'"
              (Display_IL.string_of_exp callee)
              (Display_IL.string_of_exp fun_exp));
        let+ fun_sig =
          let fun_lval = T.lval_of_arg fun_arg in
          match lval_to_taints fun_lval with
          | Some (_fun_taints, Fun fun_sig) ->
              (* The '_fun_taints' are the taints (not its signature) of the actual
               * function argument, and they are not used for instantiation, they are
               * tracked by the caller like any other intra-procedural taint. *)
              Some fun_sig
          | Some (_fun_taints, non_Fun_shape) ->
              Log.err (fun m ->
                  m "%s: '%s' does not have a function shape but: %s"
                    (Display_IL.string_of_exp callee)
                    (Display_IL.string_of_exp fun_exp)
                    (show_shape non_Fun_shape));
              None
          | None ->
              Log.err (fun m ->
                  m "%s: Could not find the shape of function argument '%s'"
                    (Display_IL.string_of_exp callee)
                    (T.show_arg fun_arg));
              None
        in
        let args_taints =
          (* The args_taints need to be instantiated too. *)
          fun_args_taints
          |> List_.map (function
               | IL.Unnamed (taints, shape) ->
                   IL.Unnamed (inst_taints_and_shape (taints, shape))
               | IL.Named (ident, (taints, shape)) ->
                   IL.Named (ident, inst_taints_and_shape (taints, shape)))
        in
        Log.debug (fun m ->
            m ~tags:sigs_tag
              "** %s: Instantiated function call '%s' arguments: %s -> %s"
              (Display_IL.string_of_exp callee)
              (Display_IL.string_of_exp fun_exp)
              (Effect.show_args_taints fun_args_taints)
              (Effect.show_args_taints args_taints));
        match
          instantiate_function_signature lval_env fun_sig ~callee:fun_exp
            ~args:None args_taints
        with
        | Some call_effects -> call_effects
        | None ->
            Log.err (fun m ->
                m "%s: Could not instantiate the signature of '%s'"
                  (Display_IL.string_of_exp callee)
                  (Display_IL.string_of_exp fun_exp));
            [])
  in
  let call_effects =
    taint_sig.effects |> Effects.elements |> List.concat_map inst_effect
  in
  Log.debug (fun m ->
      m ~tags:sigs_tag "Instantiated call to %s: %s"
        (Display_IL.string_of_exp callee)
        (show_call_effects call_effects));
  Some call_effects
