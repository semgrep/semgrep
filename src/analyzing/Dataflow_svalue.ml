(* Iago Abal
 *
 * Copyright (C) 2020 r2c
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
module Log = Log_analyzing.Log
module G = AST_generic
module H = AST_generic_helpers
module F = IL
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module LV = IL_helpers
module Eval = Eval_il_partial

let tags = Log_analyzing.svalue_tag

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* map for each node/var whether a variable is constant *)
type mapping = G.svalue Var_env.mapping

module DataflowX = Dataflow_core.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_constness_of_function = ref None
let hook_transfer_of_assume = ref None

(*****************************************************************************)
(* Constness *)
(*****************************************************************************)

let eval_format env args =
  let cs = List_.map (Eval.eval env) args in
  if
    cs
    |> List.for_all (function
         | G.Lit _
         | G.Cst _ ->
             true
         | _ -> false)
  then G.Cst G.Cstr
  else G.NotCst

let eval_builtin_func lang env func args =
  let args = List_.map IL_helpers.exp_of_arg args in
  match func with
  | { e = _; eorig = SameAs eorig } -> (
      let* gname = H.name_of_dot_access eorig in
      match (lang, gname) with
      | ( Lang.Java,
          G.IdQualified
            {
              name_last = ("format", _), _;
              name_middle =
                Some
                  (QDots
                    ( [ (("String", _), _) ]
                    | [ (("java", _), _); (("lang", _), _); (("String", _), _) ]
                      ));
              _;
            } ) ->
          Some (eval_format env args)
      | __else__ -> None)
  | __else__ -> None

let result_of_function_call_constant lang f args =
  match (lang, f, args) with
  (* Built-in knowledge, we know these functions return constants when
     * given constant arguments. *)
  | ( Lang.Php,
      {
        e =
          Fetch
            {
              base =
                Var
                  {
                    ident = ("escapeshellarg" | "htmlspecialchars_decode"), _;
                    _;
                  };
              rev_offset = [];
            };
        _;
      },
      [ (G.Lit (G.String _) | G.Cst G.Cstr) ] ) ->
      Some (G.Cst G.Cstr)
  (* Pro/Interfile: Look up inferred constness of the function *)
  | _lang, { e = Fetch _; eorig = SameAs eorig }, _args -> (
      match !hook_constness_of_function with
      | Some constness_of_func -> (
          match constness_of_func eorig with
          | Some G.NotCst
          | None ->
              None
          | Some svalue -> Some svalue)
      | None -> None)
  | __else__ -> None

(*****************************************************************************)
(* Symbolic evaluation *)
(*****************************************************************************)

(* Defines the subset of Generic expressions that we can propagate.
 *
 * In principle we could just propagate any expression, we restricted it mainly
 * to play it safe. Eventually we could consider lifting up these restrictions
 * and see what happens. The main problem with the current approach is that
 * every now and then somebody requests X to be supported by symbolic propagation.
 *)
let rec is_symbolic_expr expr =
  match expr.G.e with
  | G.L _ -> true
  | G.N _ -> true
  | G.IdSpecial _ -> true
  | G.Cast (_, _, e)
  | G.DotAccess (e, _, FN _) ->
      is_symbolic_expr e
  | G.ArrayAccess (e1, (_, e2, _)) -> is_symbolic_expr e1 && is_symbolic_expr e2
  | G.Call (e, (_, args, _)) ->
      is_symbolic_expr e && List.for_all is_symbolic_arg args
  | G.New (_, _, _, args) ->
      let args = Tok.unbracket args in
      List.for_all is_symbolic_arg args
  | G.Record (_, fields, _) -> List.for_all is_symbolic_field fields
  | __else__ -> false

and is_symbolic_arg arg =
  match arg with
  | G.Arg e -> is_symbolic_expr e
  | G.ArgKwd (_, e)
  | G.ArgKwdOptional (_, e) ->
      is_symbolic_expr e
  | G.ArgType _ -> true
  | G.OtherArg _ -> false

and is_symbolic_field field =
  match field with
  | G.F
      {
        s =
          G.DefStmt
            ({ name = G.EN _; _ }, G.FieldDefColon { vinit = Some expr; _ });
        _;
      } ->
      is_symbolic_expr expr
  | __else__ -> false

let sym_prop eorig =
  match eorig with
  | SameAs e_gen when is_symbolic_expr e_gen -> G.Sym e_gen
  | ___else___ -> G.NotCst

let eval_or_sym_prop env exp =
  match Eval.eval env exp with
  | G.NotCst -> sym_prop exp.eorig
  | c -> c

let no_cycles_in_svalue (id_info : G.id_info) svalue =
  let for_all_id_info : (G.id_info -> bool) -> G.any -> bool =
    (* Check that all id_info's satisfy a given condition. We use refs so that
     * we can have a single visitor for all calls, given that the old
     * `mk_visitor` was pretty expensive, and constructing a visitor object may
     * be as well. *)
    let i = ref 0 in
    let ff = ref (fun _ -> assert false) in
    let ok = ref true in
    let vout =
      object (self : 'self)
        inherit [_] G.iter_no_id_info

        method! visit_id_info env ii =
          ok := !ok && !ff ii;
          (match !(ii.id_svalue) with
          | Some (Sym e) when !ok ->
              (* Following `id_svalue`s can explode in pathological cases,
               * see 'tests/rules/sym_prop_explosion.js', so we need to
               * set a bound. *)
              if !i < Limits_semgrep.svalue_prop_MAX_VISIT_SYM_IN_CYCLE_CHECK
              then (
                incr i;
                self#visit_expr env e)
              else ok := false
          | None
          | Some _ ->
              ());
          if not !ok then raise Exit
      end
    in
    fun f ast ->
      i := 0;
      ff := f;
      ok := true;
      try
        vout#visit_any () ast;
        !ok
      with
      | Exit -> false
  in
  (* Check that `c' contains no reference to `var'. It can contain references
   * to other occurrences of `var', but not to the same occurrence (that would
   * be a cycle), and each occurence must have its own `id_svalue` ref. This
   * is not supposed to happen, but if it does happen by accident then it would
   * cause an infinite loop, stack overflow, or segfault later on. *)
  match svalue with
  | G.Sym e ->
      for_all_id_info
        (fun ii ->
          (* Note the use of physical equality, we are looking for the *same*
           * id_svalue ref, that tells us it's the same variable occurrence. *)
          not (phys_equal id_info.id_svalue ii.id_svalue))
        (G.E e)
  | G.NotCst
  | G.Cst _
  | G.Lit _ ->
      true

let set_svalue_ref id_info c' =
  if no_cycles_in_svalue id_info c' then
    match !(id_info.id_svalue) with
    | None -> id_info.id_svalue := Some c'
    | Some c -> id_info.id_svalue := Some (Eval.refine c c')
  else
    Log.debug (fun m ->
        m ~tags "Cycle check failed for %s := ..." (G.show_id_info id_info))
(* (G.show_svalue c') *)

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

(* This is a must-analysis so a variable is only constant if it is constant in
 * all preceding paths:
 *
 *     def foo():
 *         if cond():
 *              x = "abc"
 *         return x # x is not constant, it may be undefined!
 *
 * THINK: We could have an option to decide whether we want may/must.
 *
 * For simplicity we just assume non-constant. This is OK because it's a must-
 * analysis. But then we have problems with loops such as:
 *
 *     x = "a"
 *     while cond():
 *         x = x + "a"
 *
 * ^ FIXME: At the entry node everything must be set to non-constant, but
 * otherwise it should be initialized with _|_.
 *)
let union_env =
  VarMap.merge (fun _ c1_opt c2_opt ->
      let* c1 = c1_opt in
      let* c2 = c2_opt in
      Some (Eval.union c1 c2))

let input_env ~enter_env ~(flow : F.cfg) mapping ni =
  let node = flow.graph#nodes#assoc ni in
  match node.F.n with
  | Enter -> enter_env
  | _else -> (
      let pred_envs =
        CFG.predecessors flow ni
        |> List_.map (fun (pi, _) -> mapping.(pi).D.out_env)
      in
      (* Note that `VarMap.empty` represents an environment where all variables
       * are non-constant, thus `VarMap.empty` is not the neutral element wrt
       * `union_env` but the absorbing element. In other words,
       * `union_env VarMap.empty env` will always return an  environment where
       * all variables are non-constant.
       *
       * FIXME: Right now `enter_env` only sets the function parameters to
       *        non-constant, but it should do the same with every local
       *        variable. Then we could change `union_env` to stop assuming
       *        non-constant by default. Although by storing `NotCst` explicitly
       *        we need to be careful with performance when processing large
       *        graphs, we should pick a compact representation for potentially
       *        large sets of `NotCst`s.
       *)
      match pred_envs with
      | [] -> VarMap.empty
      | [ penv ] -> penv
      | penv1 :: penvs -> List.fold_left union_env penv1 penvs)

let update_env_with env var sval =
  (* We save allocations by not storing `NotCst` at all, we actually
   * remove variables from the environment when they become non-constant!
   * This improves perf by a lot when proccessing large graphs. *)
  match sval with
  | G.NotCst -> VarMap.remove (IL.str_of_name var) env
  | __else__ -> VarMap.add (IL.str_of_name var) sval env

(* Semgrep Pro *)
let transfer_of_assume (assume : bool) (cond : IL.exp_kind)
    (inp : G.svalue Var_env.t) : G.svalue Var_env.t =
  match !hook_transfer_of_assume with
  | None -> inp
  | Some hook -> hook assume cond inp

let rec transfer :
    lang:Lang.t ->
    enter_env:G.svalue Var_env.t ->
    fun_cfg:F.fun_cfg ->
    G.svalue Var_env.transfn =
 fun ~lang ~enter_env ~fun_cfg
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let flow = fun_cfg.cfg in

  let node = flow.graph#nodes#assoc ni in

  let inp' = input_env ~enter_env ~flow mapping ni in

  let out' =
    match node.F.n with
    | Enter
    | Exit
    | Join
    | NCond _
    | NGoto _
    | NReturn _
    | NThrow _
    | NOther _
    | NTodo _ ->
        inp'
    | TrueNode cond -> transfer_of_assume true cond.e inp'
    | FalseNode cond -> transfer_of_assume false cond.e inp'
    | NInstr instr -> (
        let eval_env = Eval.mk_env lang inp' in

        (* TODO: For now we only handle the simplest cases. *)
        match instr.i with
        | Assign ({ base = Var var; rev_offset = [] }, exp) ->
            (* var = exp *)
            let cexp = eval_or_sym_prop eval_env exp in
            update_env_with inp' var cexp
        | Call (Some { base = Var var; rev_offset = [] }, func, args) -> (
            let args_val =
              List_.map
                (fun arg -> Eval.eval eval_env (IL_helpers.exp_of_arg arg))
                args
            in
            match result_of_function_call_constant lang func args_val with
            | Some svalue -> VarMap.add (IL.str_of_name var) svalue inp'
            | None -> (
                match eval_builtin_func lang eval_env func args with
                | None
                | Some NotCst ->
                    (* symbolic propagation *)
                    (* Call to an arbitrary function, we are intraprocedural so we cannot
                     * propagate actual constants in this case, but we can propagate the
                     * call itself as a symbolic expression. *)
                    let ccall = sym_prop instr.iorig in
                    update_env_with inp' var ccall
                | Some cexp -> update_env_with inp' var cexp))
        | New ({ base = Var var; rev_offset = [] }, _ty, _ii, _args) ->
            update_env_with inp' var (sym_prop instr.iorig)
        | CallSpecial
            (Some { base = Var var; rev_offset = [] }, (special, _), args) ->
            let args = List_.map IL_helpers.exp_of_arg args in
            let cexp =
              (* We try to evaluate the special function, if we know how. *)
              if special =*= Concat then
                (* var = concat(args) *)
                Eval.eval_concat eval_env args
              else G.NotCst
            in
            let cexp =
              (* If we don't know how to evaluate this function, or if the
               * evaluation fails, then we do sym-prop. *)
              if cexp =*= G.NotCst then sym_prop instr.iorig else cexp
            in
            update_env_with inp' var cexp
        | Call
            ( None,
              {
                e =
                  Fetch
                    { base = Var var; rev_offset = { o = Dot _; _ } :: _; _ };
                _;
              },
              _ ) ->
            (* Method call `var.f(args)` that returns void, we conservatively
             * assume that it may be updating `var`; e.g. in Ruby strings are
             * mutable so given `x.concat(y)` we will assume that the value of
             * `x` has been changed. *)
            VarMap.remove (IL.str_of_name var) inp'
        | ___else___ -> (
            (* In any other case, assume non-constant.
             * This covers e.g. `x.f = E`, `x[E1] = E2`, `*x = E`, etc. *)
            let lvar_opt = LV.lvar_of_instr_opt instr in
            match lvar_opt with
            | None -> inp'
            | Some lvar -> VarMap.remove (IL.str_of_name lvar) inp'))
  in
  let out' = do_lambdas lang fun_cfg.lambdas out' node in
  { D.in_env = inp'; out_env = out' }

and do_lambdas lang lambdas in_env node =
  (* In svalue-analysis we only need to visit lambdas at definition site,
   * we simply propagate svalues into the lambda's body, but whatever
   * happens inside the lambda is not visible outside it. *)
  match node.F.n with
  | NInstr { i = AssignAnon (lval, Lambda _); _ } -> (
      match LV.lval_is_lambda lambdas lval with
      | Some (_name, lambda_cfg) ->
          let mapping = fixpoint_with_env lang in_env lambda_cfg in
          update_svalue lambda_cfg.cfg mapping;
          let lambda_env =
            mapping.(lambda_cfg.cfg.exit).Dataflow_core.out_env
          in
          let out_env =
            (* We look at the svalue info resulting from analyzing the lambda's
               * body, and we kill the svalue info of any variables that may be altered
               * by the execution of the lambda. *)
            VarMap.merge
              (fun _var opt_env opt_lam ->
                match (opt_env, opt_lam) with
                | Some sval1, Some sval2 when Eval.eq sval1 sval2 ->
                    (* Nothing changed so we keep the same svalue info. *)
                    opt_env
                (* Either the svalue of 'var' changed or it was deemed non-constant
                 * (see 'update_env_with'), once the lambda is defined we don't know
                 * when it's going to execute, so we just kill the svalue info.
                 * (We could do better.) *)
                | Some _, Some _
                | Some _, None ->
                    None
                (* Some new constant was found inside the lambda, but we ignore those
                 * outside the lambda. *)
                | None, Some _ -> None
                (* Cannot happen, just to please the compiler. *)
                | None, None -> None)
              in_env lambda_env
          in
          out_env
      | None -> in_env)
  | __else__ -> in_env

and fixpoint_with_env lang enter_env fun_cfg =
  let flow = fun_cfg.cfg in
  let mapping, timeout =
    DataflowX.fixpoint ~timeout:Limits_semgrep.svalue_prop_FIXPOINT_TIMEOUT
      ~eq_env:(Var_env.eq_env Eval.eq)
      ~init:(DataflowX.new_node_array flow (Var_env.empty_inout ()))
      ~trans:(transfer ~lang ~enter_env ~fun_cfg)
        (* svalue is a forward analysis! *)
      ~forward:true ~flow
  in
  if timeout =*= `Timeout then
    Log.warn (fun m -> m "Fixpoint timeout while performing svalue-propagation");
  mapping

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

and (fixpoint : Lang.t -> IL.fun_cfg -> mapping) =
 fun lang fun_cfg ->
  let enter_env = VarMap.empty in
  fixpoint_with_env lang enter_env fun_cfg

and update_svalue (flow : F.cfg) mapping =
  flow.graph#nodes#keys
  |> List.iter (fun ni ->
         let ni_info = mapping.(ni) in

         let node = flow.graph#nodes#assoc ni in

         (* Update RHS svalue according to the input env. *)
         LV.rlvals_of_node node.n
         |> List.iter (function
              | { base = Var var; _ } -> (
                  match
                    VarMap.find_opt (IL.str_of_name var) ni_info.D.in_env
                  with
                  | None -> ()
                  | Some c -> set_svalue_ref var.id_info c)
              | ___else___ -> ())
         (* Should not update the LHS svalue since in x = E, x is a "ref",
          * and it should not be substituted for the value it holds. *))
