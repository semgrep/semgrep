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
module G = AST_generic
module F = IL
module D = Dataflow_core
module Var_env = Dataflow_var_env
module VarMap = Var_env.VarMap
module LV = IL_helpers

let logger = Logging.get_logger [ __MODULE__ ]

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

type constness = Constant | NotConstant [@@deriving show]

(*****************************************************************************)
(* Hooks *)
(*****************************************************************************)

let hook_constness_of_function = ref None
let hook_transfer_of_assume = ref None

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let warning _tok s =
  (* TODO: Report these errors as matches of a builtin_div_by_zero rule. *)
  logger#warning "CFGError: %s" s

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Constness *)
(*****************************************************************************)

let result_of_function_call_is_constant lang f args =
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
      true
  (* Pro/Interfile: Look up inferred constness of the function *)
  | _lang, { e = Fetch _; eorig = SameAs eorig }, _args -> (
      match !hook_constness_of_function with
      | Some constness_of_func -> (
          match constness_of_func eorig with
          | Some Constant -> true
          | Some NotConstant
          | None ->
              false)
      | None -> false)
  | __else__ -> false

let eq_literal l1 l2 = G.equal_literal l1 l2
let eq_ctype t1 t2 = t1 =*= t2

let ctype_of_literal = function
  | G.Bool _ -> G.Cbool
  | G.Int _ -> G.Cint
  | G.String _ -> G.Cstr
  | ___else___ -> G.Cany

let eq c1 c2 =
  match (c1, c2) with
  | G.Lit l1, G.Lit l2 -> eq_literal l1 l2
  | G.Cst t1, G.Cst t2 -> eq_ctype t1 t2
  | G.Sym e1, G.Sym e2 ->
      (* We only consider two `Sym`s equal when they are exactly the same. If two
       * `Sym`s are structuraly equal but their `id_svalue` pointers are different
       * it may not be safe to use them interchangeably. (TBH I'm not sure if/when
       * it's safe so I'm erring on the side of caution!) *)
      phys_equal e1 e2
  | G.NotCst, G.NotCst -> true
  | G.Lit _, _
  | G.Cst _, _
  | G.Sym _, _
  | G.NotCst, _ ->
      false

let union_ctype t1 t2 = if eq_ctype t1 t2 then t1 else G.Cany

(* aka merge *)
let union c1 c2 =
  match (c1, c2) with
  | _any, G.NotCst
  | G.NotCst, _any ->
      G.NotCst
  | c1, c2 when eq c1 c2 -> c1
  (* Note that merging symbolic expressions can be tricky.
   *
   * Example: Both branches assign `y = x.foo`, but `x` may have a different
   * value in each branch. When merging symbolic expressions like `x.foo` we
   * must first merge their dependencies (in our example, `x`).
   *
   *     if c:
   *       x = a
   *       y = x.foo
   *     else:
   *       x = b
   *       y = x.foo
   *
   * Example: If we are not careful, when analyzing loops, we could introduce
   * circular dependencies! (See tests/rules/sym_prop_no_merge1.go)
   *
   *     for cond {
   *       x = f(x)
   *     }
   *)
  | _any, G.Sym _
  | G.Sym _, _any ->
      G.NotCst
  | G.Lit l1, G.Lit l2 ->
      let t1 = ctype_of_literal l1 and t2 = ctype_of_literal l2 in
      G.Cst (union_ctype t1 t2)
  | G.Lit l1, G.Cst t2
  | G.Cst t2, G.Lit l1 ->
      let t1 = ctype_of_literal l1 in
      G.Cst (union_ctype t1 t2)
  | G.Cst t1, G.Cst t2 -> G.Cst (union_ctype t1 t2)

(* THINK: This assumes that analyses are sound... but they are not
   (e.g. due to aliasing). *)
let refine c1 c2 =
  match (c1, c2) with
  | _ when eq c1 c2 -> c1
  | c, G.NotCst
  | G.NotCst, c ->
      c
  | G.Lit _, _
  | G.Cst _, G.Cst _
  | G.Sym _, G.Sym _ ->
      c1
  | G.Cst _, G.Sym _ -> c1
  | G.Cst _, G.Lit _ -> c2
  | G.Sym _, G.Lit _
  | G.Sym _, G.Cst _ ->
      c2

(*****************************************************************************)
(* Constness evaluation *)
(*****************************************************************************)

let literal_of_bool b =
  let b_str = string_of_bool b in
  (* TODO: use proper token when possible? *)
  let tok = Tok.unsafe_fake_tok b_str in
  G.Bool (b, tok)

let literal_of_int i =
  let i_str = string_of_int i in
  (* TODO: use proper token when possible? *)
  let tok = Tok.unsafe_fake_tok i_str in
  G.Int (Some i, tok)

let int_of_literal = function
  | G.Int (x, _) -> x
  | ___else___ -> None

let literal_of_string ?tok s : G.literal =
  let tok =
    match tok with
    | None -> Tok.unsafe_fake_tok s
    | Some tok ->
        (* THIHK: IMO this should be `Parse_info.fake_info tok s`. Yet right now
         * we are picking an arbitrary token from one of the strings involved in
         * computing `s` and prentending it is a real token for `s`, but it's NOT.
         * This may not interact well with Autofix (?). Anyways for now we have
         * to do this because an $MVAR could match `s` and Semgrep assumes that
         * an $MVAR always has a source location. *)
        tok
  in
  G.String (fb (s, tok))

let eval_unop_bool op b =
  match op with
  | G.Not -> G.Lit (literal_of_bool (not b))
  | _else -> G.Cst G.Cbool

let eval_binop_bool op b1 b2 =
  match op with
  | G.Or -> G.Lit (literal_of_bool (b1 || b2))
  | G.And -> G.Lit (literal_of_bool (b1 && b2))
  | _else -> G.Cst G.Cbool

let eval_unop_int op opt_i =
  match (op, opt_i) with
  | G.Plus, Some i -> G.Lit (literal_of_int i)
  | G.Minus, Some i -> G.Lit (literal_of_int (-i))
  | ___else____ -> G.Cst G.Cint

(* This reduces arithmetic "exceptions" to `G.Cst G.Cint`, it does NOT
 * detect overflows for any other language than OCaml. Note that OCaml
 * integers have just 63-bits in 64-bit architectures!
 *)
let eval_binop_int tok op opt_i1 opt_i2 =
  let sign_bit i = i asr (Sys.int_size - 1) =|= 1 in
  match (op, opt_i1, opt_i2) with
  | G.Plus, Some i1, Some i2 ->
      let r = i1 + i2 in
      if sign_bit i1 =:= sign_bit i2 && sign_bit r <> sign_bit i1 then
        G.Cst G.Cint (* overflow *)
      else G.Lit (literal_of_int (i1 + i2))
  | G.Minus, Some i1, Some i2 ->
      let r = i1 - i2 in
      if sign_bit i1 <> sign_bit i2 && sign_bit r <> sign_bit i1 then
        G.Cst G.Cint (* overflow *)
      else G.Lit (literal_of_int (i1 - i2))
  | G.Mult, Some i1, Some i2 ->
      let overflow =
        i1 <> 0 && i2 <> 0
        && ((i1 < 0 && i2 =|= min_int) (* >max_int *)
           || (i1 =|= min_int && i2 < 0) (* >max_int *)
           ||
           if sign_bit i1 =:= sign_bit i2 then abs i1 > abs (max_int / i2)
             (* >max_int *)
           else abs i1 > abs (min_int / i2) (* <min_int *))
      in
      if overflow then G.Cst G.Cint else G.Lit (literal_of_int (i1 * i2))
  | G.Div, Some i1, Some i2 -> (
      if i1 =|= min_int && i2 =|= -1 then
        G.Cst G.Cint (* = max_int+1, overflow *)
      else
        try G.Lit (literal_of_int (i1 / i2)) with
        | Division_by_zero ->
            warning tok "Found division by zero";
            G.Cst G.Cint)
  | ___else____ -> G.Cst G.Cint

let eval_binop_string ?tok op s1 s2 =
  match op with
  | G.Plus
  | G.Concat ->
      G.Lit (literal_of_string ?tok (s1 ^ s2))
  | __else__ -> G.Cst G.Cstr

let rec eval (env : G.svalue Var_env.t) exp : G.svalue =
  match exp.e with
  | Fetch lval -> eval_lval env lval
  | Literal li -> G.Lit li
  (* Python: cond and <then-exp> or <else-exp> *)
  | Operator
      ( ((Or, _) as op),
        ([
           Unnamed { e = Operator ((And, _), [ _cond; Unnamed a_then ]); _ };
           Unnamed a_else;
         ] as args) ) -> (
      let c_then = eval env a_then in
      let c_else = eval env a_else in
      match (c_then, c_else) with
      | G.Lit (G.String _), G.Lit (G.String _) -> G.Cst G.Cstr
      | G.Lit (G.Int _), G.Lit (G.Int _) -> G.Cst G.Cstr
      (* Fall-back to default evaluation strategy. *)
      | __else__ -> eval_op env op args)
  | Operator (op, args) -> eval_op env op args
  | Composite _
  | Record _
  | Cast _
  | FixmeExp _ ->
      G.NotCst

and eval_lval env lval =
  match lval with
  | { base = Var x; rev_offset = [] } -> (
      let opt_c = VarMap.find_opt (IL.str_of_name x) env in
      match (!(x.id_info.id_svalue), opt_c) with
      | None, None -> G.NotCst
      | Some c, None
      | None, Some c ->
          c
      | Some c1, Some c2 -> refine c1 c2)
  | ___else___ -> G.NotCst

and eval_op env wop args =
  let op, tok = wop in
  let cs = args |> Common.map IL_helpers.exp_of_arg |> Common.map (eval env) in
  match (op, cs) with
  | G.Plus, [ c1 ] -> c1
  | op, [ G.Lit (G.Bool (b, _)) ] -> eval_unop_bool op b
  | op, [ G.Lit (G.Int _ as li) ] -> eval_unop_int op (int_of_literal li)
  | G.And, [ G.Lit (G.Bool (true, _)); c ] -> c (* Python: True and 42 -> 42 *)
  | G.Or, [ G.Lit (G.Bool (false, _)); c ] -> c (* Python: False or 42 -> 42 *)
  | op, [ G.Lit (G.Bool (b1, _)); G.Lit (G.Bool (b2, _)) ] ->
      eval_binop_bool op b1 b2
  | op, [ G.Lit (G.Int _ as li1); G.Lit (G.Int _ as li2) ] ->
      eval_binop_int tok op (int_of_literal li1) (int_of_literal li2)
  | op, [ G.Lit (G.String (_, (s1, _), _)); G.Lit (G.String (_, (s2, _), _)) ]
    ->
      eval_binop_string ~tok op s1 s2
  | _op, [ (G.Cst _ as c1) ] -> c1
  | _op, [ G.Cst t1; G.Cst t2 ] -> G.Cst (union_ctype t1 t2)
  | _op, [ G.Lit l1; G.Cst t2 ]
  | _op, [ G.Cst t2; G.Lit l1 ] ->
      let t1 = ctype_of_literal l1 in
      G.Cst (union_ctype t1 t2)
  | ___else___ -> G.NotCst

and eval_concat env args =
  match Common.map (eval env) args with
  | [] -> G.Lit (literal_of_string "")
  | G.Lit (G.String (_, (r, tok), _)) :: args' ->
      List.fold_left
        (fun res e ->
          match (res, e) with
          | G.Lit (G.String (_l, (x, tok), _r)), G.Lit (G.String (_, (s, _), _))
            ->
              (* should reuse l/r? *)
              G.Lit (literal_of_string ~tok (x ^ s))
          | (G.Lit _ | G.Cst _), G.Cst G.Cstr -> G.Cst G.Cstr
          | _____else_____ -> G.NotCst)
        (G.Lit (literal_of_string ~tok r))
        args'
  | ___else___ -> G.NotCst

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
  | _else -> false

and is_symbolic_arg arg =
  match arg with
  | G.Arg e -> is_symbolic_expr e
  | G.ArgKwd (_, e)
  | G.ArgKwdOptional (_, e) ->
      is_symbolic_expr e
  | G.ArgType _ -> true
  | G.OtherArg _ -> false

let sym_prop eorig =
  match eorig with
  | SameAs e_gen when is_symbolic_expr e_gen -> G.Sym e_gen
  | ___else___ -> G.NotCst

let eval_or_sym_prop env exp =
  match eval env exp with
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
    | Some c -> id_info.id_svalue := Some (refine c c')
  else logger#info "Cycle check failed for %s := ..." (G.show_id_info id_info)
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
      Some (union c1 c2))

let input_env ~enter_env ~(flow : F.cfg) mapping ni =
  let node = flow.graph#nodes#assoc ni in
  match node.F.n with
  | Enter -> enter_env
  | _else -> (
      let pred_envs =
        CFG.predecessors flow ni
        |> Common.map (fun (pi, _) -> mapping.(pi).D.out_env)
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

let transfer :
    lang:Lang.t ->
    enter_env:G.svalue Var_env.t ->
    flow:F.cfg ->
    G.svalue Var_env.transfn =
 fun ~lang ~enter_env ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
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
    | NLambda _
    | NThrow _
    | NOther _
    | NTodo _ ->
        inp'
    | TrueNode cond -> transfer_of_assume true cond.e inp'
    | FalseNode cond -> transfer_of_assume false cond.e inp'
    | NInstr instr -> (
        (* TODO: For now we only handle the simplest cases. *)
        match instr.i with
        | Assign ({ base = Var var; rev_offset = [] }, exp) ->
            (* var = exp *)
            let cexp = eval_or_sym_prop inp' exp in
            update_env_with inp' var cexp
        | Call (Some { base = Var var; rev_offset = [] }, func, args) ->
            let args_val =
              Common.map (fun arg -> eval inp' (IL_helpers.exp_of_arg arg)) args
            in
            if result_of_function_call_is_constant lang func args_val then
              VarMap.add (IL.str_of_name var) (G.Cst G.Cstr) inp'
            else
              (* symbolic propagation *)
              (* Call to an arbitrary function, we are intraprocedural so we cannot
               * propagate actual constants in this case, but we can propagate the
               * call itself as a symbolic expression. *)
              let ccall = sym_prop instr.iorig in
              update_env_with inp' var ccall
        | New ({ base = Var var; rev_offset = [] }, _ty, _ii, _args) ->
            update_env_with inp' var (sym_prop instr.iorig)
        | CallSpecial
            (Some { base = Var var; rev_offset = [] }, (special, _), args) ->
            let args = Common.map IL_helpers.exp_of_arg args in
            let cexp =
              (* We try to evaluate the special function, if we know how. *)
              if special =*= Concat then
                (* var = concat(args) *)
                eval_concat inp' args
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

  { D.in_env = inp'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint : Lang.t -> IL.name list -> F.cfg -> mapping) =
 fun lang _inputs flow ->
  let enter_env = VarMap.empty in
  DataflowX.fixpoint ~timeout:Limits_semgrep.svalue_prop_FIXPOINT_TIMEOUT
    ~eq_env:(Var_env.eq_env eq)
    ~init:(DataflowX.new_node_array flow (Var_env.empty_inout ()))
    ~trans:(transfer ~lang ~enter_env ~flow) (* svalue is a forward analysis! *)
    ~forward:true ~flow

let update_svalue (flow : F.cfg) mapping =
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
