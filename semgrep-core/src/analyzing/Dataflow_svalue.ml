(* Iago Abal
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
open IL
module G = AST_generic
module F = IL
module D = Dataflow_core
module VarMap = Dataflow_core.VarMap
module LV = IL_lvalue_helpers

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* map for each node/var whether a variable is constant *)
type mapping = G.svalue Dataflow_core.mapping

module DataflowX = Dataflow_prog.Make (struct
  type node = F.node
  type edge = F.edge
  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

type constness_type = Constant | NotAlwaysConstant [@@deriving show]

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let warning tok s =
  (* TODO: Report these errors as matches of a builtin_div_by_zero rule. *)
  Error_code.warning tok (Error_code.CFGError s)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_name name = spf "%s:%d" (fst name.ident) name.sid

let str_of_resolved_name name =
  let name = Common.map fst name in
  String.concat "." name

(*****************************************************************************)
(* Constness *)
(*****************************************************************************)

let hook_constness_table_of_functions = ref None

let result_of_function_call_is_constant lang f args =
  let check_f f_name =
    match !hook_constness_table_of_functions with
    | Some constness_f -> (
        match constness_f f_name with
        | Some Constant ->
            logger#trace "%s is always constant" f_name;
            true
        | Some NotAlwaysConstant ->
            logger#trace "%s is not always constant" f_name;
            false
        | None ->
            logger#trace "we have no information about the constness of %s"
              f_name;
            false)
    | _ -> false
  in

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
              offset = NoOffset;
            };
        _;
      },
      [ (G.Lit (G.String _) | G.Cst G.Cstr) ] ) ->
      true
  (* DeepSemgrep: Look up inferred constness of the function *)
  | ( _,
      {
        (* If there is an offset, the full resolved name will be found
           there. Otherwise, it will be found in the base *)
        e =
          ( Fetch { offset = Dot { ident; id_info = { id_resolved; _ }; _ }; _ }
          | Fetch
              {
                base = Var { ident; id_info = { id_resolved; _ }; _ };
                offset = NoOffset | Index _;
              } );
        _;
      },
      _ ) -> (
      match !id_resolved with
      | Some (G.ResolvedName (name, _alternate_names), _) ->
          let f_name = str_of_resolved_name name in
          check_f f_name
      | _ ->
          logger#info "%s does not have a resolved name" (fst ident);
          false)
  | _ -> false

let eq_literal l1 l2 = G.equal_literal l1 l2
let eq_ctype t1 t2 = t1 = t2

let ctype_of_literal = function
  | G.Bool _ -> G.Cbool
  | G.Int _ -> G.Cint
  | G.String _ -> G.Cstr
  | ___else___ -> G.Cany

let eq c1 c2 =
  match (c1, c2) with
  | G.Lit l1, G.Lit l2 -> eq_literal l1 l2
  | G.Cst t1, G.Cst t2 -> eq_ctype t1 t2
  | G.Sym e1, G.Sym e2 -> AST_utils.with_structural_equal G.equal_expr e1 e2
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
  (* For now we only propagate symbolic expressions through linear sequences
   * of statements. Note that merging symbolic expressions can be tricky.
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
   * circular dependencies! (See tests/OTHER/rules/sym_prop_no_merge1.go)
   *
   *     for cond {
   *       x = f(x)
   *     }
   *)
  | _any, G.Sym _
  | G.Sym _, _any
  | _any, G.NotCst
  | G.NotCst, _any ->
      G.NotCst
  | c1, c2 when eq c1 c2 -> c1
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

let refine_svalue_ref c_ref c' =
  match !c_ref with
  | None -> c_ref := Some c'
  | Some c -> c_ref := Some (refine c c')

(*****************************************************************************)
(* Constness evaluation *)
(*****************************************************************************)

let literal_of_bool b =
  let b_str = string_of_bool b in
  (* TODO: use proper token when possible? *)
  let tok = Parse_info.unsafe_fake_info b_str in
  G.Bool (b, tok)

let literal_of_int i =
  let i_str = string_of_int i in
  (* TODO: use proper token when possible? *)
  let tok = Parse_info.unsafe_fake_info i_str in
  G.Int (Some i, tok)

let int_of_literal = function
  | G.Int (x, _) -> x
  | ___else___ -> None

let literal_of_string ?tok s =
  let tok =
    match tok with
    | None -> Parse_info.unsafe_fake_info s
    | Some tok ->
        (* THIHK: IMO this should be `Parse_info.fake_info tok s`. Yet right now
         * we are picking an arbitrary token from one of the strings involved in
         * computing `s` and prentending it is a real token for `s`, but it's NOT.
         * This may not interact well with Autofix (?). Anyways for now we have
         * to do this because an $MVAR could match `s` and Semgrep assumes that
         * an $MVAR always has a source location. *)
        tok
  in
  G.String (s, tok)

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
  let sign i = i asr (Sys.int_size - 1) in
  match (op, opt_i1, opt_i2) with
  | G.Plus, Some i1, Some i2 ->
      let r = i1 + i2 in
      if sign i1 = sign i2 && sign r <> sign i1 then G.Cst G.Cint (* overflow *)
      else G.Lit (literal_of_int (i1 + i2))
  | G.Minus, Some i1, Some i2 ->
      let r = i1 - i2 in
      if sign i1 <> sign i2 && sign r <> sign i1 then G.Cst G.Cint
        (* overflow *)
      else G.Lit (literal_of_int (i1 + i2))
  | G.Mult, Some i1, Some i2 ->
      let overflow =
        i1 <> 0 && i2 <> 0
        && ((i1 < 0 && i2 = min_int) (* >max_int *)
           || (i1 = min_int && i2 < 0) (* >max_int *)
           ||
           if sign i1 * sign i2 = 1 then abs i1 > abs (max_int / i2)
             (* >max_int *)
           else abs i1 > abs (min_int / i2) (* <min_int *))
      in
      if overflow then G.Cst G.Cint else G.Lit (literal_of_int (i1 * i2))
  | G.Div, Some i1, Some i2 -> (
      if i1 = min_int && i2 = -1 then G.Cst G.Cint (* = max_int+1, overflow *)
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

let rec eval (env : G.svalue D.env) exp : G.svalue =
  match exp.e with
  | Fetch lval -> eval_lval env lval
  | Literal li -> G.Lit li
  | Operator (op, args) -> eval_op env op args
  | Composite _
  | Record _
  | Cast _
  | FixmeExp _ ->
      G.NotCst

and eval_lval env lval =
  match lval with
  | { base = Var x; offset = NoOffset } -> (
      let opt_c = D.VarMap.find_opt (str_of_name x) env in
      match (!(x.id_info.id_svalue), opt_c) with
      | None, None -> G.NotCst
      | Some c, None
      | None, Some c ->
          c
      | Some c1, Some c2 -> refine c1 c2)
  | ___else___ -> G.NotCst

and eval_op env wop args =
  let op, tok = wop in
  let cs = Common.map (eval env) args in
  match (op, cs) with
  | G.Plus, [ c1 ] -> c1
  | op, [ G.Lit (G.Bool (b, _)) ] -> eval_unop_bool op b
  | op, [ G.Lit (G.Int _ as li) ] -> eval_unop_int op (int_of_literal li)
  | op, [ G.Lit (G.Bool (b1, _)); G.Lit (G.Bool (b2, _)) ] ->
      eval_binop_bool op b1 b2
  | op, [ G.Lit (G.Int _ as li1); G.Lit (G.Int _ as li2) ] ->
      eval_binop_int tok op (int_of_literal li1) (int_of_literal li2)
  | op, [ G.Lit (G.String (s1, _)); G.Lit (G.String (s2, _)) ] ->
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
  | G.Lit (G.String (r, tok)) :: args' ->
      List.fold_left
        (fun res e ->
          match (res, e) with
          | G.Lit (G.String (r, tok)), G.Lit (G.String (s, _)) ->
              G.Lit (literal_of_string ~tok (r ^ s))
          | (G.Lit _ | G.Cst _), G.Cst G.Cstr -> G.Cst G.Cstr
          | _____else_____ -> G.NotCst)
        (G.Lit (literal_of_string ~tok r))
        args'
  | ___else___ -> G.NotCst

(*****************************************************************************)
(* Symbolic evaluation *)
(*****************************************************************************)

(* Defines the subset of Generic expressions that we can propagate. *)
let rec is_symbolic_expr expr =
  match expr.G.e with
  | G.L _ -> true
  | G.N _ -> true
  | G.IdSpecial _ -> true
  | G.DotAccess (e, _, FN _) -> is_symbolic_expr e
  | G.ArrayAccess (e1, (_, e2, _)) -> is_symbolic_expr e1 && is_symbolic_expr e2
  | G.Call (e, (_, args, _)) ->
      is_symbolic_expr e && List.for_all is_symbolic_arg args
  | _ -> false

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
  let ( let* ) = Option.bind in
  VarMap.merge (fun _ c1_opt c2_opt ->
      let* c1 = c1_opt in
      let* c2 = c2_opt in
      Some (union c1 c2))

let input_env enter_env (flow : F.cfg) mapping ni _config =
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
  | G.NotCst -> D.VarMap.remove (str_of_name var) env
  | __else__ -> D.VarMap.add (str_of_name var) sval env

let transfer :
    lang:Lang.t ->
    flow:F.cfg ->
    'a Dataflow_core.env ->
    G.svalue Dataflow_core.transfn =
 fun ~lang ~flow
     (* the transfer function to update the mapping at node index ni *)
       env _mapping ni ->
  let node = flow.graph#nodes#assoc ni in

  let inp' = env in

  let out' =
    match node.F.n with
    | Enter
    | Exit
    | TrueNode
    | FalseNode
    | Join
    | NCond _
    | NGoto _
    | NReturn _
    | NThrow _
    | NOther _
    | NFunc _
    | NClass _
    | NModule _
    | NTodo _ ->
        inp'
    | NInstr instr -> (
        (* TODO: For now we only handle the simplest cases. *)
        match instr.i with
        | Assign ({ base = Var var; offset = NoOffset }, exp) ->
            (* var = exp *)
            let cexp = eval_or_sym_prop inp' exp in
            update_env_with inp' var cexp
        | Call (Some { base = Var var; offset = NoOffset }, func, args) ->
            let args_val = Common.map (eval inp') args in
            if result_of_function_call_is_constant lang func args_val then
              D.VarMap.add (str_of_name var) (G.Cst G.Cstr) inp'
            else
              (* symbolic propagation *)
              (* Call to an arbitrary function, we are intraprocedural so we cannot
                 * propagate actual constants in this case, but we can propagate the
                 * call itself as a symbolic expression. *)
              let ccall = sym_prop instr.iorig in
              update_env_with inp' var ccall
        | CallSpecial
            (Some { base = Var var; offset = NoOffset }, (Concat, _), args) ->
            (* var = concat(args) *)
            let cexp = eval_concat inp' args in
            update_env_with inp' var cexp
        | Call (None, { e = Fetch { base = Var var; offset = Dot _; _ }; _ }, _)
          ->
            (* Method call `var.f(args)` that returns void, we conservatively
               * assume that it may be updating `var`; e.g. in Ruby strings are
               * mutable. *)
            D.VarMap.remove (str_of_name var) inp'
        | ___else___ -> (
            (* In any other case, assume non-constant.
               * This covers e.g. `x.f = E`, `x[E1] = E2`, `*x = E`, etc. *)
            let lvar_opt = LV.lvar_of_instr_opt instr in
            match lvar_opt with
            | None -> inp'
            | Some lvar -> D.VarMap.remove (str_of_name lvar) inp'))
  in

  { D.in_env = inp'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec (fixpoint : Lang.t -> IL.name list -> F.cfg -> mapping) =
 fun lang _inputs flow ->
  let enter_env = D.VarMap.empty in
  DataflowX.fixpoint ~enter_env ~eq
    ~init:(DataflowX.new_node_array flow (Dataflow_core.empty_inout ()))
    ~trans:(fun _name flow env -> transfer ~lang ~flow env)
      (* svalue is a forward analysis! *)
    ~forward:true ~flow ~meet:input_env
    ~modify_env:(fun env node _ ->
      match node.n with
      | NFunc _ -> VarMap.empty
      | _ -> env)
    ~config:() ~name:None ~conclude:update_svalue

and update_svalue (flow : F.cfg) mapping =
  let for_all_id_info : (G.id_info -> bool) -> G.any -> bool =
    (* Check that all id_info's satisfy a given condition. We use refs so that
     * we can have a single visitor for all calls, given that `mk_visitor` is
     * pretty expensive. *)
    let ff = ref (fun _ -> true) in
    let ok = ref true in
    let hooks =
      {
        Visitor_AST.default_visitor with
        kid_info =
          (fun (_k, _vout) ii ->
            ok := !ok && !ff ii;
            if not !ok then raise Exit);
      }
    in
    let vout = Visitor_AST.mk_visitor hooks in
    fun f ast ->
      ff := f;
      ok := true;
      try
        vout ast;
        !ok
      with
      | Exit -> false
  in
  let no_cycles var c =
    (* Check that `c' contains no reference to `var'. It can contain references
     * to other occurrences of `var', but not to the same occurrence (that would
     * be a cycle), and each occurence must have its own `id_svalue` ref. This
     * is not supposed to happen, but if it does happen by accident then it would
     * cause an infinite loop, stack overflow, or segfault later on. *)
    match c with
    | G.Sym e ->
        for_all_id_info
          (fun ii ->
            (* Note the use of physical equality, we are looking for the *same*
             * id_svalue ref, that tells us it's the same variable occurrence. *)
            var.id_info.id_svalue != ii.id_svalue)
          (G.E e)
    | G.NotCst
    | G.Cst _
    | G.Lit _ ->
        true
  in
  flow.graph#nodes#keys
  |> List.iter (fun ni ->
         let ni_info = mapping.(ni) in

         let node = flow.graph#nodes#assoc ni in

         (* Update RHS svalue according to the input env. *)
         LV.rlvals_of_node_kind node.n
         |> List.iter (function
              | { base = Var var; _ } -> (
                  match
                    D.VarMap.find_opt (str_of_name var) ni_info.D.in_env
                  with
                  | None -> ()
                  | Some c ->
                      if no_cycles var c then
                        refine_svalue_ref var.id_info.id_svalue c
                      else
                        logger#error "Cycle check failed for %s -> %s"
                          (str_of_name var) (G.show_svalue c))
              | ___else___ -> ())
         (* Should not update the LHS svalue since in x = E, x is a "ref",
          * and it should not be substituted for the value it holds. *))
