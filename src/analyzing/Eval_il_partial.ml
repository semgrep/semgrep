(* Iago Abal
 *
 * Copyright (C) 2020 Semgrep Inc.
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
module G = AST_generic
module H = AST_generic_helpers
module Log = Log_analyzing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Constness evaluation on the IL.
 *
 * See Eval_generic_partial.ml for similar code but operating on the generic
 * AST.
 * See also Eval_generic.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = { lang : Lang.t; vars : G.svalue Dataflow_var_env.t }

let mk_env lang vars = { lang; vars }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let warning _tok s =
  (* TODO: Report these errors as matches of a builtin_div_by_zero rule. *)
  Log.warn (fun m -> m "CFGError: %s" s)

(*****************************************************************************)
(* Svalue Lattice *)
(*****************************************************************************)
let eq_literal l1 l2 = G.equal_literal l1 l2
let eq_ctype t1 t2 = t1 =*= t2

let eq (c1 : G.svalue) (c2 : G.svalue) : bool =
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
      let t1 = H.ctype_of_literal l1 and t2 = H.ctype_of_literal l2 in
      G.Cst (union_ctype t1 t2)
  | G.Lit l1, G.Cst t2
  | G.Cst t2, G.Lit l1 ->
      let t1 = H.ctype_of_literal l1 in
      G.Cst (union_ctype t1 t2)
  | G.Cst t1, G.Cst t2 -> G.Cst (union_ctype t1 t2)

(* THINK: This assumes that analyses are sound... but they are not
   (e.g. due to aliasing). *)
let refine (c1 : G.svalue) (c2 : G.svalue) : G.svalue =
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
(* Eval helpers *)
(*****************************************************************************)

let literal_of_bool b =
  let b_str = string_of_bool b in
  (* TODO: use proper token when possible? *)
  let tok = Tok.unsafe_fake_tok b_str in
  G.Bool (b, tok)

let literal_of_int i64 =
  (* TODO: use proper token when possible? *)
  G.Int (Parsed_int.of_int64 i64)

let int_of_literal = function
  | G.Int (opt, _) -> opt
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
  | G.Minus, Some i -> G.Lit (literal_of_int (Int64.neg i))
  | ___else____ -> G.Cst G.Cint

(* This reduces arithmetic "exceptions" to `G.Cst G.Cint`, it does NOT
 * detect overflows for any other language than OCaml. Note that OCaml
 * integers have just 63-bits in 64-bit architectures!
 *)
let eval_binop_int tok op opt_i1 opt_i2 =
  let open Int64_ in
  let sign_bit i = i asr Int.sub Sys.int_size 1 =|= 1L in
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
        i1 <> 0L && i2 <> 0L
        && ((i1 < 0L && i2 =|= min_int) (* >max_int *)
           || (i1 =|= min_int && i2 < 0L) (* >max_int *)
           ||
           if sign_bit i1 =:= sign_bit i2 then abs i1 > abs (max_int / i2)
             (* >max_int *)
           else abs i1 > abs (min_int / i2) (* <min_int *))
      in
      if overflow then G.Cst G.Cint else G.Lit (literal_of_int (i1 * i2))
  | G.Div, Some i1, Some i2 -> (
      if i1 =|= min_int && i2 =|= -1L then
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec eval (env : env) (exp : IL.exp) : G.svalue =
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
  | RecordOrDict _
  | Cast _
  | FixmeExp _ ->
      G.NotCst

and eval_lval env lval =
  match lval with
  | { base = Var x; rev_offset = [] } -> (
      let opt_c =
        Dataflow_var_env.VarMap.find_opt (IL.str_of_name x) env.vars
      in
      match (!(x.id_info.id_svalue), opt_c) with
      | None, None -> G.NotCst
      | Some c, None
      | None, Some c ->
          c
      | Some c1, Some c2 -> refine c1 c2)
  | ___else___ -> G.NotCst

and eval_op env wop args =
  let op, tok = wop in
  let cs = args |> List_.map IL_helpers.exp_of_arg |> List_.map (eval env) in
  match (op, cs) with
  | G.Plus, [ c1 ] -> c1
  | op, [ G.Lit (G.Bool (b, _)) ] -> eval_unop_bool op b
  | op, [ G.Lit (G.Int _ as li) ] -> eval_unop_int op (int_of_literal li)
  | G.And, [ G.Lit (G.Bool (true, _)); c ] when Lang.equal env.lang Lang.Python
    ->
      c (* Python: True and 42 -> 42 *)
  | G.Or, [ G.Lit (G.Bool (false, _)); c ] when Lang.equal env.lang Lang.Python
    ->
      c (* Python: False or 42 -> 42 *)
  | op, [ G.Lit (G.Bool (b1, _)); G.Lit (G.Bool (b2, _)) ] ->
      eval_binop_bool op b1 b2
  | op, [ G.Lit (G.Int _ as li1); G.Lit (G.Int _ as li2) ] ->
      eval_binop_int tok op (int_of_literal li1) (int_of_literal li2)
  | op, [ G.Lit (G.String (_, (s1, _), _)); G.Lit (G.String (_, (s2, _), _)) ]
    ->
      eval_binop_string ~tok op s1 s2
  | G.Mult, [ (G.Lit (G.String _) | G.Cst G.Cstr); _N ]
    when Lang.equal env.lang Lang.Python ->
      (* Python: "..." * N, NOTE that we don't check the type of N, partly because
         * we lack good type inference for Python, but should be fine. *)
      G.Cst G.Cstr
  | _op, [ (G.Cst _ as c1) ] -> c1
  | _op, [ G.Cst t1; G.Cst t2 ] -> G.Cst (union_ctype t1 t2)
  | _op, [ G.Lit l1; G.Cst t2 ]
  | _op, [ G.Cst t2; G.Lit l1 ] ->
      let t1 = H.ctype_of_literal l1 in
      G.Cst (union_ctype t1 t2)
  | ___else___ -> G.NotCst

let eval_concat (env : env) args =
  match List_.map (eval env) args with
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
