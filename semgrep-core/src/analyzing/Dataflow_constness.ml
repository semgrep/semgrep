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
module D = Dataflow
module VarMap = Dataflow.VarMap

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* map for each node/var whether a variable is constant *)
type mapping = G.constness Dataflow.mapping

module DataflowX = Dataflow.Make (struct
  type node = F.node

  type edge = F.edge

  type flow = (node, edge) CFG.t

  let short_string_of_node n = Display_IL.short_string_of_node_kind n.F.n
end)

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let warning tok s =
  (* TODO: Report these errors as matches of a builtin_div_by_zero rule. *)
  Error_code.warning tok (Error_code.CFGError s)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_ctype = function
  | G.Cbool -> "bool"
  | G.Cint -> "int"
  | G.Cstr -> "str"
  | G.Cany -> "???"

let string_of_constness = function
  | G.NotCst -> "dyn"
  | G.Cst t -> Printf.sprintf "cst(%s)" (string_of_ctype t)
  | G.Lit l -> (
      match l with
      | G.Bool (b, _) -> Printf.sprintf "lit(%b)" b
      | G.Int (Some i, _) -> Printf.sprintf "lit(%d)" i
      | G.String (s, _) -> Printf.sprintf "lit(\"%s\")" s
      | ___else___ -> "lit(???)")

let str_of_name name = spf "%s:%d" (fst name.ident) name.sid

(*****************************************************************************)
(* Constness *)
(*****************************************************************************)

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
  | G.NotCst, G.NotCst -> true
  | ___else___ -> false

let union_ctype t1 t2 = if eq_ctype t1 t2 then t1 else G.Cany

let union c1 c2 =
  match (c1, c2) with
  | _ when eq c1 c2 -> c1
  | _any, G.NotCst
  | G.NotCst, _any ->
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
  | G.Cst _, G.Cst _ ->
      c1
  | G.Cst _, G.Lit _ -> c2

let refine_constness_ref c_ref c' =
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

let literal_of_string s =
  (* TODO: use proper token when possible? *)
  let tok = Parse_info.unsafe_fake_info s in
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
        try G.Lit (literal_of_int (i1 / i2))
        with Division_by_zero ->
          warning tok "Found division by zero";
          G.Cst G.Cint)
  | ___else____ -> G.Cst G.Cint

let eval_binop_string op s1 s2 =
  match op with
  | G.Plus
  | G.Concat ->
      G.Lit (literal_of_string (s1 ^ s2))
  | __else__ -> G.Cst G.Cstr

let rec eval (env : G.constness D.env) exp : G.constness =
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
      match (!(x.id_info.id_constness), opt_c) with
      | None, None -> G.NotCst
      | Some c, None
      | None, Some c ->
          c
      | Some c1, Some c2 -> refine c1 c2)
  | ___else___ -> G.NotCst

and eval_op env wop args =
  let op, tok = wop in
  let cs = List.map (eval env) args in
  match (op, cs) with
  | G.Plus, [ c1 ] -> c1
  | op, [ G.Lit (G.Bool (b, _)) ] -> eval_unop_bool op b
  | op, [ G.Lit (G.Int _ as li) ] -> eval_unop_int op (int_of_literal li)
  | op, [ G.Lit (G.Bool (b1, _)); G.Lit (G.Bool (b2, _)) ] ->
      eval_binop_bool op b1 b2
  | op, [ G.Lit (G.Int _ as li1); G.Lit (G.Int _ as li2) ] ->
      eval_binop_int tok op (int_of_literal li1) (int_of_literal li2)
  | op, [ G.Lit (G.String (s1, _)); G.Lit (G.String (s2, _)) ] ->
      eval_binop_string op s1 s2
  | _op, [ (G.Cst _ as c1) ] -> c1
  | _op, [ G.Cst t1; G.Cst t2 ] -> G.Cst (union_ctype t1 t2)
  | _op, [ G.Lit l1; G.Cst t2 ]
  | _op, [ G.Cst t2; G.Lit l1 ] ->
      let t1 = ctype_of_literal l1 in
      G.Cst (union_ctype t1 t2)
  | ___else___ -> G.NotCst

and eval_concat env args =
  args
  |> List.map (eval env)
  |> List.fold_left
       (fun res e ->
         match (res, e) with
         | G.Lit (G.String (r, _)), G.Lit (G.String (s, _)) ->
             G.Lit (literal_of_string (r ^ s))
         | (G.Lit _ | G.Cst _), G.Cst G.Cstr -> G.Cst G.Cstr
         | _____else_____ -> G.NotCst)
       (G.Lit (literal_of_string ""))

(*****************************************************************************)
(* Transfer *)
(*****************************************************************************)

(* FIXME: This takes "Bottom" as the default constness of a variable.
 *
 * E.g. in
 *
 *     def foo():
 *         if cond():
 *              x = "abc"
 *         return x
 *
 * we infer that `foo' returns the string "abc" when `x' may not even be defined!
 *
 * It would be more sound to assume "Top" (i.e., `G.NotCst`) as default, or
 * perhaps we could have a switch to control whether we want a may- or must-
 * analysis?
 *)
let union_env = Dataflow.varmap_union union

let transfer :
    enter_env:G.constness Dataflow.env ->
    flow:F.cfg ->
    G.constness Dataflow.transfn =
 fun ~enter_env ~flow
     (* the transfer function to update the mapping at node index ni *)
       mapping ni ->
  let node = flow.graph#nodes#assoc ni in

  let inp' =
    (* input mapping *)
    match node.F.n with
    | Enter -> enter_env
    | _else ->
        (flow.graph#predecessors ni)#fold
          (fun acc (ni_pred, _) -> union_env acc mapping.(ni_pred).D.out_env)
          VarMap.empty
  in

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
    | NTodo _ ->
        inp'
    | NInstr instr -> (
        (* TODO: For now we only handle the simplest cases. *)
        match instr.i with
        | Assign ({ base = Var var; offset = NoOffset }, exp) ->
            (* var = exp *)
            let cexp = eval inp' exp in
            D.VarMap.add (str_of_name var) cexp inp'
        | CallSpecial
            (Some { base = Var var; offset = NoOffset }, (Concat, _), args) ->
            (* var = concat(args) *)
            let cexp = eval_concat inp' args in
            D.VarMap.add (str_of_name var) cexp inp'
        | Call (None, { e = Fetch { base = Var var; offset = Dot _; _ }; _ }, _)
          ->
            (* Method call `var.f(args)` that returns void, we conservatively
             * assume that it may be updating `var`; e.g. in Ruby strings are
             * mutable. *)
            D.VarMap.add (str_of_name var) G.NotCst inp'
        | ___else___ -> (
            (* In any other case, assume non-constant.
             * This covers e.g. `x.f = E`, `x[E1] = E2`, `*x = E`, etc. *)
            let lvar_opt = IL.lvar_of_instr_opt instr in
            match lvar_opt with
            | None -> inp'
            | Some lvar -> D.VarMap.add (str_of_name lvar) G.NotCst inp'))
  in

  { D.in_env = inp'; out_env = out' }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let (fixpoint : IL.name list -> F.cfg -> mapping) =
 fun inputs flow ->
  let enter_env =
    inputs |> List.to_seq
    |> Seq.map (fun var -> (str_of_name var, G.NotCst))
    |> D.VarMap.of_seq
  in
  DataflowX.fixpoint ~eq
    ~init:(DataflowX.new_node_array flow (Dataflow.empty_inout ()))
    ~trans:(transfer ~enter_env ~flow) (* constness is a forward analysis! *)
    ~forward:true ~flow

let update_constness (flow : F.cfg) mapping =
  flow.graph#nodes#keys
  |> List.iter (fun ni ->
         let ni_info = mapping.(ni) in

         let node = flow.graph#nodes#assoc ni in

         (* Update RHS constness according to the input env. *)
         rlvals_of_node node.n
         |> List.iter (function
              | { base = Var var; _ } -> (
                  match
                    D.VarMap.find_opt (str_of_name var) ni_info.D.in_env
                  with
                  | None -> ()
                  | Some c -> refine_constness_ref var.id_info.id_constness c)
              | ___else___ -> ())
         (* Should not update the LHS constness since in x = E, x is a "ref",
          * and it should not be substituted for the value it holds. *))
