(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
open Core_jsonnet
module A = AST_jsonnet
module V = Value_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = unit

exception Error of string * Parse_info.t

(* -1, 0, 1 *)
type cmp = Inf | Eq | Sup

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let sv e = V.show_value_ e
let todo _env _v = failwith "TODO"
let eval_string _env v = v
let eval_list f env x = x |> Common.map (fun x -> f env x)

(*****************************************************************************)
(* Evaluator *)
(*****************************************************************************)

let eval_tok _env v = v

let eval_wrap ofa env (v1, v2) =
  let v1 = ofa env v1 in
  (v1, v2)

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let eval_ident _env v = v

let string_of_string (x : A.string_) : string A.wrap =
  let _verbatimTODO, _kindTODO, (l, xs, r) = x in
  let str = xs |> Common.map fst |> String.concat "" in
  let infos = xs |> Common.map snd in
  let tk = Parse_info.combine_infos l (infos @ [ r ]) in
  (str, tk)

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

let rec eval_expr (env : env) (v : expr) : V.value_ =
  match v with
  | L v ->
      let prim =
        match v with
        | A.Null tk -> V.Null tk
        | A.Bool (b, tk) -> V.Bool (b, tk)
        | A.Str x -> V.Str (string_of_string x)
        | A.Number (s, tk) ->
            (* TODO: double check things *)
            let f = float_of_string s in
            V.Double (f, tk)
      in
      V.Primitive prim
  (* lazy evaluation of Array and Functions *)
  | Array (l, xs, r) -> V.Array (l, Array.of_list xs, r)
  | Lambda v ->
      let v = eval_function_definition env v in
      V.Function v
  | O v ->
      let v = (eval_bracket eval_obj_inside) env v in
      todo env v
  | Id v ->
      let v = (eval_wrap eval_string) env v in
      todo env v
  | IdSpecial v ->
      let v = (eval_wrap eval_special) env v in
      todo env v
  | Local (v1, v2, v3, v4) ->
      let v1 = eval_tok env v1 in
      let v2 = (eval_list eval_bind) env v2 in
      let v3 = eval_tok env v3 in
      let v4 = eval_expr env v4 in
      todo env (v1, v2, v3, v4)
  | ArrayAccess (v1, v2) -> (
      let e = eval_expr env v1 in
      let l, e', _r = (eval_bracket eval_expr) env v2 in
      match (e, e') with
      | V.Array (_l, arr, _r), V.Primitive (V.Double (f, tkf)) ->
          if Float.is_integer f then
            let i = int_of_float f in
            match i with
            | _ when i < 0 ->
                error tkf (spf "negative value for array index: %s" (sv e'))
            | _ when i >= 0 && i < Array.length arr ->
                let ei = arr.(i) in
                eval_expr env ei
            | _else_ ->
                error tkf (spf "Out of bound for array index: %s" (sv e'))
          else error tkf (spf "Not an integer: %s" (sv e'))
      | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv e')))
  | Call (v1, v2) ->
      let v1 = eval_expr env v1 in
      let v2 = (eval_bracket (eval_list eval_argument)) env v2 in
      todo env (v1, v2)
  | UnaryOp ((op, tk), e) -> (
      match op with
      | UBang -> (
          match eval_expr env e with
          | V.Primitive (V.Bool (b, tk)) -> V.Primitive (V.Bool (not b, tk))
          | v -> error tk (spf "Not a boolean for !: %s" (sv v)))
      | UPlus
      | UMinus
      | UTilde ->
          todo env ())
  | BinaryOp (el, (op, tk), er) -> (
      match op with
      | Plus -> (
          match (eval_expr env el, eval_expr env er) with
          | V.Array (l1, arr1, _r1), V.Array (_l2, arr2, r2) ->
              V.Array (l1, Array.append arr1 arr2, r2)
          | _else_ -> todo env ())
      | And -> (
          match eval_expr env el with
          | V.Primitive (V.Bool (b, _)) as v ->
              if b then eval_expr env er else v
          | v -> error tk (spf "Not a boolean for &&: %s" (sv v)))
      | Or -> (
          match eval_expr env el with
          | V.Primitive (V.Bool (b, _)) as v ->
              if b then v else eval_expr env er
          | v -> error tk (spf "Not a boolean for ||: %s" (sv v)))
      | Lt
      | LtE
      | Gt
      | GtE ->
          let cmp = eval_std_cmp env tk el er in
          let bool =
            match (op, cmp) with
            | Lt, Inf -> true
            | Lt, (Eq | Sup) -> false
            | LtE, (Inf | Eq) -> true
            | LtE, Sup -> true
            | Gt, (Inf | Eq) -> false
            | Gt, Sup -> true
            | GtE, Inf -> false
            | GtE, (Eq | Sup) -> true
            | ( ( Plus | Minus | Mult | Div | LSL | LSR | And | Or | BitAnd
                | BitOr | BitXor ),
                _ ) ->
                assert false
          in
          Primitive (Bool (bool, tk))
      | Minus
      | Mult
      | Div
      | LSL
      | LSR
      | BitAnd
      | BitOr
      | BitXor ->
          todo env ())
  | If (tif, e1, e2, e3) -> (
      match eval_expr env e1 with
      | V.Primitive (V.Bool (b, _)) ->
          if b then eval_expr env e2 else eval_expr env e3
      | v -> error tif (spf "not a boolean for if: %s" (sv v)))
  | Error (v1, v2) ->
      let v1 = eval_tok env v1 in
      let v2 = eval_expr env v2 in
      todo env (v1, v2)

and eval_special env v =
  match v with
  | Self -> todo env
  | Super -> todo env

and eval_argument env v =
  match v with
  | Arg v ->
      let v = eval_expr env v in
      todo env v
  | NamedArg (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)

and eval_bind env v =
  match v with
  | B (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)

and eval_function_definition _env v = v

(*
and eval_parameter env v =
  match v with
  | P (v1, v2, v3) ->
      let v1 = eval_ident env v1 in
      let v2 = eval_tok env v2 in
      let v3 = eval_expr env v3 in
      todo env (v1, v2, v3)
*)

(*****************************************************************************)
(* std.cmp *)
(*****************************************************************************)
(* Seems like std.cmp() is not defined in std.jsonnet nor mentionned in
 * the Jsonnet Standard library spec, so I guess it's a hidden builtin
 * so we dont need to produce a value_ that other code can use; we can
 * return a cmp.
 *)
and eval_std_cmp env tk (el : expr) (er : expr) : cmp =
  let rec eval_std_cmp_value_ (v_el : V.value_) (v_er : V.value_) : cmp =
    match (v_el, v_er) with
    | V.Array (_, [||], _), V.Array (_, [||], _) -> Eq
    | V.Array (_, [||], _), V.Array (_, _, _) -> Inf
    | V.Array (_, _, _), V.Array (_, [||], _) -> Sup
    | V.Array (al, ax, ar), V.Array (bl, bx, br) -> (
        match eval_std_cmp env tk ax.(0) bx.(0) with
        | (Inf | Sup) as r -> r
        | Eq ->
            let a_sub =
              V.Array (al, Array.sub ax 1 (Array.length ax - 1), ar)
            in
            let b_sub =
              V.Array (bl, Array.sub bx 1 (Array.length bx - 1), br)
            in
            eval_std_cmp_value_ a_sub b_sub)
    | V.Primitive (V.Double (fl, _)), V.Primitive (V.Double (fr, _)) ->
        Float.compare fl fr |> int_to_cmp
    | V.Primitive (V.Str (strl, _)), V.Primitive (V.Str (strr, _)) ->
        (* TODO? or use unicode? *)
        String.compare strl strr |> int_to_cmp
    (* note that it does not make sense to compare (<, <=, >=, >) 2 booleans
     * or 2 nulls. They are not ordonnable
     *)
    | _else_ ->
        error tk (spf "comparing uncomparable: %s vs %s" (sv v_el) (sv v_er))
  in
  eval_std_cmp_value_ (eval_expr env el) (eval_expr env er)

(*****************************************************************************)
(* eval_obj_inside *)
(*****************************************************************************)

and eval_obj_inside env v =
  match v with
  | Object (v1, v2) ->
      let v1 = (eval_list eval_obj_assert) env v1 in
      let v2 = (eval_list eval_field) env v2 in
      todo env (v1, v2)
  | ObjectComp v ->
      let v = eval_obj_comprehension env v in
      todo env v

and eval_obj_assert env v =
  (fun env (v1, v2) ->
    let v1 = eval_tok env v1 in
    let v2 = eval_expr env v2 in
    todo env (v1, v2))
    env v

and eval_field env _v = todo env "TODO: field"

and eval_field_name env v =
  match v with
  | FExpr v ->
      let v = (eval_bracket eval_expr) env v in
      todo env v

and eval_obj_comprehension env v =
  (fun env (v1, v2, v3, v4) ->
    let v1 = eval_field_name env v1 in
    let v2 = eval_tok env v2 in
    let v3 = eval_expr env v3 in
    let v4 = eval_for_comp env v4 in
    todo env (v1, v2, v3, v4))
    env v

and eval_for_comp env v =
  (fun env (v1, v2, v3, v4) ->
    let v1 = eval_tok env v1 in
    let v2 = eval_ident env v2 in
    let v3 = eval_tok env v3 in
    let v4 = eval_expr env v4 in
    todo env (v1, v2, v3, v4))
    env v

(*****************************************************************************)
(* Manifestation *)
(*****************************************************************************)
(* We can't define manifestation in a separate module because
 * it's mutually recursive with the evaluator
 *
 * See https://jsonnet.org/ref/spec.html#manifestation
 *)
and manifest_value (_v : Value_jsonnet.value_) : JSON.t = failwith "TODO"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval_program (e : Core_jsonnet.program) : Value_jsonnet.value_ =
  let env = () in
  let v = eval_expr env e in
  v
