(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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

let src = Logs.Src.create "ojsonnet.eval"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Code common to Eval_jsonnet_envir.ml and Eval_jsonnet_subst.ml *)

let debug = false
let extra_debug = false

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

exception Error of string * Tok.t

(* -1, 0, 1 *)
type cmp = Inf | Eq | Sup

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let fk = Tok.unsafe_fake_tok ""

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let sv v =
  let s = V.show v in
  if String.length s > 100 then Str.first_chars s 100 ^ "..." else s

(* note that this can be really slow when you use Std_jsonnet.ml *)
let show_env (env : V.env) : string =
  if extra_debug then V.show_env env else "<turn debug on>"

let show_lazy_value (lv : V.lazy_value) : string =
  if extra_debug then V.show_lazy_value lv else "<turn debug on>"

let string_of_local_id = function
  | V.LSelf -> "self"
  | V.LSuper -> "super"
  | V.LId s -> s

let str_of_caller (e0 : expr) : string =
  match e0 with
  | Id (s, _) -> s
  | ArrayAccess
      (Id (obj, _), (_, L (Str (None, DoubleQuote, (_, [ (meth, _) ], _))), _))
    ->
      spf "%s.%s" obj meth
  | _else_ -> "<unknown>"

let short_string_of_value (v : V.t) : string =
  match v with
  | V.Primitive x -> (
      match x with
      | Null _ -> "null"
      | Bool (b, _) -> spf "%b" b
      | Double (f, _) ->
          if Float.is_integer f then spf "%d" (int_of_float f)
          else spf "%0.2f" f
      | Str (s, _) -> spf "\"%s\"" s)
  | V.Lambda _ -> "<lambda>"
  | V.Array _ -> "<array>"
  | V.Object _ -> "<object>"

let debug_call (env : V.env) (e0 : expr) (l, args, _r) : unit =
  if not env.in_debug_call then
    Log.debug (fun m ->
        let env = { env with in_debug_call = true } in
        let fstr = str_of_caller e0 in
        m "%s> %s(%s) at %s"
          (Common2.repeat "-" env.depth |> String.concat "")
          fstr
          (args
          |> List_.map (fun arg ->
                 try
                   match arg with
                   | Arg e ->
                       let v = env.eval_expr env e in
                       short_string_of_value v
                   | NamedArg (id, _tk, e) ->
                       let v = env.eval_expr env e in
                       spf "%s=%s" (fst id) (short_string_of_value v)
                   (* in theory arguments are evaluated lazily, see
                    * pass/short_circuit_func.jsonnet, so turning debug
                    * on which forces the evaluation of all arguments can
                    * trigger more Error
                    *)
                 with
                 | Error _ -> "<error>")
          |> String.concat ", ")
          (Tok.stringpos_of_tok l))

let debug_ret (env : V.env) (e0 : expr) (_l, _args, _r) (retv : V.t) : unit =
  if not env.in_debug_call then
    Log.debug (fun m ->
        let fstr = str_of_caller e0 in
        m "%s< %s(...) = %s"
          (Common2.repeat "-" env.depth |> String.concat "")
          fstr
          (short_string_of_value retv))

(*****************************************************************************)
(* Call *)
(*****************************************************************************)
let eval_call_ (env : V.env) (e0 : expr) (largs, args, _rargs) =
  match env.eval_expr_for_call env e0 with
  | Lambda
      ({ f_tok = _; f_params = lparams, params, rparams; f_body = eb }, locals)
    ->
      (* the named_args are supposed to be the last one *)
      let basic_args, named_args =
        args
        |> Either_.partition (function
             | Arg ei -> Left ei
             | NamedArg (id, _tk, ei) -> Right (fst id, ei))
      in
      (* opti? use a hashtbl? but for < 5 elts, probably worse? *)
      let hnamed_args = Hashtbl_.hash_of_list named_args in
      let basic_args = Array.of_list basic_args in
      let m = Array.length basic_args in
      let binds =
        params
        |> List_.mapi (fun i (P (id, teq, ei')) ->
               let ei'' =
                 match i with
                 | _ when i < m -> basic_args.(i) (* ei *)
                 | _ when Hashtbl.mem hnamed_args (fst id) ->
                     Hashtbl.find hnamed_args (fst id)
                 | _else_ -> ei'
               in
               B (id, teq, ei''))
      in
      let start = env.locals in
      let locals = Map_.fold (fun k v acc -> Map_.add k v acc) locals start in
      env.eval_expr
        { env with depth = env.depth + 1; locals }
        (Local (lparams, binds, rparams, eb))
  | v -> error largs (spf "not a function: %s" (sv v))

let eval_call (env : V.env) (e0 : expr) (l, args, r) : V.t =
  debug_call env e0 (l, args, r);
  let v = eval_call_ env e0 (l, args, r) in
  debug_ret env e0 (l, args, r) v;
  v

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

(* alt: could move to Value_jsonnet.ml *)
let std_type (v : V.t) : string =
  match v with
  | V.Primitive (Null _) -> "null"
  | V.Primitive (Bool _) -> "boolean"
  | V.Primitive (Double _) -> "number"
  | V.Primitive (Str _) -> "string"
  | V.Object _ -> "object"
  | V.Array _ -> "array"
  | V.Lambda _ -> "function"

let std_primivite_equals (v : V.t) (v' : V.t) : bool =
  match (v, v') with
  | Primitive p, Primitive p' -> (
      match (p, p') with
      (* alt: use deriving and Primitive.eq *)
      | Null _, Null _ -> true
      | Bool (b, _), Bool (b', _) -> b =:= b'
      | Str (s, _), Str (s', _) -> s = s'
      | Double (f, _), Double (f', _) -> f =*= f'
      | Null _, _
      | Bool _, _
      | Str _, _
      | Double _, _ ->
          false)
  (* Should we raise an exn if one of the value is not a primitive?
   * No, the spec seems to not restrict what v and v' can be.
   *)
  | _else_ -> false

(* Seems like std.cmp() is not defined in std.jsonnet nor mentionned in
 * the Jsonnet Standard library spec, so I guess it's a hidden builtin
 * so we dont need to produce a value_ that other code can use; we can
 * return a cmp.
 *)
let eval_std_cmp (env : V.env) tk (el : expr) (er : expr) : cmp =
  let rec eval_std_cmp_value_aux (v_el : V.t) (v_er : V.t) : cmp =
    match (v_el, v_er) with
    | V.Array (_, [||], _), V.Array (_, [||], _) -> Eq
    | V.Array (_, [||], _), V.Array (_, _, _) -> Inf
    | V.Array (_, _, _), V.Array (_, [||], _) -> Sup
    | V.Array (al, ax, ar), V.Array (bl, bx, br) -> (
        let a0 = env.to_value env ax.(0) in

        let b0 = env.to_value env bx.(0) in

        match eval_std_cmp_value_aux a0 b0 with
        | (Inf | Sup) as r -> r
        | Eq ->
            let a_sub =
              V.Array (al, Array.sub ax 1 (Array.length ax - 1), ar)
            in
            let b_sub =
              V.Array (bl, Array.sub bx 1 (Array.length bx - 1), br)
            in
            eval_std_cmp_value_aux a_sub b_sub)
    | Primitive (Double (fl, _)), Primitive (Double (fr, _)) ->
        Float.compare fl fr |> int_to_cmp
    | Primitive (Str (strl, _)), Primitive (Str (strr, _)) ->
        (* TODO? or use unicode? *)
        String.compare strl strr |> int_to_cmp
    (* note that it does not make sense to compare (<, <=, >=, >) 2 booleans
     * or 2 nulls. They are not ordonnable
     *)
    | _else_ ->
        error tk (spf "comparing uncomparable: %s vs %s" (sv v_el) (sv v_er))
  in
  eval_std_cmp_value_aux (env.eval_expr env el) (env.eval_expr env er)

let eval_std_method_ (env : V.env) (e0 : expr) (method_str, tk) (l, args, r) =
  match (method_str, args) with
  | "type", [ Arg e ] ->
      let v = env.eval_expr env e in
      let s = std_type v in
      V.Primitive (V.Str (s, l))
  (* this method is called in std.jsonnet equals()::, and calls to
   * this equals() are generated in Desugar_jsonnet when
   * desugaring the == operator.
   *)
  | "type", _else_ ->
      error tk
        (spf "Improper #arguments to std.type: expected 1, got %d"
           (List.length args))
  | "primitiveEquals", [ Arg e; Arg e' ] ->
      let v = env.eval_expr env e in
      let v' = env.eval_expr env e' in
      let b = std_primivite_equals v v' in
      V.Primitive (V.Bool (b, l))
  | "primitiveEquals", _else_ ->
      error tk
        (spf "Improper #arguments to std.primitiveEquals: expected 2, got %d"
           (List.length args))
  | "length", [ Arg e ] -> (
      match env.eval_expr env e with
      | V.Primitive (V.Str (s, tk)) ->
          let i = String.length s in
          V.Primitive (V.Double (float_of_int i, tk))
      | V.Array (_, arr, _) ->
          let i = Array.length arr in
          V.Primitive (V.Double (float_of_int i, tk))
      | V.Object (_, (_asserts, flds), _) ->
          let i = List.length flds in
          (* TODO: in the spec they use std.objectFieldsEx *)
          V.Primitive (V.Double (float_of_int i, tk))
      | v ->
          error l
            (spf "length operates on strings, objects, and arrays, got %s"
               (sv v)))
  | "makeArray", [ Arg e; Arg e' ] -> (
      match (env.eval_expr env e, env.eval_expr env e') with
      | V.Primitive (V.Double (n, tk)), V.Lambda (fdef, locals) ->
          if Float.is_integer n then
            let n = Float.to_int n in
            let e i =
              Call
                ( Lambda fdef,
                  (fk, [ Arg (L (Number (string_of_int i, fk))) ], fk) )
            in
            Array
              ( fk,
                Array.init n (fun i ->
                    env.to_lazy_value { env with locals } (e i)),
                fk )
          else error tk (spf "Got non-integer %f in std.makeArray" n)
      | v, _e' ->
          error tk (spf "Improper arguments to std.makeArray: %s" (sv v)))
  | "makeArray", _else_ ->
      error tk
        (spf "Improper number of arguments to std.makeArray: expected 2, got %d"
           (List.length args))
  | "filter", [ Arg e; Arg e' ] -> (
      match (env.eval_expr env e, env.eval_expr env e') with
      | Lambda (f, locals), Array (l, eis, r) ->
          (* note that we do things lazily even here, so we still
           * return an Array with the same lazy value elements in it,
           * but just filtered
           *)
          let elts' =
            (* TODO? use Array.to_seqi instead? *)
            eis |> Array.to_list |> List_.index_list
            |> List_.filter_map (fun (ei, ji) ->
                   match
                     env.eval_std_filter_element { env with locals } tk f ei
                   with
                   | Primitive (Bool (false, _)), _ -> None
                   | Primitive (Bool (true, _)), _ -> Some ji
                   | v ->
                       error tk
                         (spf "filter function must return boolean, got: %s"
                            (sv (fst v))))
            |> Array.of_list
            |> Array.map (fun idx -> eis.(idx))
          in
          Array (l, elts', r)
      | v1, v2 ->
          error tk
            (spf
               "Builtin function filter expected (function, array) but got \
                (%s, %s)"
               (sv v1) (sv v2)))
  | "filter", _else_ ->
      error tk
        (spf "Improper number of arguments to std.filter: expected 2, got %d"
           (List.length args))
  | "objectHasEx", [ Arg e; Arg e'; Arg e'' ] -> (
      match
        (env.eval_expr env e, env.eval_expr env e', env.eval_expr env e'')
      with
      | V.Object o, Primitive (Str (s, _)), Primitive (Bool (b, _)) ->
          let _, (_asserts, flds), _ = o in
          let eltopt =
            flds |> List.find_opt (fun { V.fld_name; _ } -> fst fld_name = s)
          in
          let b =
            match eltopt with
            | None -> false
            | Some { fld_hidden = visibility, _; _ } ->
                visibility <> A.Hidden || b
          in
          Primitive (Bool (b, tk))
      | v1, v2, v3 ->
          error tk
            (spf
               "Builtin function objectHasEx expected (object, string, \
                boolean), got (%s, %s, %s)"
               (sv v1) (sv v2) (sv v3)))
  | "objectHasEx", _else_ ->
      error tk
        (spf
           "Improper number of arguments to std.objectHasEx: expected 3, got %d"
           (List.length args))
  (* math builtin/1 *)
  | ("floor" as op), [ Arg e ] -> (
      match env.eval_expr env e with
      | V.Primitive (V.Double (f, tk)) ->
          let float_op =
            match op with
            | "floor" -> floor
            | _else_ -> raise Impossible
          in
          V.Primitive (V.Double (float_op f, tk))
      | v1 ->
          error tk
            (spf "Builtin function %s expected (number) but got (%s)" op (sv v1))
      )
  (* math builtin/2 *)
  | ("modulo" as op), [ Arg e1; Arg e2 ] -> (
      match (env.eval_expr env e1, env.eval_expr env e2) with
      | V.Primitive (V.Double (f1, tk1)), V.Primitive (V.Double (f2, _tk2)) ->
          let float_op =
            match op with
            | "modulo" -> mod_float
            | _else_ -> raise Impossible
          in
          V.Primitive (V.Double (float_op f1 f2, tk1))
      | v1, v2 ->
          error tk
            (spf
               "Builtin function %s expected (number, number) but got (%s, %s)"
               op (sv v1) (sv v2)))
  (* Default to regular call, handled by std.jsonnet code hopefully.
   * Note that we call eval_call_() here, not eval_call(), to avoid double
   * trace.
   *)
  | _else_ -> eval_call_ env e0 (l, args, r)

let eval_std_method (env : V.env) (e0 : expr) (method_str, tk) (l, args, r) =
  debug_call env e0 (l, args, r);
  let v = eval_std_method_ env e0 (method_str, tk) (l, args, r) in
  debug_ret env e0 (l, args, r) v;
  v

let eval_unary_op (env : V.env) (op : unary_op wrap) (e : expr) : V.t =
  let op, tk = op in
  match op with
  | UBang -> (
      match env.eval_expr env e with
      | V.Primitive (Bool (b, tk)) -> V.Primitive (Bool (not b, tk))
      | v -> error tk (spf "Not a boolean for unary !: %s" (sv v)))
  | UPlus -> (
      match env.eval_expr env e with
      | V.Primitive (Double (f, tk)) -> V.Primitive (Double (f, tk))
      | v -> error tk (spf "Not a number for unary +: %s" (sv v)))
  | UMinus -> (
      match env.eval_expr env e with
      | V.Primitive (Double (f, tk)) -> V.Primitive (Double (-.f, tk))
      | v -> error tk (spf "Not a number for unary -: %s" (sv v)))
  | UTilde -> (
      match env.eval_expr env e with
      | V.Primitive (Double (f, tk)) ->
          let f = f |> Int64.of_float |> Int64.lognot |> Int64.to_float in
          V.Primitive (Double (f, tk))
      | v -> error tk (spf "Not a number for unary -: %s" (sv v)))

let eval_binary_op (env : V.env) (el : expr) (op, tk) (er : expr) =
  match op with
  | Plus -> (
      match (env.eval_expr env el, env.eval_expr env er) with
      | V.Array (l1, arr1, _r1), V.Array (_l2, arr2, r2) ->
          V.Array (l1, Array.append arr1 arr2, r2)
      | V.Primitive (Double (f1, tk)), V.Primitive (Double (f2, _)) ->
          V.Primitive (Double (f1 +. f2, tk))
      | V.Primitive (Str (s1, tk1)), V.Primitive (Str (s2, _tk2)) ->
          V.Primitive (Str (s1 ^ s2, tk1))
      | V.Primitive (Str (s, tk)), v ->
          V.Primitive (Str (s ^ env.tostring v, tk))
      | v, V.Primitive (Str (s, tk)) ->
          V.Primitive (Str (env.tostring v ^ s, tk))
      | V.Object objl, V.Object objr ->
          let obj = env.eval_plus_object env tk objl objr in
          V.Object obj
      | v1, v2 ->
          error tk (spf "TODO: Plus (%s, %s) not yet handled" (sv v1) (sv v2)))
  | And -> (
      match env.eval_expr env el with
      | V.Primitive (Bool (b, _)) as v -> if b then env.eval_expr env er else v
      | v -> error tk (spf "Not a boolean for &&: %s" (sv v)))
  | Or -> (
      match env.eval_expr env el with
      | V.Primitive (Bool (b, _)) as v -> if b then v else env.eval_expr env er
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
        | LtE, Sup -> false
        | Gt, (Inf | Eq) -> false
        | Gt, Sup -> true
        | GtE, Inf -> false
        | GtE, (Eq | Sup) -> true
        | ( ( Plus | Minus | Mult | Div | LSL | LSR | And | Or | BitAnd | BitOr
            | BitXor ),
            _ ) ->
            assert false
      in
      Primitive (Bool (bool, tk))
  | Minus
  | Mult
  | Div -> (
      match (env.eval_expr env el, env.eval_expr env er) with
      | Primitive (Double (f1, itk)), Primitive (Double (f2, _)) ->
          let op =
            match op with
            | Minus -> ( -. )
            | Mult -> ( *. )
            | Div -> ( /. )
            | _else_ -> assert false
          in
          Primitive (Double (op f1 f2, itk))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Tok.content_of_tok tk) (sv v2)))
  | LSL
  | LSR
  | BitAnd
  | BitOr
  | BitXor -> (
      let v1 = env.eval_expr env el in
      let v2 = env.eval_expr env er in
      match (v1, v2) with
      | Primitive (Double (f1, tk1)), Primitive (Double (f2, tk2)) ->
          let i1 = Int64.of_float f1 in
          let i2 = Int64.of_float f2 in
          let i64 =
            match op with
            | LSL ->
                let i2 = Int64.to_int i2 in
                if i2 < 0 then
                  error tk2 (spf "negative number for LSL: %s" (sv v2))
                else Int64.shift_left i1 i2
            | LSR ->
                let i2 = Int64.to_int i2 in
                if i2 < 0 then
                  error tk2 (spf "negative number for LSR: %s" (sv v2))
                else Int64.shift_right i1 i2
            | BitAnd -> Int64.logand i1 i2
            | BitOr -> Int64.logor i1 i2
            | BitXor -> Int64.logxor i1 i2
            | _else_ -> assert false
          in
          Primitive (Double (Int64.to_float i64, tk1))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Tok.content_of_tok tk) (sv v2)))

(*****************************************************************************)
(* Common evaluation *)
(*****************************************************************************)

let eval_literal _env lit =
  let prim =
    match lit with
    | A.Null tk -> V.Null tk
    | A.Bool (b, tk) -> V.Bool (b, tk)
    | A.Str x -> V.Str (A.string_of_string_ x)
    | A.Number (s, tk) ->
        (* TODO: double check things *)
        let f = float_of_string s in
        V.Double (f, tk)
  in
  V.Primitive prim
