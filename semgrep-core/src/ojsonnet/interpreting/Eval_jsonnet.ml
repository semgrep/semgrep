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
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
type env = {
  (* The spec uses a lambda-calculus inspired substitution model, but
   * it is probably simpler and more efficient to use a classic
   * environment where the locals are defined. Jsonnet uses lazy
   * evaluation so we model this by using Lazy below.
   *)
  locals : (local_id, V.value_ Lazy.t) Map_.t;
}

and local_id = LSelf | LSuper | LId of string

exception Error of string * Parse_info.t

(* -1, 0, 1 *)
type cmp = Inf | Eq | Sup

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let fk = Parse_info.unsafe_fake_info ""
let sv e = V.show_value_ e
let todo _env _v = failwith "TODO"

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

let string_of_local_id = function
  | LSelf -> "self"
  | LSuper -> "super"
  | LId s -> s

let lookup env tk local_id =
  let lzv =
    try Map_.find local_id env.locals with
    | Not_found ->
        error tk
          (spf "could not find '%s' in the environment"
             (string_of_local_id local_id))
  in
  Lazy.force lzv

let tostring (v : Value_jsonnet.value_) : string =
  let j = Manifest_jsonnet.manifest_value v in
  JSON.string_of_json j

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

let rec eval_expr env e =
  try eval_expr_aux env e with
  | Failure "TODO" ->
      pr2 (spf "TODO: construct not handled:\n %s" (show_expr e));
      failwith "TODO:eval"

and eval_expr_aux (env : env) (v : expr) : V.value_ =
  match v with
  | L v ->
      let prim =
        match v with
        | A.Null tk -> V.Null tk
        | A.Bool (b, tk) -> V.Bool (b, tk)
        | A.Str x -> V.Str (A.string_of_string_ x)
        | A.Number (s, tk) ->
            (* TODO: double check things *)
            let f = float_of_string s in
            V.Double (f, tk)
      in
      V.Primitive prim
  (* lazy evaluation of Array elements and Functions *)
  | Array (l, xs, r) ->
      let elts =
        xs |> Array.of_list
        |> Array.map (fun e ->
               let v = lazy (eval_expr env e) in
               { V.v; e })
      in
      V.Array (l, elts, r)
  | Lambda v -> V.Function v
  | O v -> eval_obj_inside env v
  | Id (s, tk) -> lookup env tk (LId s)
  | IdSpecial (Self, tk) -> lookup env tk LSelf
  | IdSpecial (Super, tk) -> lookup env tk LSuper
  (* Should not have to evaluate these on their own. *)
  | IdSpecial (StdLength, _)
  | IdSpecial (StdMakeArray, _) ->
      assert false
  | Call (IdSpecial (StdLength, _), (_l, args, _r)) ->
      Primitive (Double (float_of_int (List.length args), fk))
  | Call (IdSpecial (StdMakeArray, tk), (_l, args, _r)) -> (
      let mk_lazy_val i fdef =
        let e =
          Call
            (Lambda fdef, (fk, [ Arg (L (Number (string_of_int i, fk))) ], fk))
        in
        { V.v = lazy (eval_expr env e); e }
      in
      match args with
      | [ Arg e; Arg e' ] -> (
          match (eval_expr env e, e') with
          | Primitive (Double (n, tk)), Lambda fdef ->
              if Float.is_integer n then
                Array
                  ( fk,
                    Array.init (Float.to_int n) (fun i -> mk_lazy_val i fdef),
                    fk )
              else error tk (spf "Got non-integer %f in std.makeArray" n)
          | v, e' ->
              error tk
                (spf "Improper arguments to std.makeArray: %s, %s" (sv v)
                   ([%show: expr] e')))
      | _ ->
          error tk
            (spf
               "Improper number of arguments to std.makeArray: expected 2, got \
                %d"
               (List.length args)))
  | Local (_tlocal, binds, _tsemi, e) ->
      let locals =
        binds
        |> List.fold_left
             (fun acc (B (id, _teq, e)) ->
               (* closure! *)
               (* TODO? should we use env.locals or acc ? *)
               let lzv = lazy (eval_expr { locals = env.locals } e) in
               Map_.add (LId (fst id)) lzv acc)
             env.locals
      in
      eval_expr { (* env with *) locals } e
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
                Lazy.force ei.v
            | _else_ ->
                error tkf (spf "Out of bound for array index: %s" (sv e'))
          else error tkf (spf "Not an integer: %s" (sv e'))
      | V.Object (_l, (_assertsTODO, fields), _r), V.Primitive (V.Str (fld, tk))
        -> (
          match
            fields
            |> List.find_opt (fun (field : V.field) ->
                   fst field.fld_name =$= fld)
          with
          | None -> error tk (spf "field %s not present in %s" fld (sv e))
          | Some fld -> Lazy.force fld.fld_value.v)
      (* TODO? support ArrayAccess for Strings? *)
      | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv e')))
  | Call (e0, (l, args, _r)) -> (
      match eval_expr env e0 with
      | V.Function { f_tok = _; f_params = l, params, r; f_body = eb } ->
          (* the named_args are supposed to be the last one *)
          let basic_args, named_args =
            args
            |> Common.partition_either (function
                 | Arg ei -> Left ei
                 | NamedArg (id, _tk, ei) -> Right (fst id, ei))
          in
          (* opti? use a hashtbl? but for < 5 elts, probably worse? *)
          let hnamed_args = Common.hash_of_list named_args in
          let basic_args = Array.of_list basic_args in
          let m = Array.length basic_args in
          let binds =
            params
            |> List.mapi (fun i (P (id, teq, ei')) ->
                   let ei'' =
                     match i with
                     | _ when i < m -> basic_args.(i) (* ei *)
                     | _ when Hashtbl.mem hnamed_args (fst id) ->
                         Hashtbl.find hnamed_args (fst id)
                     | _else_ -> ei'
                   in
                   B (id, teq, ei''))
          in
          eval_expr env (Local (l, binds, r, eb))
      | v -> error l (spf "not a function: %s" (sv v)))
  | UnaryOp ((op, tk), e) -> (
      match op with
      | UBang -> (
          match eval_expr env e with
          | V.Primitive (V.Bool (b, tk)) -> V.Primitive (V.Bool (not b, tk))
          | v -> error tk (spf "Not a boolean for unary !: %s" (sv v)))
      | UPlus -> (
          match eval_expr env e with
          | V.Primitive (V.Double (f, tk)) -> V.Primitive (V.Double (f, tk))
          | v -> error tk (spf "Not a number for unary +: %s" (sv v)))
      | UMinus -> (
          match eval_expr env e with
          | V.Primitive (V.Double (f, tk)) -> V.Primitive (V.Double (-.f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v)))
      | UTilde -> (
          match eval_expr env e with
          | V.Primitive (V.Double (f, tk)) ->
              let f = f |> Int64.of_float |> Int64.lognot |> Int64.to_float in
              V.Primitive (V.Double (f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v))))
  | BinaryOp (el, (op, tk), er) -> eval_binary_op env el (op, tk) er
  | If (tif, e1, e2, e3) -> (
      match eval_expr env e1 with
      | V.Primitive (V.Bool (b, _)) ->
          if b then eval_expr env e2 else eval_expr env e3
      | v -> error tif (spf "not a boolean for if: %s" (sv v)))
  | Error (tk, e) -> (
      match eval_expr env e with
      | V.Primitive (V.Str (s, tk)) -> error tk (spf "ERROR: %s" s)
      | v -> error tk (spf "ERROR: %s" (tostring v)))

and eval_binary_op env el (op, tk) er =
  match op with
  | Plus -> (
      match (eval_expr env el, eval_expr env er) with
      | V.Array (l1, arr1, _r1), V.Array (_l2, arr2, r2) ->
          V.Array (l1, Array.append arr1 arr2, r2)
      | V.Primitive (V.Double (f1, tk)), V.Primitive (V.Double (f2, _)) ->
          V.Primitive (V.Double (f1 +. f2, tk))
      | V.Primitive (V.Str (s1, tk1)), V.Primitive (V.Str (s2, _tk2)) ->
          V.Primitive (V.Str (s1 ^ s2, tk1))
      | V.Primitive (V.Str (s, tk)), v ->
          V.Primitive (V.Str (s ^ tostring v, tk))
      | v, V.Primitive (V.Str (s, tk)) ->
          V.Primitive (V.Str (tostring v ^ s, tk))
      (* TODO: when objects, inheritance, very complex! *)
      | _else_ -> todo env ())
  | And -> (
      match eval_expr env el with
      | V.Primitive (V.Bool (b, _)) as v -> if b then eval_expr env er else v
      | v -> error tk (spf "Not a boolean for &&: %s" (sv v)))
  | Or -> (
      match eval_expr env el with
      | V.Primitive (V.Bool (b, _)) as v -> if b then v else eval_expr env er
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
        | ( ( Plus | Minus | Mult | Div | LSL | LSR | And | Or | BitAnd | BitOr
            | BitXor ),
            _ ) ->
            assert false
      in
      Primitive (Bool (bool, tk))
  | Minus
  | Mult
  | Div -> (
      match (eval_expr env el, eval_expr env er) with
      | V.Primitive (V.Double (f1, itk)), V.Primitive (V.Double (f2, _)) ->
          let op =
            match op with
            | Minus -> ( -. )
            | Mult -> ( *. )
            | Div -> ( /. )
            | _else_ -> assert false
          in
          V.Primitive (V.Double (op f1 f2, itk))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Parse_info.str_of_info tk)
               (sv v2)))
  | LSL
  | LSR
  | BitAnd
  | BitOr
  | BitXor -> (
      let v1 = eval_expr env el in
      let v2 = eval_expr env er in
      match (v1, v2) with
      | V.Primitive (V.Double (f1, tk1)), V.Primitive (V.Double (f2, tk2)) ->
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
          V.Primitive (V.Double (Int64.to_float i64, tk1))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Parse_info.str_of_info tk)
               (sv v2)))

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
        let a0 = Lazy.force ax.(0).v in
        let b0 = Lazy.force bx.(0).v in
        match eval_std_cmp_value_ a0 b0 with
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

and eval_obj_inside env (l, x, r) : V.value_ =
  match x with
  | Object (assertsTODO, fields) ->
      (* sanity check no duplicated fields *)
      let hdupes = Hashtbl.create 16 in
      (* We need a 'let rec' below because while defining the object o,
       * we need to add in the environment a binding from LSelf to o
       * when evaluating the field value.
       * The use of lazy_o below is a bit ugly but is a trick to overcome
       * let-rec limitations (you can't do 'let rec o = <a_value>').
       * See https://stackoverflow.com/questions/19859953/limitations-of-let-rec-in-ocaml
       *)
      let rec lazy_o =
        lazy
          (V.Object
             ( l,
               ( assertsTODO,
                 fields
                 |> Common.map_filter
                      (fun
                        { fld_name = FExpr (tk, ei, _); fld_hidden; fld_value }
                      ->
                        match eval_expr env ei with
                        | V.Primitive (V.Null _) -> None
                        | V.Primitive (V.Str ((str, _) as fld_name)) ->
                            if Hashtbl.mem hdupes str then
                              error tk (spf "duplicate field name: \"%s\"" str)
                            else Hashtbl.add hdupes str true;

                            let fld_value =
                              let v =
                                lazy
                                  (let locals =
                                     env.locals |> Map_.add LSelf lazy_o
                                     |> Map_.add LSuper (lazy V.empty_obj)
                                   in
                                   eval_expr { (* env with *) locals } fld_value)
                              in
                              { V.v; e = fld_value }
                            in
                            Some { V.fld_name; fld_hidden; fld_value }
                        | v ->
                            error tk
                              (spf "field name was not a string: %s" (sv v))) ),
               r ))
      in
      Lazy.force lazy_o
  | ObjectComp x ->
      let v = eval_obj_comprehension env x in
      todo env v

and eval_obj_comprehension env v =
  (fun env (_fldname, _tk, v3, v4) ->
    let v3 = eval_expr env v3 in
    let v4 = eval_for_comp env v4 in
    todo env (v3, v4))
    env v

and eval_for_comp env v =
  (fun env (_tk1, _id, _tk2, v4) ->
    let v4 = eval_expr env v4 in
    todo env v4)
    env v

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let eval_program (e : Core_jsonnet.program) : Value_jsonnet.value_ =
  let empty_env = { locals = Map_.empty } in
  let v = eval_expr empty_env e in
  v
