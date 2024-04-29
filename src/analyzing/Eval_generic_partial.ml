(* Iago Abal
 *
 * Copyright (C) 2020-2022 Semgrep Inc.
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
open AST_generic
module G = AST_generic
module H = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Partial evaluator for the generic AST.
 *
 * See also Dataflow_svalue.ml, for the IL-based version....
 * LATER: we should remove the code below and rely only on Dataflow_svalue.ml.
 * For that we may need to add `e_svalue` to AST_generic.expr and fill it in
 * during constant-propagation.
 *
 * See also Eval_generic.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type var = string * G.sid

type env = {
  lang : Lang.t option;
  (* basic constant propagation of literals for semgrep *)
  constants : (var, G.svalue) Hashtbl.t;
  (* TODO: this is actually used only in Constant_propagation.ml, but
   * put here so we can reuse the same env in Constant_propagation.ml
   *)
  attributes : (var, G.attribute list) Hashtbl.t;
}

let default_env lang =
  { lang; constants = Hashtbl.create 100; attributes = Hashtbl.create 100 }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let ( let* ) o f = Option.bind o f
let fb = Tok.unsafe_fake_bracket

let is_lang env l2 =
  match env.lang with
  | None -> false
  | Some l1 -> l1 = l2

let is_js env =
  match env.lang with
  | None -> false
  | Some lang -> Lang.is_js lang

let find_id env id id_info =
  match id_info with
  | { id_resolved = { contents = Some (_kind, sid) }; _ } ->
      let s = H.str_of_ident id in
      Hashtbl.find_opt env.constants (s, sid)
  | __else__ -> None

let find_name env name =
  match name with
  | Id (id, id_info) -> find_id env id id_info
  | IdQualified _ -> None

let fold_args1 f args =
  match args with
  | [] -> None
  | a1 :: args -> List.fold_left f a1 args

let find_type_args args =
  args
  |> List.find_map (function
       | Some (Lit (Bool _)) -> Some Cbool
       | Some (Lit (Int _)) -> Some Cint
       | Some (Lit (String _)) -> Some Cstr
       | Some (Cst ctype) -> Some ctype
       | _arg -> None)

let sign i = Int64.shift_right i (Sys.int_size - 1)

let int_add n m =
  let r = Int64.add n m in
  if Int64.equal (sign n) (sign m) && sign r <> sign n then None (* overflow *)
  else Some r

let int_mult i1 i2 =
  let overflow =
    Int64_.(
      i1 <> 0L && i2 <> 0L
      && ((i1 < 0L && i2 = min_int) (* >max_int *)
         || (i1 = min_int && i2 < 0L) (* >max_int *)
         ||
         if sign i1 * sign i2 = 1L then abs i1 > abs (max_int / i2)
           (* >max_int *)
         else abs i1 > abs (min_int / i2) (* <min_int *)))
  in
  if overflow then None else Some Int64_.(i1 * i2)

let binop_int_cst op i1 i2 =
  match (i1, i2) with
  | Some (Lit (Int (Some n, _))), Some (Lit (Int (Some m, _))) ->
      let* r = op n m in
      Some (Lit (Int (Parsed_int.of_int64 r)))
  | Some (Lit (Int _)), Some (Cst Cint)
  | Some (Cst Cint), Some (Lit (Int _)) ->
      Some (Cst Cint)
  | _i1, _i2 -> None

let binop_bool_cst op b1 b2 =
  match (b1, b2) with
  | Some (Lit (Bool (b1, t1))), Some (Lit (Bool (b2, _))) ->
      Some (Lit (Bool (op b1 b2, t1)))
  | Some (Lit (Bool _)), Some (Cst Cbool)
  | Some (Cst Cbool), Some (Lit (Bool _)) ->
      Some (Cst Cbool)
  | _b1, _b2 -> None

let concat_string_cst env s1 s2 =
  match (s1, s2) with
  | Some (Lit (String (l, (s1, t1), r))), Some (Lit (String (_, (s2, _), _))) ->
      Some (Lit (String (l, (s1 ^ s2, t1), r)))
  | Some (Lit (String (l, (s1, t1), r))), Some (Lit (Int (Some i, _)))
    when is_lang env Lang.Java || is_js env ->
      (* implicit int-to-string conversion *)
      Some (Lit (String (l, (s1 ^ Int64.to_string i, t1), r)))
  | Some (Lit (String (l, (s1, t1), r))), Some (Lit (Float (Some m, _)))
    when is_js env ->
      (* implicit float-to-string conversion *)
      let m_str =
        (* JS: we parse all numbers as floats, and 1.0 is printed as "1" *)
        if Float.is_integer m then string_of_int (int_of_float m)
        else string_of_float m
      in
      Some (Lit (String (l, (s1 ^ m_str, t1), r)))
  | Some (Lit (String _)), Some (Cst Cstr)
  | Some (Cst Cstr), Some (Lit (String _))
  | Some (Cst Cstr), Some (Cst Cstr) ->
      Some (Cst Cstr)
  | _b1, _b2 -> None

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let rec eval (env : env) (x : G.expr) : G.svalue option =
  match x.e with
  | L x -> Some (Lit x)
  | N (Id (_, { id_svalue = { contents = Some x }; _ }))
  | DotAccess
      ( { e = IdSpecial ((This | Self), _); _ },
        _,
        FN (Id (_, { id_svalue = { contents = Some x }; _ })) ) ->
      Some x
  (* ugly: terraform specific. *)
  | DotAccess
      ( { e = N (Id ((("local" | "var"), _), _)); _ },
        _,
        FN (Id (_, { id_svalue = { contents = Some x }; _ })) )
    when is_lang env Lang.Terraform ->
      Some x
  (* id_svalue is populated when used with the pro engine. *)
  | DotAccess (_, _, FN (Id (_, { id_svalue = { contents = Some x }; _ }))) ->
      Some x
  | N (IdQualified { name_info = { id_svalue = { contents = Some x }; _ }; _ })
    ->
      Some x
  (* ugly: dockerfile specific *)
  | Call
      ( { e = N (Id (("!dockerfile_expand!", _), _)); _ },
        ( _,
          [
            Arg { e = N (Id (_, { id_svalue = { contents = Some x }; _ })); _ };
          ],
          _ ) )
    when is_lang env Lang.Dockerfile ->
      Some x
  | Conditional (_e1, e2, e3) ->
      let* v2 = eval env e2 in
      let* v3 = eval env e3 in
      Some (Eval_il_partial.union v2 v3)
  | Call
      ( { e = IdSpecial (EncodedString str_kind, _); _ },
        (_, [ Arg { e = L (String (_, (str, str_tok), _) as str_lit); _ } ], _)
      ) -> (
      match str_kind with
      | "r" ->
          let str = String.escaped str in
          (* TODO? reuse l/r from the Call instead of using fb below? or
           * from the String above?
           *)
          Some (Lit (String (fb (str, str_tok))))
      | _else ->
          (* THINK: is this good enough for "b" and "u"? *)
          Some (Lit str_lit))
  | Call ({ e = IdSpecial (InterpolatedElement, _); _ }, (_, [ Arg e ], _)) ->
      eval env e
  | Call ({ e = IdSpecial special; _ }, args) -> eval_special env special args
  | Call ({ e = N name; _ }, args) -> eval_call env name args
  | Call (({ e = DotAccess (_, _, FN (Id _)); _ } as e), args) ->
      let* name = H.name_of_dot_access e in
      eval_call env name args
  | N name -> find_name env name
  | _ -> None

and eval_args env args =
  args |> Tok.unbracket
  |> List_.map (function
       | Arg e -> eval env e
       | _ -> None)

and eval_special env (special, _) args =
  match (special, eval_args env args) with
  (* booleans *)
  | Op Not, [ Some (Lit (Bool (b, t))) ] -> Some (Lit (Bool (not b, t)))
  | Op Or, args -> fold_args1 (binop_bool_cst ( || )) args
  | Op And, args -> fold_args1 (binop_bool_cst ( && )) args
  (* integers *)
  | Op Plus, args when find_type_args args = Some Cint ->
      fold_args1 (binop_int_cst int_add) args
  | Op Mult, args when find_type_args args = Some Cint ->
      fold_args1 (binop_int_cst int_mult) args
  (* strings *)
  | (Op (Plus | Concat) | ConcatString _), args
    when find_type_args args = Some Cstr ->
      fold_args1 (concat_string_cst env) args
  | Op Mult, [ Some (Lit (String _) | Cst Cstr); _N ]
    when env.lang = Some Lang.Python ->
      (* Python: "..." * N, NOTE that we don't check the type of N, partly because
       * we lack good type inference for Python, but should be fine. *)
      Some (Cst Cstr)
  | __else__ -> None

and eval_call env name args =
  (* Built-in knowledge, we know these functions return constants when
   * given constant arguments. *)
  let args = eval_args env args in
  match (env.lang, name, args) with
  | ( Some Lang.Php,
      Id ((("escapeshellarg" | "htmlspecialchars_decode"), _), _),
      [ Some (Lit (String _) | Cst Cstr) ] ) ->
      Some (Cst Cstr)
  | ( Some Lang.Java,
      IdQualified
        {
          name_last = ("format", _), _;
          name_middle =
            Some
              (QDots
                ( [ (("String", _), _) ]
                | [ (("java", _), _); (("lang", _), _); (("String", _), _) ] ));
          _;
        },
      _args ) ->
      if
        args
        |> List.for_all (function
             | Some (Lit _ | Cst _) -> true
             | _ -> false)
      then Some (Cst Cstr)
      else None
  | _lang, _name, _args -> None
