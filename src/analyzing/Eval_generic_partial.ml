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
  | IdSpecial _ -> None

let apply_bop_args f args =
  match args with
  | [ a1; a2 ] -> f a1 a2
  | _ -> None

let apply_uop_args f args =
  match args with
  | [ a1 ] -> f a1
  | _ -> None

let lit_to_type = function
  | Some (Lit (Bool _)) -> Some Cbool
  | Some (Lit (Int _)) -> Some Cint
  | Some (Lit (String _)) -> Some Cstr
  | Some (Cst ctype) -> Some ctype
  | _arg -> None

let map_type_args = List.map lit_to_type

(* Sign of an Int64 value as a plain OCaml int (-1, 0, or 1).
 * Avoids Int64.abs wrapping and shift-amount pitfalls in sign detection. *)
let sign i : int =
  if Int64_.(i > 0L) then 1 else if Int64_.(i < 0L) then -1 else 0

let int_add n m =
  let r = Int64.add n m in
  (* Overflow/underflow: same-sign operands produce an opposite-sign result. *)
  if
    (n > Int64.zero && m > Int64.zero && r < Int64.zero)
    || (n < Int64.zero && m < Int64.zero && r > Int64.zero)
  then None
  else Some r

let int_sub n m =
  let r = Int64.sub n m in
  (* Underflow when m>0: n-m < min_int iff n < min_int+m (safe: min_int+m > min_int) *)
  (* Overflow  when m<0: n-m > max_int iff n > max_int+m (safe: max_int+m < max_int) *)
  if
    (m > Int64.zero && n < Int64.add Int64.min_int m)
    || (m < Int64.zero && n > Int64.add Int64.max_int m)
  then None
  else Some r

let int_mult i1 i2 =
  let overflow =
    Int64_.(
      i1 <> 0L && i2 <> 0L
      && ((i1 < 0L && i2 =|= min_int) (* >max_int *)
         || (i1 =|= min_int && i2 < 0L) (* >max_int *)
         (* min_int * k for k >= 2: abs(min_int) wraps in Int64, defeating
          * the abs-based guard below, so catch this case explicitly. *)
         || (i1 =|= min_int && i2 >= 2L)
         || (i2 =|= min_int && i1 >= 2L)
         ||
         (* same sign → result positive, compare against max_int *)
         (* different sign → result negative, compare against min_int *)
         let same_sign = sign i1 = sign i2 in
         if same_sign then abs i1 > abs (max_int / i2)
         else abs i1 > abs (min_int / i2)))
  in
  if overflow then None else Some Int64_.(i1 * i2)

(* OCaml int div is trunc div, but many languages have different semantics *)
(* if clean div, return Some i1/i2. O/w, return None *)
let int_div i1 i2 =
  if i2 = Int64.zero then None
  else if i1 = Int64.min_int && i2 = Int64.minus_one then None (* overflow *)
  else if Int64_.(i1 mod i2) <> Int64.zero then
    None (* not an integer division *)
  else Some Int64_.(i1 / i2)

let lang_divs_to_int (lang : Lang.t option) =
  match lang with
  | Some
      ( Cpp | C | Csharp | Go | Java | Kotlin | Rust | Swift | Solidity | Apex
      | Cairo | Powershell | Ruby | Python2 | Php | Hack ) ->
      true
  | _ -> false

let int_mod i1 i2 =
  if i2 = Int64.zero then None
  else if i1 = Int64.min_int && i2 = Int64.minus_one then None (* overflow *)
  else
    let r = Int64_.(i1 mod i2) in
    (* OCaml and Python have different semantics for negative remainders *)
    if r <> Int64.zero && sign i1 <> sign i2 then None else Some r

let int_floor_div i1 i2 =
  if i2 = Int64.zero then None
  else if i1 = Int64.min_int && i2 = Int64.minus_one then None
  else if
    (* Floor division rounds down towards negative infinity *)
    sign i1 <> sign i2 && Int64_.(i1 mod i2) <> Int64.zero
  then Some Int64_.((i1 / i2) - 1L)
  else Some Int64_.(i1 / i2)

let int_pow i1 i2 =
  let open Int64_ in
  if i2 < 0L then None
  else if i1 = 0L && i2 = 0L then None
  else if i1 = 0L then Some 0L
  else if i1 = 1L then Some 1L
  else if i1 = -1L then if Int64.rem i2 2L = 0L then Some 1L else Some (-1L)
  else
    let rec safe_power x n =
      if equal n 0L then Some 1L
      else if equal n 1L then Some x
      else if equal Int64_.(n mod 2L) 0L then
        let* y = safe_power x Int64_.(n / 2L) in
        int_mult y y
      else
        let* rest = safe_power x Int64_.((n - 1L) / 2L) in
        Option.bind (int_mult rest rest) (int_mult x)
    in
    safe_power i1 i2

let int_bitand i1 i2 = Some Int64_.(i1 land i2)
let int_bitor i1 i2 = Some Int64_.(i1 lor i2)
let int_bitxor i1 i2 = Some Int64_.(i1 lxor i2)
let int_bitnot i = Some Int64_.(i lxor -1L)
let valid_shift i2 = i2 >= Int64.zero && i2 < 64L

let int_lsl i1 i2 =
  if not (valid_shift i2) then None
  else
    let n = Int64.to_int i2 in
    if n > 0 && Int64_.(i1 lsr Int.sub 64 n) <> Int64.zero then None
    else
      let result = Int64_.(i1 lsl n) in
      (* Sign-change check: if the shift moves a 0-bit into the sign position
       * (e.g. max_int << 1 = -2) the result is not representable as a signed
       * integer of the same width, so decline to fold. *)
      if i1 >= Int64.zero <> (result >= Int64.zero) then None else Some result

let int_lsr i1 i2 =
  if not (valid_shift i2) then None else Some Int64_.(i1 lsr Int64.to_int i2)

let int_asr i1 i2 =
  if not (valid_shift i2) then None else Some Int64_.(i1 asr Int64.to_int i2)

let unop_int_cst op i =
  match i with
  | Some (Lit (Int (Some n, _))) ->
      let* r = op n in
      Some (Lit (Int (Parsed_int.of_int64 r)))
  | Some (Lit (Int _)) -> Some (Cst Cint)
  | Some (Cst Cint) -> Some (Cst Cint)
  | _i -> None

let unop_bool_cst op b =
  match b with
  | Some (Lit (Bool (b, t))) -> Some (Lit (Bool (op b, t)))
  | Some (Cst Cbool) -> Some (Cst Cbool)
  | _b -> None

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

let binop_cmp_cst cmp i1 i2 =
  match (i1, i2) with
  | Some (Lit (Int (Some n, _))), Some (Lit (Int (Some m, _))) ->
      Some (Lit (Bool (cmp n m, Tok.unsafe_fake_tok "")))
  | Some (Lit (Int _)), Some (Cst Cint)
  | Some (Cst Cint), Some (Lit (Int _)) ->
      Some (Cst Cbool)
  | _i1, _i2 -> None

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
      ( { e = N (IdSpecial (((This | Self), _), _)); _ },
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
      ( { e = Special (EncodedString str_kind, _); _ },
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
  | Call ({ e = Special (InterpolatedElement, _); _ }, (_, [ Arg e ], _)) ->
      eval env e
  | Call ({ e = Special special; _ }, args) -> eval_special env special args
  | Call ({ e = N name; _ }, args) -> eval_call env name args
  | Call (({ e = DotAccess (_, _, FN (Id _)); _ } as e), args) ->
      let* name = H.name_of_dot_access e in
      eval_call env name args
  | N name -> find_name env name
  | _ -> None

and eval_args env (args : arguments) : G.svalue option list =
  args |> Tok.unbracket
  |> List.map (function
    | Arg e -> eval env e
    | _ -> None)

and eval_special env (special, _) (args : arguments) =
  let args = eval_args env args in
  match (special, args |> map_type_args) with
  | Op Plus, [ Some Cint ] ->
      (* unary plus is identity *)
      apply_uop_args (unop_int_cst Option.some) args
  | Op Plus, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_add) args
  | Op Minus, [ Some Cint ] ->
      (* unary minus *)
      apply_uop_args
        (unop_int_cst (fun n ->
             if n = Int64.min_int then None else Some (Int64.neg n)))
        args
  | Op Minus, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_sub) args
  | Op Mult, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_mult) args
  | Op Div, [ Some Cint; Some Cint ] when lang_divs_to_int env.lang ->
      apply_bop_args (binop_int_cst int_div) args
  | Op Mod, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_mod) args
  | Op Pow, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_pow) args
  | Op FloorDiv, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_floor_div) args
  | Op LSL, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_lsl) args
  | Op LSR, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_lsr) args
  | Op ASR, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_asr) args
  | Op BitAnd, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_bitand) args
  | Op BitOr, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_bitor) args
  | Op BitXor, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_int_cst int_bitxor) args
  | Op BitNot, [ Some Cint ] -> apply_uop_args (unop_int_cst int_bitnot) args
  (* bools *)
  | Op Not, [ Some Cbool ] -> apply_uop_args (unop_bool_cst not) args
  | Op Or, [ Some Cbool; Some Cbool ] ->
      apply_bop_args (binop_bool_cst ( || )) args
  | Op And, [ Some Cbool; Some Cbool ] ->
      apply_bop_args (binop_bool_cst ( && )) args
  | Op Xor, [ Some Cbool; Some Cbool ] ->
      apply_bop_args (binop_bool_cst ( <> )) args
  | Op Eq, [ Some Cbool; Some Cbool ] ->
      apply_bop_args (binop_bool_cst ( = )) args
  | Op Eq, [ Some Cint; Some Cint ] -> apply_bop_args (binop_cmp_cst ( = )) args
  | Op NotEq, [ Some Cbool; Some Cbool ] ->
      apply_bop_args (binop_bool_cst ( <> )) args
  | Op NotEq, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_cmp_cst ( <> )) args
  | Op Lt, [ Some Cint; Some Cint ] -> apply_bop_args (binop_cmp_cst ( < )) args
  | Op Gt, [ Some Cint; Some Cint ] -> apply_bop_args (binop_cmp_cst ( > )) args
  | Op LtE, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_cmp_cst ( <= )) args
  | Op GtE, [ Some Cint; Some Cint ] ->
      apply_bop_args (binop_cmp_cst ( >= )) args
  (* strings *)
  | ConcatString (FString "f"), _ when is_lang env Lang.Python ->
      eval_python_fstring env args
  | ConcatString _, _ ->
      (* ConcatString can have N parts (template literals, implicit concat); fold all *)
      begin match args with
      | hd :: tl -> List.fold_left (concat_string_cst env) hd tl
      | [] -> None
      end
  | Op (Plus | Concat), Some Cstr :: _ ->
      (* Binary string concat; first arg is a string so attempt concat.
       * concat_string_cst handles implicit int/float-to-string for JS/Java. *)
      apply_bop_args (concat_string_cst env) args
  | Op Mult, [ Some Cstr; _N ] when env.lang = Some Lang.Python ->
      (* Python: "..." * N, NOTE that we don't check the type of N, partly because
       * we lack good type inference for Python, but should be fine. *)
      Some (Cst Cstr)
  | Op Nullish, _ when is_js env ->
      begin match args with
      | [ Some (Lit (Null _)); Some v ] -> Some v
      | hd :: _ -> hd
      | _ -> None
      end
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

(** [eval_python_fstring env args] expects [args] to be evaluated already. If
all arguments are literals or constants, it returns a constant string,
otherwise it returns [None]. *)
and eval_python_fstring env args =
  let helper s =
    let tok = Tok.unsafe_fake_tok "" in
    let wrap_bracket = (tok, (s, tok), tok) in
    Some (Lit (String wrap_bracket))
  in
  match args with
  | [] -> helper ""
  | _ -> (
      args
      |> List.map (function
        | Some (Lit (String _) | Cst Cstr) as x -> x
        | Some (Lit (Int (Some n, _))) -> helper (Int64.to_string n)
        | Some (Lit (Float (Some f, _))) -> helper (string_of_float f)
        | _ -> None)
      |> function
      | [] -> None
      | hd :: tl -> List.fold_left (concat_string_cst env) hd tl)
