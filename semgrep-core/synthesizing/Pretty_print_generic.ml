(* Emma Jin
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
open AST_generic
module F = Format

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pretty print AST generic code (can also be a semgrep pattern).
 *
 * This will be useful for the pattern-from-code synthesizing project but
 * also for correct autofixing.
 *
 * TODO: Pretty printing library to use:
 *  - OCaml Format lib? see pfff/commons/OCaml.string_of_v for an example
 *  - Martin's easy-format?
 *  - Wadler's pretty-printer combinators?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = {
  mvars: Metavars_generic.metavars_binding;
  lang: Lang.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let todo any =
  pr ("TODO");
  pr (show_any any);
  failwith "TODO"

let token tok = Parse_info.str_of_info tok

let ident (s, _) = s

let arithop env (op, tok) =
  match op with
      | Plus -> "+"
      | Minus -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Pow -> "^"
      | Eq -> (match env.lang with
                | Lang.OCaml -> "="
                | _ -> "=="
              )
      | _ -> todo (E (IdSpecial (ArithOp op, tok)))
      (*
      | Pow | FloorDiv | MatMult (* Python *)
      | LSL | LSR | ASR (* L = logic, A = Arithmetic, SL = shift left *)
      | BitOr | BitXor | BitAnd | BitNot (* unary *) | BitClear (* Go *)
      (* todo? rewrite in CondExpr? have special behavior *)
      | And | Or (* also shortcut operator *) | Xor (* PHP*) | Not (* unary *)
      | NotEq     (* less: could be desugared to Not Eq *)
      | PhysEq (* '==' in OCaml, '===' in JS/... *)
      | NotPhysEq (* less: could be desugared to Not PhysEq *)
      | Lt | LtE | Gt | GtE  (* less: could be desugared to Or (Eq Lt) *)
      | Cmp (* <=>, PHP *)
      (* todo: not really an arithmetic operator, maybe rename the type *)
      | Concat (* '.' PHP *) *)

(*****************************************************************************)
(* Pretty printer *)
(*****************************************************************************)
let rec expr env =
let ppf = F.sprintf in
function
  | Id ((s,_), idinfo) -> id env (s, idinfo)
  | IdSpecial (sp, tok) -> special env (sp, tok)
  | Call (e, (_, es, _)) ->
      ppf "%s(%s)" (expr env e) (arguments env es)
  | L x -> literal env x
  | Tuple es -> ppf "(%s)" (tuple env es)
  | ArrayAccess (e1, e2) -> ppf "%s[%s]" (expr env e1) (expr env e2)
  | SliceAccess (e, o1, o2, o3) -> slice_access env e (o1, o2) o3
  | DotAccess (e, tok, fi) -> dot_access env (e, tok, fi)
  | Ellipsis _ -> "..."
  | Conditional (e1, e2, e3) -> cond env (e1, e2, e3)
  | x -> todo (E x)

and id env (s, {id_resolved; _}) =
   match !id_resolved with
       | Some (ImportedEntity ents, _) -> dotted_access env ents
       | _ -> s

and special env = function
  | (ArithOp op, tok) -> arithop env (op, tok)
  | (sp, tok) -> todo (E (IdSpecial (sp, tok)))

and literal env = function
  | Bool ((b,_)) -> F.sprintf "%B" b
  | Int ((s,_)) -> s
  | Float ((s,_)) -> s
  | Char ((s,_)) -> F.sprintf "'%s'" s
  | String ((s,_)) ->
      (match env.lang with
      | Lang.Python | Lang.Python2 | Lang.Python3 ->
            "'" ^ s ^ "'"
      | _ -> raise Todo
      )
  | Regexp ((s,_)) -> s
  | x -> todo (E (L x))

and arguments env xs =
  match xs with
  | [] -> ""
  | [x] -> argument env x
  | x::y::xs ->
      argument env x ^ ", " ^ arguments env (y::xs)

and argument env = function
  | Arg e -> expr env e
  | x -> todo (Ar x)

and tuple env = function
  | [] -> ""
  | [x] -> expr env x
  | x::y::xs -> expr env x ^ ", " ^ tuple env (y::xs)

and dotted_access env = function
  | [] -> ""
  | [x] -> ident x
  | x::y::xs -> ident x ^ "." ^ dotted_access env (y::xs)

and slice_access env e (o1, o2) = function
  | None -> F.sprintf "%s[%s:%s]" (expr env e) (option env o1) (option env o2)
  | Some e1 -> F.sprintf "%s[%s:%s:%s]" (expr env e) (option env o1) (option env o2) (expr env e1)

and option env = function
  | None -> ""
  | Some e -> expr env e

and dot_access env (e, tok, fi) =
  F.sprintf "%s%s%s" (expr env e) (token tok) (field_ident env fi)

and field_ident env fi =
  match fi with
       | FId (s, _) -> s
       | FName ((s, _), _) -> s
       | FDynamic e -> expr env e

and cond env (e1, e2, e3) =
  let s1 = expr env e1 in
  let s2 = expr env e2 in
  let s3 = expr env e3 in
  match env.lang with
     | Lang.Python -> F.sprintf "%s if %s else %s" s2 s1 s3
     | Lang.OCaml -> F.sprintf "if %s then %s else %s" s1 s2 s3
     | Lang.Java -> F.sprintf "%s ? %s : %s" s1 s2 s3
     | _ -> todo (E(Conditional(e1, e2, e3)))



(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let expr_to_string lang mvars e =
  let env = { lang; mvars } in
  expr env e


let pattern_to_string lang any =
  let mvars = [] in
  match any with
  | E e -> expr_to_string lang mvars e
  | _ ->
      failwith "todo: only expression pattern can be pretty printed right now"
