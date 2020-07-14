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

let ident (s, _) = s

let token default tok =
  try Parse_info.str_of_info tok
  with Parse_info.NoTokenLocation _ -> default

let print_type = function
  | TyBuiltin (s, _) -> s
  | TyName (id, _) -> ident id
  | x -> todo (T x)

let print_bool env = function
  | true ->
     (match env.lang with
         | Lang.Python | Lang.Python2 | Lang.Python3 -> "True"
         | Lang.Java | Lang.Go | Lang.C | Lang.JSON | Lang.Javascript
         | Lang.OCaml | Lang.Ruby | Lang.Typescript -> "true")
  | false ->
     (match env.lang with
         | Lang.Python | Lang.Python2 | Lang.Python3  -> "False"
         | Lang.Java | Lang.Go | Lang.C | Lang.JSON | Lang.Javascript
         | Lang.OCaml | Lang.Ruby | Lang.Typescript -> "false")

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
      | _ -> todo (E (IdSpecial (Op op, tok)))
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
let rec stmt env =
function
  | ExprStmt (e, tok) -> F.sprintf "%s%s" (expr env e) (token "" tok)
  | x -> todo (S x)

and expr env =
function
  | Id ((s,_), idinfo) -> id env (s, idinfo)
  | IdQualified(name, idinfo) -> id_qualified env (name, idinfo)
  | IdSpecial (sp, tok) -> special env (sp, tok)
  | Call (e1, e2) -> call env (e1, e2)
  | L x -> literal env x
  | Tuple es -> F.sprintf "(%s)" (tuple env es)
  | ArrayAccess (e1, e2) -> F.sprintf "%s[%s]" (expr env e1) (expr env e2)
  | Assign (e1, _tok, e2) -> F.sprintf "%s = %s" (expr env e1) (expr env e2)
  | SliceAccess (e, o1, o2, o3) -> slice_access env e (o1, o2) o3
  | DotAccess (e, tok, fi) -> dot_access env (e, tok, fi)
  | Ellipsis _ -> "..."
  | Conditional (e1, e2, e3) -> cond env (e1, e2, e3)
  (* | OtherExpr (op, anys) -> *)
  | TypedMetavar (id, _, typ) -> tyvar env (id, typ)
  | x -> todo (E x)

and id env (s, {id_resolved; _}) =
   match !id_resolved with
       | Some (ImportedEntity ents, _) -> dotted_access env ents
       | _ -> s

and id_qualified env ((id, {name_qualifier; _}), _idinfo) =
  match name_qualifier with
       | Some (QDots dot_ids) ->
            F.sprintf "%s.%s" (dotted_access env dot_ids) (ident id)
       | Some (QTop _t) ->
            F.sprintf "::"
       | Some (QExpr (e, _t)) -> expr env e ^ "::"
       | None -> ident id


and special env = function
  | (Op op, tok) -> arithop env (op, tok)
  | (New, _) -> "new"
  | (sp, tok) -> todo (E (IdSpecial (sp, tok)))

and call env (e, (_, es, _)) =
  let s1 = expr env e in
  match (e, es) with
       | (IdSpecial(Op _, _), x::y::[]) -> F.sprintf "%s %s %s" (argument env x) s1 (argument env y)
       | (IdSpecial(New, _), x::ys) -> F.sprintf "%s %s(%s)" s1 (argument env x) (arguments env ys)
       | _ -> F.sprintf "%s(%s)" s1 (arguments env es)

and literal env = function
  | Bool ((b,_)) -> print_bool env b
  | Int ((s,_)) -> s
  | Float ((s,_)) -> s
  | Char ((s,_)) -> F.sprintf "'%s'" s
  | String ((s,_)) ->
      (match env.lang with
      | Lang.Python | Lang.Python2 | Lang.Python3 ->
            "'" ^ s ^ "'"
      | Lang.Java | Lang.Go | Lang.C | Lang.JSON | Lang.Javascript
      | Lang.OCaml | Lang.Ruby | Lang.Typescript ->
            "\"" ^ s ^ "\""
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
  | ArgType t -> print_type t
  | ArgKwd (id, e) -> F.sprintf "%s=%s" (ident id) (expr env e)
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

and dot_access env (e, _tok, fi) =
  F.sprintf "%s.%s" (expr env e) (field_ident env fi)

and field_ident env fi =
  match fi with
       | FId id -> ident id
       | FName (id, _) -> ident id
       | FDynamic e -> expr env e

and tyvar env (id, typ) =
  match env.lang with
    | Lang.Java -> F.sprintf "(%s %s)" (print_type typ) (ident id)
    | Lang.Go -> F.sprintf "(%s : %s)" (ident id) (print_type typ)
    | _ -> failwith "Not implemented for this language"

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

let stmt_to_string lang mvars s =
  let env = { lang; mvars } in
  stmt env s

let pattern_to_string lang any =
  let mvars = [] in
  match any with
  | E e -> expr_to_string lang mvars e
  | S s -> stmt_to_string lang mvars s
  | _ ->
      pr2 (AST_generic.show_any any);
      failwith "todo: only expression pattern can be pretty printed right now"
