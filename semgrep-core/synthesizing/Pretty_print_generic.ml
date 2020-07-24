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
  pr (show_any any); "*TODO*"

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
      | Pow -> "**"
      | Eq -> (match env.lang with
                | Lang.OCaml -> "="
                | _ -> "=="
              )
      | Lt -> "<"
      | LtE -> "<="
      | Gt -> ">"
      | GtE -> ">="
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

(* statements *)

let rec stmt env level =
function
  | ExprStmt (e, tok) -> F.sprintf "%s%s" (expr env e) (token "" tok)
  | Block (x) -> block env x level
  | If (tok, e, s, sopt) -> if_stmt env level (token "if" tok, e, s, sopt)
  | While (tok, e, s) -> while_stmt env level (tok, e, s)
  | Return (tok, eopt) -> return env (tok, eopt)
  | x -> todo (S x)

and block env (t1, ss, t2) level =
  let rec indent =
  function
    | 0 -> ""
    | n -> "    " ^ (indent (n - 1))
  in
  let rec show_statements env =
    function
      | [] -> ""
      | [x] -> F.sprintf "%s%s" (indent level) (stmt env level x)
      | x::xs -> F.sprintf "%s%s\n%s" (indent level) (stmt env level x) (show_statements env xs)
   in
   let get_boundary t =
     let t_str = token "" t in
       match t_str with "" -> "" | "{" -> "{\n" | "}" -> "\n}" | _ -> t_str
   in
     F.sprintf "%s%s%s" (get_boundary t1) (show_statements env ss) (get_boundary t2)

and if_stmt env level (tok, e, s, sopt) =
  let no_paren_cond = F.sprintf "%s %s" in (* if cond *)
  let paren_cond = F.sprintf "%s (%s)" in (* if cond *)
  let colon_body = F.sprintf "%s:\n%s\n" in (* (if cond) body *)
  let bracket_body = F.sprintf "%s %s\n" (* (if cond) body *)
  in
  let (format_cond, elseif_str, format_block) =
    (match env.lang with
    | Lang.Python | Lang.Python2 | Lang.Python3 -> (no_paren_cond, "elif", colon_body)
    | Lang.Java | Lang.Go | Lang.C | Lang.JSON | Lang.Javascript | Lang.Typescript -> (paren_cond, "else if", bracket_body)
    | Lang.Ruby -> failwith "I don't want to deal with Ruby right now"
    | Lang.OCaml -> failwith "Impossible; if statements should be expressions"
    )
  in
  let e_str = format_cond tok (expr env e) in
  let s_str = (stmt env (level + 1) s) in
  let if_stmt_prt = format_block e_str s_str in
        match sopt with
        | None -> if_stmt_prt
        | Some (Block(_, [If (_, e', s', sopt')], _)) -> F.sprintf "%s%s" if_stmt_prt (if_stmt env level (elseif_str, e', s', sopt'))
        | Some (body) -> F.sprintf "%s%s" if_stmt_prt (format_block "else" (stmt env (level + 1) body))

and while_stmt env level (tok, e, s) =
   let ocaml_while = F.sprintf "%s %s do\n%s\ndone\n" in
   let python_while = F.sprintf "%s %s:\n%s\n" in
   let go_while = F.sprintf "%s %s %s\n" in
   let c_while = F.sprintf "%s (%s) %s\n" in
   let ruby_while = F.sprintf "%s %s\n %s\nend\n" in
   let while_format =
      (match env.lang with
      | Lang.Python | Lang.Python2 | Lang.Python3 -> python_while
      | Lang.Java | Lang.C | Lang.JSON | Lang.Javascript | Lang.Typescript -> c_while
      | Lang.Go -> go_while
      | Lang.Ruby -> ruby_while
      | Lang.OCaml -> ocaml_while
      )
   in
      while_format (token "while" tok) (expr env e) (stmt env (level + 1) s)


and return env (tok, eopt) =
  let to_return =
  match eopt with
  | None -> ""
  | Some e -> expr env e
  in
  match env.lang with
  | Lang.Java | Lang.C -> F.sprintf "%s %s;" (token "return" tok) to_return
  | Lang.Python | Lang.Python2 | Lang.Python3
  | Lang.Go | Lang.Ruby | Lang.OCaml
  | Lang.JSON | Lang.Javascript | Lang.Typescript -> F.sprintf "%s %s" (token "return" tok) to_return

(* expressions *)

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
  | OtherExpr (op, anys) -> other env (op, anys)
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

and other env (op, anys) =
  match (op, anys) with
      | OE_In, [E e1; E e2] -> F.sprintf "%s in %s" (expr env e1) (expr env e2)
      | OE_NotIn, [E e1; E e2] -> F.sprintf "%s not in %s" (expr env e1) (expr env e2)
      | _ -> todo (E (OtherExpr(op, anys)))

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
  stmt env 0 s

let pattern_to_string lang any =
  let mvars = [] in
  match any with
  | E e -> expr_to_string lang mvars e
  | S s -> stmt_to_string lang mvars s
  | _ ->
      pr2 (AST_generic.show_any any);
      failwith "todo: only expression pattern can be pretty printed right now"
