(* Emma Jin, Yoann Padioleau
 *
 * Copyright (C) 2020, 2023 Semgrep Inc.
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
open AST_generic
module G = AST_generic
module F = Format
module Log = Log_printing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pretty print AST generic code, including patterns (in AST_generic.any).
 *
 * This module was started to help the pattern-from-code synthesizing project
 * (which is now in src/experiments/). It is also used for debugging
 * purpose in the code to handle 'semgrep-core -dfg_value'.
 * TODO? It could also be used for correct autofixing (even though we probably
 * should use instead Ugly_print_AST.ml now).
 * TODO? It could also be used in Eval_generic.ml to pretty print code
 * stored in metavariable that needs to be dumped (for example for
 * 'metavariable-pattern:').
 *
 * Note that even though this file is called Pretty_print_AST.ml, we don't
 * really do advanced pretty printing like in Wadler's "A prettier printer"
 * (https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf).
 * The only "pretty" thing we do is to indent statements in nested blocks.
 *
 * Moreover, we rely on the fact that the generic AST is actually almost a
 * Concrete Syntax Tree (CST) with most of the tokens stored in the tree.
 * This is why for example we don't need to handle the many different ways
 * languages represented True and False ("true" in most languages, but
 * sometimes "True", or even "TRUE"); we can expect the token associated
 * with the boolean in the AST/CST to contain the actually string used
 * for the boolean (via Parse_info.str_of_info).
 *
 * TODO: Pretty printing library to use:
 *  - OCaml Format lib? see commons/OCaml.string_of_v for an example
 *  - Martin's easy-format?
 *  - Wadler's pretty-printer combinators?
 *
 * See also http://rigaux.org/language-study/syntax-across-languages.html
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO? we could also make this module more flexible by allowing to
 * also pass some metavariable bindings? 'mvars : Metavariable.bindings;'?
 *)
type env = { lang : Lang.t; (* indentation level *) level : int }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let todo any =
  Log.warn (fun m -> m "Pretty_print_AST TODO: %s" (show_any any));
  "*TODO*"

let rec indent = function
  | 0 -> ""
  | n -> "    " ^ indent (n - 1)

let opt f = function
  | None -> ""
  | Some x -> f x

(* 'd' stands for default below (shorter to type at the call site).
 * Note that Parse_info.str_of_info() does not raise an exn anymore
 * on fake tokens (safe on unsafe). It instead returns the fake token string.
 * TODO? maybe get rid of the default then, since in practice NoTokenLocation
 * should never happen anymore.
 *)
let token ?(d = "TODO") tok =
  try Tok.content_of_tok tok with
  (* this exn can trigger now only for Parse_info.Ab (abstracted token),
   * which should never happen. We don't use Parse_info.Ab anymore.
   *)
  | Tok.NoTokenLocation _ -> d

type lang_kind = CLikeSemiColon | Other

let _lang_kind = function
  | Lang.C
  | Lang.Cpp
  | Lang.Java
  | Lang.Apex
  | Lang.Csharp
  | Lang.Rust
  | Lang.Move_on_sui
  | Lang.Move_on_aptos ->
      CLikeSemiColon
  | _other_ -> Other

(*****************************************************************************)
(* Ident and operators *)
(*****************************************************************************)

let ident (s, _) = s

let arithop _env (op, tok) =
  match op with
  | Plus -> token ~d:"+" tok
  | Minus -> token ~d:"-" tok
  | Mult -> token ~d:"*" tok
  | Div -> token ~d:"/" tok
  | Mod -> token ~d:"%" tok
  | Pow -> token ~d:"**" tok
  | Eq -> token ~d:"=" tok
  | Lt -> token ~d:"<" tok
  | LtE -> token ~d:"<=" tok
  | Gt -> token ~d:">" tok
  | GtE -> token ~d:">=" tok
  | NotEq -> token ~d:"!=" tok
  | _else_ -> token tok

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

let rec stmt env st =
  match st.s with
  | ExprStmt (e, tok) ->
      (* note that it is frequent for tok above to be a fake token.
       * Indeed, many languages do not use semicolons (e.g., Python),
       * or they are automatically inserted (e.g., via ASI in Javascript).
       * Note that in those cases, we generate a fake token in
       * AST_generic.sc with an empty string in it and 'token()' below
       * should use that, so in practice this ~d:";" should never be used.
       *)
      F.sprintf "%s%s" (expr env e) (token ~d:";" tok)
  | Block x -> block env x
  | If (tok, e, s, sopt) -> if_stmt env (token ~d:"if" tok, e, s, sopt)
  | While (tok, e, s) -> while_stmt env (tok, e, s)
  | DoWhile (_tok, s, e) -> do_while stmt env (s, e)
  | For (tok, hdr, s) -> for_stmt env (tok, hdr, s)
  | Return (tok, eopt, sc) ->
      (* TODO:
         | Lang.R -> F.sprintf "%s(%s)" (token ~d:"return" tok) to_return
      *)
      let to_return = expr_opt env eopt in
      F.sprintf "%s %s%s" (token ~d:"return" tok) to_return (token ~d:"" sc)
  | Break (tok, lbl, sc) ->
      let lbl_str = label_ident env lbl in
      F.sprintf "%s%s%s" (token ~d:"break" tok) lbl_str (token ~d:"" sc)
  | Continue (tok, lbl, sc) ->
      let lbl_str = label_ident env lbl in
      F.sprintf "%s%s%s" (token ~d:"continue" tok) lbl_str (token ~d:"" sc)
  | DefStmt def -> def_stmt env def
  | Switch (_, _, _)
  | Label (_, _)
  | Goto (_, _, _)
  | Throw (_, _, _)
  | Try (_, _, _, _, _)
  | WithUsingResource (_, _, _)
  | Assert (_, _, _)
  | DirectiveStmt _
  | DisjStmt (_, _)
  | OtherStmtWithStmt (_, _, _)
  | OtherStmt (_, _)
  | RawStmt _ ->
      todo (S st)

and label_ident env lbl =
  match lbl with
  | LNone -> ""
  | LId l -> F.sprintf " %s" (ident l)
  | LInt (n, _) -> F.sprintf " %d" n
  | LDynamic e -> F.sprintf " %s" (expr env e)

and block env (t1, ss, t2) =
  let rec show_statements env = function
    | [] -> ""
    | [ x ] -> F.sprintf "%s%s" (indent env.level) (stmt env x)
    | x :: xs ->
        F.sprintf "%s%s\n%s" (indent env.level) (stmt env x)
          (show_statements env xs)
  in
  let get_boundary t =
    let t_str = token ~d:"" t in
    match t_str with
    | "" -> ""
    | "{" -> "\n" ^ indent (env.level - 1) ^ "{\n"
    | "}" -> "\n" ^ indent (env.level - 1) ^ "}\n"
    | _ -> t_str
  in
  if env.level > 0 then
    F.sprintf "%s%s%s" (get_boundary t1) (show_statements env ss)
      (get_boundary t2)
  else show_statements env ss

and if_stmt env (tok, e, s, sopt) =
  let no_paren_cond = F.sprintf "%s %s" in
  (* if cond *)
  let paren_cond = F.sprintf "%s (%s)" in
  (* if cond *)
  let colon_body = F.sprintf "%s:\n%s\n" in
  (* (if cond) body *)
  let bracket_body = F.sprintf "%s %s" (* (if cond) body *) in
  let format_cond, elseif_str, format_block =
    match env.lang with
    | Lang.Circom
    | Lang.Cairo
    | Lang.Xml
    | Lang.Dart
    | Lang.Clojure
    | Lang.Lisp
    | Lang.Scheme
    | Lang.Julia
    | Lang.Elixir
    | Lang.Bash
    | Lang.Dockerfile
    | Lang.Ruby
    | Lang.Ocaml
    | Lang.Scala
    | Lang.Solidity
    | Lang.Php
    | Lang.Promql
    | Lang.Protobuf
    | Lang.Ql
    | Lang.Hack
    | Lang.Yaml
    | Lang.Html
    | Lang.Terraform ->
        raise Todo
    | Lang.Python
    | Lang.Python2
    | Lang.Python3 ->
        (no_paren_cond, "elif", colon_body)
    | Lang.Apex
    | Lang.Java
    | Lang.Go
    | Lang.C
    | Lang.Cpp
    | Lang.Csharp
    | Lang.Json
    | Lang.Jsonnet
    | Lang.Js
    | Lang.Move_on_sui
    | Lang.Move_on_aptos
    | Lang.Ts
    | Lang.Vue
    | Lang.Kotlin
    | Lang.Rust
    | Lang.R
    (* Swift does not require parentheses around the condition, but it does
     * permit them. *)
    | Lang.Swift ->
        (paren_cond, "else if", bracket_body)
    | Lang.Lua -> (paren_cond, "elseif", bracket_body)
  in
  let e_str = format_cond tok (condition env e) in
  let s_str = stmt { env with level = env.level + 1 } s in
  let if_stmt_prt = format_block e_str s_str in
  match sopt with
  | None -> if_stmt_prt
  | Some { s = If (_, e', s', sopt'); _ } ->
      F.sprintf "%s%s" if_stmt_prt
        (if_stmt env (indent env.level ^ elseif_str, e', s', sopt'))
  | Some { s = Block (_, [ { s = If (_, e', s', sopt'); _ } ], _); _ } ->
      F.sprintf "%s%s" if_stmt_prt
        (if_stmt env (indent env.level ^ elseif_str, e', s', sopt'))
  | Some body ->
      F.sprintf "%s%s" if_stmt_prt
        (format_block
           (indent env.level ^ "else")
           (stmt { env with level = env.level + 1 } body))

and condition env x =
  match x with
  | Cond e -> expr env e
  | OtherCond _ -> raise Todo

and while_stmt env (tok, e, s) =
  let ocaml_while = F.sprintf "%s %s do\n%s\ndone" in
  let python_while = F.sprintf "%s %s:\n%s" in
  let go_while = F.sprintf "%s %s %s" in
  let c_while = F.sprintf "%s (%s) %s" in
  let ruby_while = F.sprintf "%s %s\n %s\nend" in
  let while_format =
    match env.lang with
    | Lang.Circom
    | Lang.Cairo
    | Lang.Xml
    | Lang.Dart
    | Lang.Clojure
    | Lang.Lisp
    | Lang.Scheme
    | Lang.Julia
    | Lang.Elixir
    | Lang.Bash
    | Lang.Php
    | Lang.Promql
    | Lang.Protobuf
    | Lang.Dockerfile
    | Lang.Hack
    | Lang.Lua
    | Lang.Yaml
    | Lang.Scala
    | Lang.Solidity
    | Lang.Swift
    | Lang.Html
    | Lang.Terraform
    | Lang.Ql ->
        raise Todo
    | Lang.Python
    | Lang.Python2
    | Lang.Python3 ->
        python_while
    | Lang.Apex
    | Lang.Java
    | Lang.C
    | Lang.Cpp
    | Lang.Csharp
    | Lang.Kotlin
    | Lang.Json
    | Lang.Jsonnet
    | Lang.Js
    | Lang.Move_on_sui
    | Lang.Move_on_aptos
    | Lang.Ts
    | Lang.Vue
    | Lang.Rust
    | Lang.R ->
        c_while
    | Lang.Go -> go_while
    | Lang.Ruby -> ruby_while
    | Lang.Ocaml -> ocaml_while
  in
  while_format (token ~d:"while" tok) (condition env e)
    (stmt { env with level = env.level + 1 } s)

and do_while stmt env (s, e) =
  let c_do_while = F.sprintf "do %s\nwhile(%s)" in
  let do_while_format =
    match env.lang with
    | Lang.Circom
    | Lang.Cairo
    | Lang.Xml
    | Lang.Dart
    | Lang.Clojure
    | Lang.Lisp
    | Lang.Scheme
    | Lang.Julia
    | Lang.Elixir
    | Lang.Bash
    | Lang.Php
    | Lang.Dockerfile
    | Lang.Hack
    | Lang.Lua
    | Lang.Promql
    | Lang.Protobuf
    | Lang.Yaml
    | Lang.Scala
    | Lang.Solidity
    | Lang.Swift
    | Lang.Html
    | Lang.Terraform ->
        raise Todo
    | Lang.Apex
    | Lang.Java
    | Lang.C
    | Lang.Cpp
    | Lang.Csharp
    | Lang.Kotlin
    | Lang.Js
    | Lang.Ts
    | Lang.Vue ->
        c_do_while
    | Lang.Python
    | Lang.Python2
    | Lang.Python3
    | Lang.Ql
    | Lang.Go
    | Lang.Json
    | Lang.Jsonnet
    | Lang.Move_on_sui
    | Lang.Move_on_aptos
    | Lang.Ocaml
    | Lang.Rust
    | Lang.R ->
        failwith "impossible; no do while"
    | Lang.Ruby -> failwith "ruby is so weird (here, do while loop)"
  in
  do_while_format (stmt { env with level = env.level + 1 } s) (expr env e)

and for_stmt env (for_tok, hdr, s) =
  let for_format =
    match env.lang with
    | Lang.Circom
    | Lang.Cairo
    | Lang.Xml
    | Lang.Dart
    | Lang.Clojure
    | Lang.Lisp
    | Lang.Scheme
    | Lang.Julia
    | Lang.Elixir
    | Lang.Bash
    | Lang.Php
    | Lang.Promql
    | Lang.Protobuf
    | Lang.Html
    | Lang.Dockerfile
    | Lang.Hack
    | Lang.Lua
    | Lang.Yaml
    | Lang.Scala
    | Lang.Solidity
    | Lang.Terraform ->
        raise Todo
    | Lang.Apex
    | Lang.Java
    | Lang.C
    | Lang.Cpp
    | Lang.Csharp
    | Lang.Kotlin
    | Lang.Js
    | Lang.Move_on_sui ->
        failwith "Move on SUI has for loops????"
    | Lang.Move_on_aptos
    | Lang.Ts
    | Lang.Vue
    | Lang.Rust
    | Lang.R
    | Lang.Swift ->
        F.sprintf "%s (%s) %s"
    | Lang.Go -> F.sprintf "%s %s %s"
    | Lang.Python
    | Lang.Python2
    | Lang.Python3 ->
        F.sprintf "%s %s:\n%s"
    | Lang.Ruby -> F.sprintf "%s %s\ndo %s\nend"
    | Lang.Json
    | Lang.Jsonnet
    | Lang.Ocaml
    | Lang.Ql ->
        failwith "JSON/OCaml/QL has for loops????"
  in
  let show_init = function
    | ForInitVar (ent, var_def) ->
        F.sprintf "%s%s%s"
          (opt (fun x -> type_ x ^ " ") var_def.vtype)
          (ident_or_dynamic ent.name)
          (opt (fun x -> " = " ^ expr env x) var_def.vinit)
    | ForInitExpr e_init -> expr env e_init
  in
  let rec show_init_list = function
    | [] -> ""
    | [ x ] -> show_init x
    | x :: xs -> show_init x ^ ", " ^ show_init_list xs
  in
  let opt_expr = opt (fun x -> expr env x) in
  let hdr_str =
    let for_each (pat, tok, e) =
      F.sprintf "%s %s %s" (pattern env pat) (token ~d:"in" tok) (expr env e)
    in
    let multi_for_each = function
      | FE fe -> for_each fe
      | FECond (fe, _, e) -> F.sprintf "%s if %s" (for_each fe) (expr env e)
      | FEllipsis _ -> "..."
    in
    match hdr with
    | ForClassic (init, cond, next) ->
        F.sprintf "%s; %s; %s" (show_init_list init) (opt_expr cond)
          (opt_expr next)
    | ForEach (pat, tok, e) -> for_each (pat, tok, e)
    | MultiForEach fors -> String.concat ";" (List_.map multi_for_each fors)
    | ForEllipsis tok -> token ~d:"..." tok
  in
  let body_str = stmt { env with level = env.level + 1 } s in
  for_format (token ~d:"for" for_tok) hdr_str body_str

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

and type_ t =
  match t.t with
  | TyN (Id (id, _)) -> ident id
  | _ -> todo (T t)

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

and def_stmt env (entity, def_kind) =
  let var_def (ent, def) =
    let no_val, with_val =
      match env.lang with
      | Lang.Circom
      | Lang.Cairo
      | Lang.Xml
      | Lang.Dart
      | Lang.Clojure
      | Lang.Lisp
      | Lang.Scheme
      | Lang.Julia
      | Lang.Elixir
      | Lang.Bash
      | Lang.Php
      | Lang.Promql
      | Lang.Protobuf
      | Lang.Dockerfile
      | Lang.Hack
      | Lang.Lua
      | Lang.Yaml
      | Lang.Scala
      | Lang.Solidity
      | Lang.Swift
      | Lang.Html
      | Lang.Terraform ->
          raise Todo
      | Lang.Apex
      | Lang.Java
      | Lang.C
      | Lang.Cpp
      | Lang.Csharp
      | Lang.Kotlin ->
          ( (fun typ id _e -> F.sprintf "%s %s;" typ id),
            fun typ id e -> F.sprintf "%s %s = %s;" typ id e )
      | Lang.Js
      | Lang.Ts
      | Lang.Vue ->
          ( (fun _typ id _e -> F.sprintf "var %s;" id),
            fun _typ id e -> F.sprintf "var %s = %s;" id e )
      | Lang.Go ->
          ( (fun typ id _e -> F.sprintf "var %s %s" id typ),
            fun typ id e -> F.sprintf "var %s %s = %s" id typ e )
          (* will have extra space if no type *)
      | Lang.Python
      | Lang.Python2
      | Lang.Python3
      | Lang.Ruby
      | Lang.Ql ->
          ( (fun _typ id _e -> F.sprintf "%s" id),
            fun _typ id e -> F.sprintf "%s = %s" id e )
      | Lang.Move_on_sui
      | Lang.Move_on_aptos
      | Lang.Rust ->
          ( (fun typ id _e -> F.sprintf "let %s: %s" id typ),
            fun typ id e -> F.sprintf "let %s: %s = %s" id typ e )
          (* will have extra space if no type *)
      | Lang.R ->
          ( (fun _typ id _e -> F.sprintf "%s" id),
            fun _typ id e -> F.sprintf "%s <- %s" id e )
      | Lang.Json
      | Lang.Jsonnet
      | Lang.Ocaml ->
          failwith "I think JSON/OCaml have no variable definitions"
    in
    let typ, id =
      match ent.name with
      | EN (Id (_, { id_type = { contents = Some t }; _ })) ->
          (type_ t, ident_or_dynamic ent.name)
      | _ -> ("", ident_or_dynamic ent.name)
    in
    match def.vinit with
    | None -> no_val typ id ""
    | Some e -> with_val typ id (expr env e)
  in
  match def_kind with
  | VarDef def -> var_def (entity, def)
  | _ -> todo (S (DefStmt (entity, def_kind) |> G.s))

(* TODO? maybe we should check IdFlags.is_hidden !(id_info.id_flags) *)
and ident_or_dynamic = function
  | EN (Id (x, _idinfo)) -> ident x
  | EN _
  | EDynamic _
  | EPattern _
  | OtherEntity _ ->
      raise Todo

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

and expr env e =
  match e.e with
  | N (Id ((s, _), idinfo)) -> id env (s, idinfo)
  | N (IdQualified qualified_info) -> id_qualified env qualified_info
  | IdSpecial (sp, tok) -> special env (sp, tok)
  | Call (e1, e2) -> call env (e1, e2)
  | New (_, t, _, es) -> new_call env (t, es)
  | L x -> literal env x
  | Container (Tuple, (_, es, _)) -> F.sprintf "(%s)" (tuple env es)
  | ArrayAccess (e1, (_, e2, _)) ->
      F.sprintf "%s[%s]" (expr env e1) (expr env e2)
  | Assign (e1, tok, e2) ->
      F.sprintf "%s %s %s" (expr env e1) (token ~d:"=" tok) (expr env e2)
  | AssignOp (e1, op, e2) ->
      F.sprintf "%s %s= %s" (expr env e1) (arithop env op) (expr env e2)
  | SliceAccess (e, (_, (o1, o2, o3), _)) -> slice_access env e (o1, o2) o3
  | DotAccess (e, tok, fi) -> dot_access env (e, tok, fi)
  | Ellipsis _ -> "..."
  | Conditional (e1, e2, e3) -> cond env (e1, e2, e3)
  | OtherExpr (categ, anys) -> other env (categ, anys)
  | TypedMetavar (id, _, typ) -> tyvar env (id, typ)
  | _x -> todo (E e)

and expr_opt env eopt = opt (expr env) eopt

and id env (s, { id_resolved; _ }) : string =
  match !id_resolved with
  | Some (ImportedEntity ents, _) -> canonical_name env ents
  | Some (ImportedModule ents, _) -> canonical_name env ents
  | _ -> s

(* TODO: factorize with dotted_access *)
and canonical_name env = function
  | [] -> ""
  | [ x ] -> x
  | x :: y :: xs -> x ^ "." ^ canonical_name env (y :: xs)

(* TODO: look at name_top too *)
and id_qualified env { name_last = id, _toptTODO; name_middle; name_top; _ } =
  (match name_top with
  | None -> ""
  | Some _t -> "::")
  ^
  match name_middle with
  | Some (QDots dot_ids) ->
      (* TODO: do not do fst, look also at type qualification *)
      F.sprintf "%s.%s" (dotted_access env (List_.map fst dot_ids)) (ident id)
  | Some (QExpr (e, _t)) -> expr env e ^ "::"
  | None -> ident id

and special env = function
  | This, tok -> token ~d:"this" tok
  | Self, tok -> token ~d:"self" tok
  | Op op, tok -> arithop env (op, tok)
  | IncrDecr _, _ -> "" (* should be captured in the call *)
  | sp, tok -> todo (E (IdSpecial (sp, tok) |> G.e))

and new_call env (t, (_, es, _)) =
  let s1 = type_ t in
  F.sprintf "new %s(%s)" s1 (arguments env es)

and call env (e, (_, es, _)) =
  let s1 = expr env e in
  match (e.e, es) with
  | IdSpecial (Op In, _), [ e1; e2 ] ->
      F.sprintf "%s in %s" (argument env e1) (argument env e2)
  | IdSpecial (Op NotIn, _), [ e1; e2 ] ->
      F.sprintf "%s not in %s" (argument env e1) (argument env e2)
  | IdSpecial (Op _, _), [ x; y ] ->
      F.sprintf "%s %s %s" (argument env x) s1 (argument env y)
  | IdSpecial (IncrDecr (i_d, pre_post), _), [ x ] -> (
      let op_str =
        match i_d with
        | Incr -> "++"
        | Decr -> "--"
      in
      match pre_post with
      | Prefix -> F.sprintf "%s%s" op_str (argument env x)
      | Postfix -> F.sprintf "%s%s" (argument env x) op_str)
  | _ -> F.sprintf "%s(%s)" s1 (arguments env es)

and literal _env l =
  match l with
  | Bool (true, t) -> token ~d:"true" t
  | Bool (false, t) -> token ~d:"false" t
  | Int (_, tok) -> token tok
  | Float (_, t) -> token t
  | Char (s, _) -> F.sprintf "'%s'" s
  (* TODO: once we will have string wrap bracket in AST_generic,
   * we will be able to print correctly whether '' or "" was used
   * to contain the string
   *)
  | String (_l, (s, _), _r) -> "\"" ^ s ^ "\""
  | Regexp ((_, (s, _), _), rmod) -> (
      "/" ^ s ^ "/"
      ^
      match rmod with
      | None -> ""
      | Some (s, _) -> s)
  | (Atom _ | Unit _ | Null _ | Undefined _ | Imag _ | Ratio _) as x ->
      todo (E (L x |> G.e))

and arguments env xs =
  match xs with
  | [] -> ""
  | [ x ] -> argument env x
  | x :: y :: xs -> argument env x ^ ", " ^ arguments env (y :: xs)

and argument env = function
  | Arg e -> expr env e
  | ArgType t -> type_ t
  | ArgKwd (id, e) -> F.sprintf "%s=%s" (ident id) (expr env e)
  | ArgKwdOptional (id, e) -> F.sprintf "%s=?%s" (ident id) (expr env e)
  | x -> todo (Ar x)

and tuple env = function
  | [] -> ""
  | [ x ] -> expr env x
  | x :: y :: xs -> expr env x ^ ", " ^ tuple env (y :: xs)

and dotted_access env = function
  | [] -> ""
  | [ x ] -> ident x
  | x :: y :: xs -> ident x ^ "." ^ dotted_access env (y :: xs)

and slice_access env e (o1, o2) = function
  | None ->
      F.sprintf "%s[%s:%s]" (expr env e) (expr_opt env o1) (expr_opt env o2)
  | Some e1 ->
      F.sprintf "%s[%s:%s:%s]" (expr env e) (expr_opt env o1) (expr_opt env o2)
        (expr env e1)

and dot_access env (e, _tok, fi) =
  F.sprintf "%s.%s" (expr env e) (field_ident env fi)

and field_ident env fi =
  match fi with
  | FN (Id (id, _idinfo)) -> ident id
  | FN (IdQualified qualified_info) -> id_qualified env qualified_info
  | FDynamic e -> expr env e

and tyvar env (id, typ) =
  match env.lang with
  | Lang.Java -> F.sprintf "(%s %s)" (type_ typ) (ident id)
  | Lang.Go -> F.sprintf "(%s : %s)" (ident id) (type_ typ)
  | _ -> failwith "Not implemented for this language"

and cond env (e1, e2, e3) =
  let s1 = expr env e1 in
  let s2 = expr env e2 in
  let s3 = expr env e3 in
  match env.lang with
  | Lang.Python -> F.sprintf "%s if %s else %s" s2 s1 s3
  | Lang.Ocaml -> F.sprintf "if %s then %s else %s" s1 s2 s3
  | Lang.Java -> F.sprintf "%s ? %s : %s" s1 s2 s3
  | _ -> todo (E (Conditional (e1, e2, e3) |> G.e))

and other _env (op, anys) =
  match (op, anys) with
  | _ -> todo (E (OtherExpr (op, anys) |> G.e))

(*****************************************************************************)
(* patterns *)
(*****************************************************************************)
and pattern env = function
  | PatLiteral l -> literal env l
  | PatId (id, _id_info) -> ident id
  | x -> todo (P x)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* this is mostly used for 'semgrep-core -dfg_value' *)
let ctype = function
  | G.Cbool -> "bool"
  | G.Cint -> "int"
  | G.Cstr -> "str"
  | G.Cany -> "???"

let svalue env = function
  | G.NotCst -> "TOP"
  | G.Sym e -> Printf.sprintf "sym(%s)" (expr env e)
  | G.Cst t -> Printf.sprintf "cst(%s)" (ctype t)
  | G.Lit l -> Printf.sprintf "lit(%s)" (literal env l)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let expr_to_string lang e =
  let env = { lang; level = 0 } in
  expr env e

let stmt_to_string lang s =
  let env = { lang; level = 0 } in
  stmt env s

let arguments_to_string lang x =
  let env = { lang; level = 0 } in
  arguments env x

let svalue_to_string lang c =
  let env = { lang; level = 0 } in
  svalue env c
