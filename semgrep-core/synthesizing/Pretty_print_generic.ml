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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pretty print AST generic code (can also be a semgrep pattern).
 *
 * This will be useful for the pattern-from-code synthesizing project but
 * also for correct autofixing.
 *
 * Pretty printing library to use?
 *  - OCaml Format lib
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

(*****************************************************************************)
(* Pretty printer *)
(*****************************************************************************)
let rec expr env = function
  | Id ((s,_), _idinfo) -> s
  | Call (e, (_, es, _)) ->
      expr env e ^ "(" ^ arguments env es ^ ")"
  | L x -> literal env x
  | Ellipsis _ -> "..."
  | x -> todo (E x)


and literal env = function
  | String ((s,_)) ->
      (match env.lang with
      | Lang.Python | Lang.Python2 | Lang.Python3 ->
            "'" ^ s ^ "'"
      | _ -> raise Todo
      )
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
