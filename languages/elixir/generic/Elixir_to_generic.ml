(* Yoann Padioleau
 *
 * Copyright (c) 2022-2023 Semgrep Inc.
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
open AST_elixir
module G = AST_generic
module H = AST_generic_helpers

(* TODO: to remove! *)
[@@@warning "-27"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_elixir to generic AST conversion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let todo _env _v = failwith "TODO"
let map_string _env x = x
let map_int _env x = x
let map_char _env x = x
let map_list f env xs = Common.map (f env) xs
let map_option f env x = Option.map (f env) x
let map_or_quoted f env v = todo env v

(* TODO: maybe need a 'a. for all type
   match v with
   | X v ->
       let v = f env v in
       xxx env v
   | Quoted v ->
       let v = map_quoted env v in
       xxx env v
*)

(*****************************************************************************)
(* Boilerplate *)
(*****************************************************************************)

let map_wrap f env (v1, v2) = (f env v1, v2)
let map_bracket f env (v1, v2, v3) = (v1, f env v2, v3)

let map_ident env v : G.ident =
  match v with
  | Id v ->
      let id = (map_wrap map_string) env v in
      id
  | IdEllipsis v -> ("...", v)
  | IdMetavar v ->
      let id = (map_wrap map_string) env v in
      id

let map_ident_expr env v : G.expr =
  match v with
  | IdEllipsis v -> G.Ellipsis v |> G.e
  | Id _
  | IdMetavar _ ->
      let id = map_ident env v in
      G.N (H.name_of_id id) |> G.e

let map_alias env v = (map_wrap map_string) env v

let map_wrap_operator env (op, _tk) =
  match op with
  | OPin -> todo env ()
  | ODot -> todo env ()
  | OMatch -> todo env ()
  | OCapture -> todo env ()
  | OType -> todo env ()
  | OStrictAnd -> todo env ()
  | OStrictOr -> todo env ()
  | OStrictNot -> todo env ()
  | OPipeline -> todo env ()
  | OModuleAttr -> todo env ()
  | OLeftArrow -> todo env ()
  | ODefault -> todo env ()
  | ORightArrow -> todo env ()
  | OCons -> todo env ()
  | OWhen -> todo env ()
  | O v -> todo env v
  | OOther v ->
      let v = map_string env v in
      todo env v

(* start of big mutually recursive functions *)

let rec map_atom env (v1, v2) =
  let v2 = (map_or_quoted (map_wrap map_string)) env v2 in
  todo env (v1, v2)

and map_keyword env (v1, v2) =
  let v1 = (map_or_quoted (map_wrap map_string)) env v1 in
  todo env (v1, v2)

and map_quoted env v = todo env v
(* TODO
   (map_bracket (map_list f)) env v
*)

and map_arguments env (v1, v2) : G.argument list =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  Common.map G.arg v1 @ Common.map (fun (_kwd, _e) -> todo env ()) v2

and map_items env (v1, v2) =
  let v1 = (map_list map_expr) env v1 in
  let v2 = map_keywords env v2 in
  todo env (v1, v2)

and map_keywords env v = (map_list map_pair) env v

and map_pair env (v1, v2) =
  let v1 = map_keyword env v1 in
  let v2 = map_expr env v2 in
  (v1, v2)

and map_expr_or_kwds env v =
  match v with
  | E v ->
      let v = map_expr env v in
      todo env v
  | Kwds v ->
      let v = map_keywords env v in
      todo env v

and map_expr env v : G.expr =
  match v with
  | I v -> map_ident_expr env v
  | L lit -> G.L lit |> G.e
  | A v ->
      let v = map_atom env v in
      todo env v
  | String v ->
      let v = map_quoted env v in
      todo env v
  | Charlist v ->
      let v = map_quoted env v in
      todo env v
  | Sigil (v1, v2, v3) ->
      let v2 = map_sigil_kind env v2 in
      let v3 = (map_option (map_wrap map_string)) env v3 in
      todo env (v1, v2, v3)
  | List v ->
      let v = (map_bracket map_items) env v in
      G.Container (G.List, v) |> G.e
  | Tuple v ->
      let v = (map_bracket map_items) env v in
      G.Container (G.Tuple, v) |> G.e
  | Bits v ->
      let v = (map_bracket map_items) env v in
      todo env v
  | Map (v1, v2, v3) ->
      let v2 = (map_option map_astruct) env v2 in
      let v3 = (map_bracket map_items) env v3 in
      todo env (v1, v2, v3)
  | Alias v ->
      let v = map_alias env v in
      todo env v
  | Block v ->
      let v = map_block env v in
      todo env v
  | DotAlias (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v3 = map_alias env v3 in
      todo env (v1, v2, v3)
  | DotTuple (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v3 = (map_bracket map_items) env v3 in
      todo env (v1, v2, v3)
  | DotAnon (v1, v2) ->
      let v1 = map_expr env v1 in
      todo env (v1, v2)
  | DotRemote v ->
      let v = map_remote_dot env v in
      todo env v
  | ModuleVarAccess (v1, v2) ->
      let v2 = map_expr env v2 in
      todo env (v1, v2)
  | ArrayAccess (v1, v2) ->
      let v1 = map_expr env v1 in
      let v2 = (map_bracket map_expr) env v2 in
      G.ArrayAccess (v1, v2) |> G.e
  | Call v -> map_call env v
  | UnaryOp (v1, v2) ->
      let v1 = map_wrap_operator env v1 in
      let v2 = map_expr env v2 in
      todo env (v1, v2)
  | BinaryOp (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v2 = map_wrap_operator env v2 in
      let v3 = map_expr env v3 in
      todo env (v1, v2, v3)
  | OpArity (v1, v2, v3) ->
      let v1 = map_wrap_operator env v1 in
      let v3 = (map_wrap (map_option map_int)) env v3 in
      todo env (v1, v2, v3)
  | When (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      todo env (v1, v2, v3)
  | Join (v1, v2, v3) ->
      let v1 = map_expr env v1 in
      let v3 = map_expr_or_kwds env v3 in
      todo env (v1, v2, v3)
  | Lambda (v1, v2, v3) ->
      let v2 = map_clauses env v2 in
      todo env (v1, v2, v3)
  | Capture (v1, v2) ->
      let v2 = map_expr env v2 in
      todo env (v1, v2)
  | ShortLambda (v1, v2) ->
      let v2 = (map_bracket map_expr) env v2 in
      todo env (v1, v2)
  | PlaceHolder (v1, v2) ->
      let v2 = (map_wrap (map_option map_int)) env v2 in
      todo env (v1, v2)
  | DeepEllipsis v ->
      let v = (map_bracket map_expr) env v in
      G.DeepEllipsis v |> G.e

and map_astruct env v = map_expr env v

and map_sigil_kind env v =
  match v with
  | Lower (v1, v2) ->
      let v1 = (map_wrap map_char) env v1 in
      let v2 = map_quoted env v2 in
      todo env (v1, v2)
  | Upper (v1, v2) ->
      let v1 = (map_wrap map_char) env v1 in
      let v2 = (map_bracket (map_wrap map_string)) env v2 in
      todo env (v1, v2)

and map_body env v : G.stmt list =
  let xs = (map_list map_expr) env v in
  xs |> Common.map G.exprstmt

and map_call env (v1, v2, v3) : G.expr =
  let v1 = map_expr env v1 in
  let v2 = (map_bracket map_arguments) env v2 in
  let v3 = (map_option map_do_block) env v3 in
  todo env (v1, v2, v3)

and map_remote_dot env (v1, v2, v3) =
  let v1 = map_expr env v1 in
  (* TODO let v3 = (map_or_quoted map_ident_or_operator) env v3 in *)
  todo env (v1, v2, v3)

and map_stab_clause env (v1, v2, v3) =
  let map_tuple1 env (v1, v2) =
    let v1 = map_arguments env v1 in
    let map_tuple2 env (v1, v2) =
      let v2 = map_expr env v2 in
      todo env (v1, v2)
    in
    let v2 = (map_option map_tuple2) env v2 in
    todo env (v1, v2)
  in
  let v1 = map_tuple1 env v1 in
  let v3 = map_body env v3 in
  todo env (v1, v2, v3)

and map_clauses env v = (map_list map_stab_clause) env v

and map_body_or_clauses env v =
  match v with
  | Body v ->
      let v = map_body env v in
      todo env v
  | Clauses v ->
      let v = map_clauses env v in
      todo env v

and map_do_block env v =
  let map_tuple1 env (v1, v2) =
    let v1 = map_body_or_clauses env v1 in
    let map_tuple2 env (v1, v2) =
      let v1 = (map_wrap map_exn_clause_kind) env v1 in
      let v2 = map_body_or_clauses env v2 in
      todo env (v1, v2)
    in
    let v2 = (map_list map_tuple2) env v2 in
    todo env (v1, v2)
  in
  (map_bracket map_tuple1) env v

and map_exn_clause_kind env v =
  match v with
  | After -> todo env ()
  | Rescue -> todo env ()
  | Catch -> todo env ()
  | Else -> todo env ()

and map_block env v = (map_bracket map_body_or_clauses) env v

let map_program env v : G.program = map_body env v

let map_any env v =
  match v with
  | Pr v ->
      let v = map_program env v in
      todo env v

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program x =
  let env = () in
  map_program env x

let any x =
  let env = () in
  map_any env x
