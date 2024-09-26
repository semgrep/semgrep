(* Yoann Padioleau
 *
 * Copyright (c) 2023 r2c
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
open AST_terraform
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST_terraform to AST_generic *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fb = Tok.unsafe_fake_bracket

let map_argument (arg : argument) : G.definition =
  let id, _teq, e = arg in
  let ent = G.basic_entity id in
  let def = { G.vinit = Some e; vtype = None; vtok = G.no_sc } in
  (ent, G.VarDef def)

(* We convert to a field, to be similar to Parse_terraform_xxx.map_object,
 * so some patterns like 'a=1 ... b=2' can match block body as well as objects.
 *)
let rec map_block_body_element (x : block_body_element) : G.field =
  match x with
  | Argument v1 ->
      let def = map_argument v1 in
      def |> G.fld
  | Block blk ->
      let e = map_block blk in
      G.F (G.exprstmt e)
  | BlockEllipsis t -> G.field_ellipsis t

(* TODO? convert to a definition? a class_def?
 * coupling: Constant_propagation.terraform_stmt_to_vardefs
 *)
and map_block ({ btype = _kind, tk; blabels; bbody = lb, body, rb } : block) :
    G.expr =
  let id = (Tok.content_of_tok tk, tk) in
  let labels_id =
    blabels
    |> List_.map (function
         | LblStr x -> G.L (G.String (fb x)) |> G.e
         | LblId id ->
             let n = H2.name_of_id id in
             G.N n |> G.e)
  in

  let n = H2.name_of_id id in
  (* convert in a Record like map_object *)
  let flds = List_.map map_block_body_element body in
  let body = G.Record (lb, flds, rb) |> G.e in
  let es = labels_id @ [ body ] in
  let args = es |> List_.map G.arg in
  (* coupling: if you modify this code, you should adjust
   * Constant_propagation.terraform_stmt_to_vardefs.
   * bugfix: I used to transform that in a New (..., TyN n, ...) but
   * lots of terraform rules are using some
   *   pattern-inside: resource ... {}
   *   pattern: resource
   * and the second pattern is parsed as an expression which would not
   * match the TyN.
   * TODO? convert in something else?
   * TODO: should we use something else than Call since it's already used
   * for expressions in map_expr_term() above?
   *)
  G.Call (G.N n |> G.e, Tok.unsafe_fake_bracket args) |> G.e

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* almost copy-paste of map_block_body_element above, but returning statements
 * TODO? we should transform the 'locals' and 'variable' blocks
 * in regular VarDefs instead of doing it later in
 * Constant_propagation.terraform_stmt_to_vardefs?
 *)
let program (xs : config) : G.program =
  xs
  |> List_.map (function
       (* TODO? this should never happen in terraform files at the toplevel *)
       | Argument e ->
           let def = map_argument e in
           G.DefStmt def |> G.s
       | BlockEllipsis t -> G.exprstmt (G.Ellipsis t |> G.e)
       | Block blk ->
           let e = map_block blk in
           G.exprstmt e)

let any (x : any) : G.any =
  match x with
  | E e -> G.E e
  | Pr config -> G.Pr (program config)
