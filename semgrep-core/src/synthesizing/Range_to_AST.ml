(* Yoann Padioleau
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
module G = AST_generic
module V = Visitor_AST
open Range (* for the $..$ range operators *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Locate AST fragments given a code range.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Returns range of tokens in AST. *)
let range_of_ast ast = Range.range_of_tokens (V.ii_of_any ast)

let any_to_str ast = OCaml.string_of_v (Meta_AST.vof_any ast)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

exception Found of G.any

exception FoundExpr of G.expr

let expr_at_range r1 ast =
  (* This could probably be implemented more efficiently ... but should be
   * good enough in practice.
   * todo? ideally every expression nodes in the AST would have range field
   * associated with it, or at least a special id so we could memoize
   * range for subexpressions.
   *)
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kexpr =
          (fun (k, _) e ->
            let r2_opt = range_of_ast (G.E e) in
            match r2_opt with
            (* NoTokenLocation issue for the expression, should fix! *)
            | None -> ()
            | Some r2 ->
                if not (r1 $<>$ r2) then
                  if r2 $<=$ r1 then raise (FoundExpr e) (* recurse *) else k e);
      }
  in

  try
    visitor (G.Pr ast);
    None
  with FoundExpr e -> Some e

let any_at_range_first r1 ast =
  (* This could probably be implemented more efficiently ... but should be
   * good enough in practice.
   * todo? ideally every expression nodes in the AST would have range field
   * associated with it, or at least a special id so we could memoize
   * range for subexpressions.
   *)
  let visitor =
    V.mk_visitor
      {
        V.default_visitor with
        V.kstmt =
          (fun (k, _) s ->
            let r2_opt = range_of_ast (G.S s) in
            match r2_opt with
            (* NoTokenLocation issue for the expression, should fix! *)
            | None -> ()
            | Some r2 ->
                if not (r1 $<>$ r2) then
                  if r2 $<=$ r1 then raise (Found (G.S s)) (* recurse *)
                  else k s);
      }
  in

  try
    visitor (G.Pr ast);
    match expr_at_range r1 ast with None -> None | Some e -> Some (G.E e)
  with Found a -> Some a

let any_to_stmt (any : G.any) : G.stmt =
  match any with
  | G.S s -> s
  | _ ->
      failwith
        (spf "Attempted to convert non-statement to statement %s"
           (any_to_str any))

let join_anys (anys : AST_generic.any list) : AST_generic.any option =
  match anys with
  | [] -> None
  | [ xs ] -> Some xs
  | G.S _ :: _ -> Some (G.Ss (List.map any_to_stmt anys))
  | _ ->
      failwith
        "Unable to handle ranges that contain multiple expressions. Range must \
         contain a single expression or one-or-more statements."

let split_any (any : G.any) : G.any list =
  match any with G.Ss stmts -> List.map (fun s -> G.S s) stmts | x -> [ x ]

let any_at_range_all r1 ast : AST_generic.any option =
  let rec any_at_range_list r1 ast : AST_generic.any list =
    (* Recurse if there are more tokens in the target. *)
    let rec_if_more found =
      match range_of_ast found with
      (* Failure should never happen? *)
      | None -> failwith "Could not find token range for target."
      | Some tokenrange ->
          let next_start = tokenrange.end_ + 1 in
          let last = r1.end_ in
          if next_start > last then [ found ]
          else
            found :: any_at_range_list { start = next_start; end_ = last } ast
    in
    match any_at_range_first r1 ast with
    | None -> []
    | Some found -> rec_if_more found
  in
  any_at_range_list r1 ast |> join_anys
