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


module V = Visitor_AST
module Lib = Lib_AST
module G = AST_generic
open Range (* for the $..$ range operators *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Locate AST fragments given a code range.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

exception Found of G.expr

let expr_at_range r1 ast =

  (* This could probably be implemented more efficiently ... but should be
   * good enough in practice.
   * todo? ideally every expression nodes in the AST would have range field
   * associated with it, or at least a special id so we could memoize
   * range for subexpressions.
   *)
  let visitor = V.mk_visitor { V.default_visitor with
    V.kexpr = (fun (k, _) e ->
       let toks = Lib.ii_of_any (G.E e) in
       let r2_opt = Range.range_of_tokens toks in
       (match r2_opt with
       (* NoTokenLocation issue for the expression, should fix! *)
       | None -> ()
       | Some r2 ->
         if r1 $<>$ r2
         then ()
         else
           if r2 $<=$ r1
           then raise (Found e)
           (* recurse *)
           else k e
       )
    );
   } in

  try
    visitor (G.Pr ast);
    None
  with Found e -> Some e
