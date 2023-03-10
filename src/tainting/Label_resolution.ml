(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
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
module G = AST_generic
module T = Taint
module Lval_env = Taint_lval_env
module Taints = T.Taint_set

let logger = Logging.get_logger [ __MODULE__ ]

module LabelSet = Set.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module concerns the solving of "label equations", which are the
   REQUIRES attached to a sink.

   Because there may be many different ways a taint label reaches a sink,
   it is not always correct to report one finding per one sink, per control
   flow path. For instance, take the following:

   pattern-sources:
     - pattern: |
         "a"
       label: A
     - pattern: |
         "b"
       label: B
   pattern-sinks:
     - pattern: |
         sink(...)
       requires: A and B

   on a program like:

   x = "a"
   y = "b"
   w = "a"
   z = "b"

   sink(x, y, w, z)

   There are technically many ways that A and B could reach this sink. Four,
   in fact! It's quickly exponential in the number of sinks and the number
   of arguments. A better approach would be to produce one finding, citing that
   A and B taints must be present, and then listing the A and B taints that are
   known at the time of the sink.

   This is tricky in general, though. Consider also a requires which looks like:
   (A or B) and (B or C) and (C or D)

   The number of ways in which this could be satisfied are numerous. For instance,
   we could solve it with inputs of {A, B, C}, as well as {A, C}. Given taint
   inputs of {A, B, C}, should we report twice, once for the case in which we
   satisfy the formula in one way, and once in another?

   It's kind of redundant.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let option_all l =
  List.fold_right
    (fun x acc ->
      match (x, acc) with
      | Some x, Some l -> Some (x :: l)
      | __else__ -> None)
    l (Some [])

(* TODO: restrict to just AND case *)
let rec needed_labels_of_expr e =
  match e.G.e with
  | G.L (G.Bool (v, _)) -> if v then Some LabelSet.empty else None
  | G.N (G.Id (id, _)) ->
      let str, _ = id in
      Some (LabelSet.singleton str)
  | G.Call ({ e = G.IdSpecial (G.Op G.Not, _); _ }, (_, [ Arg _e1 ], _)) -> None
  | G.Call ({ e = G.IdSpecial (G.Op op, _); _ }, (_, args, _)) -> (
      let* needed_labels_of_args =
        Common.map
          (function
            | G.Arg e -> needed_labels_of_expr e
            | __else__ -> None)
          args
        |> option_all
      in
      match op with
      | G.And ->
          Some
            (List.fold_right
               (fun x acc -> LabelSet.union x acc)
               needed_labels_of_args LabelSet.empty)
      | G.Or -> None
      | __else__ ->
          logger#error "Unexpected Boolean operator";
          None)
  | G.ParenExpr (_, e, _) -> needed_labels_of_expr e
  | ___else__ ->
      logger#error "Unexpected `requires' expression";
      None
