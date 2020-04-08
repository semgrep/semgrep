(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Various helper functions to extract subparts of AST elements.
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let subexprs_of_expr e = 
  match e with
  | L _ 
  | Id _ | IdQualified _  | IdSpecial _
  | Ellipsis _ | TypedMetavar _
    -> []

  | DotAccess (e, _, _) | Await (_, e) | Cast (_, e)
  | Ref (_, e) | DeRef (_, e) | DeepEllipsis (_, e, _)
    -> [e]
  | Assign (e1, _, e2) | AssignOp (e1, _, e2) 
  | ArrayAccess (e1, e2)
    (* not sure we always want to return 'e1' here *)
    -> [e1;e2] 
  | Conditional (e1, e2, e3) 
    -> [e1;e2;e3]
  | Tuple xs | Seq xs
    -> xs
  | Container (_, xs) 
    -> unbracket xs


  | Call (e, args) ->
      (* not sure we want to return 'e' here *)
      e::
      (args |> Common.map_filter (function
        | Arg e | ArgKwd (_, e) -> Some e 
        | ArgType _ | ArgOther _ -> None
      ))
  | SliceAccess (e1, e2opt, e3opt, e4opt) ->
      e1::([e2opt;e3opt;e4opt] |> List.map Common.opt_to_list |> List.flatten)
  | Yield (_, eopt, _) -> Common.opt_to_list eopt 
  | OtherExpr (_, anys) ->
      (* in theory we should go deeper in any *)
      anys |> Common.map_filter (function
        | E e -> Some e
        | _ -> None
      )

  (* currently skipped over but could recurse *)
  | Record _ 
  | Constructor _ 
  | Lambda _ 
  | AnonClass _
  | Xml _
  | LetPattern _ | MatchPattern _
    -> []
  | DisjExpr _ -> raise Common.Impossible

let substmts_of_stmt stmts = 
  let rec aux x = 
    (* return the current statement first, and add substmts *)
    x::(
    match x with
    (* we do not recurse inside function definitions *)
    | DefStmt _
    | DirectiveStmt _

    (* 0 *)
    | ExprStmt _ 
    | Return _ | Continue _ | Break _ | Goto _
    | Throw _
    | Assert _
    | OtherStmt _
    -> 
        []

    (* 1 *)
    | While (_, _, st) | DoWhile (_, st, _) 
    | For (_, _, st)
    | Label (_, st)
    | OtherStmtWithStmt (_, _, st)
      ->
        aux st

    (* 2 *)
    | If (_, _, st1, st2) -> 
        [st1; st2] |> List.map aux |> List.flatten

    (* n *)
    | Block xs -> 
        xs |> List.map aux |> List.flatten
    | Switch (_, _, xs) ->
        xs |> List.map snd |> List.map aux |> List.flatten
    | Try (_, st, xs, opt) ->
        ([st] |> List.map aux |> List.flatten) @
        (xs |> List.map Common2.thd3 |> List.map aux |> List.flatten) @
        (match opt with None -> [] | Some (_, st) -> [st])
    | DisjStmt _ -> raise Common.Impossible
    )
   in
   aux stmts

let flatten_substmts_of_stmts xs =
  xs |> List.map (fun x -> substmts_of_stmt x) |> List.flatten
