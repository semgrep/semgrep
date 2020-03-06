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

(* currently used to go deeper in sgrep when someone wants that
 * a pattern like 'bar();' matches also an expression statement like
 * 'x = bar();'.
 * todo? we could restrict ourselves to only a few forms?
 *   - x = <expr>,
 *   - <call>(<exprs).
 *)

let subexprs_of_expr e = 
  match e with
  | L _ 
  | Id _ | IdQualified _  | IdSpecial _
  | Ellipsis _ | TypedMetavar _
    -> []

  | DotAccess (e, _, _) | Await (_, e) | Cast (_, e)
  | Ref (_, e) | DeRef (_, e)
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


