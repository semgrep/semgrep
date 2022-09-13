(* Nat Mote
 *
 * Copyright (C) 2019-2022 r2c
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

(******************************************************************************)
(* Main module for AST-based autofix. This module will attempt to synthesize a
 * fix based a rule's fix pattern and the match's metavariable bindings. *)
(******************************************************************************)

let parse_pattern_opt lang pattern =
  try Some (Parse_pattern.parse_pattern lang pattern) with
  | Timeout _ as e -> Exception.catch_and_reraise e
  | _ -> None

(* Check whether the proposed fix results in syntactically valid code *)
let validate_fix lang text =
  (* TODO Apply fix to original text and try parsing the whole program *)
  (* Since the fix is likely a program fragment, and not a valid top level
   * program, attempt to parse it as a pattern *)
  match parse_pattern_opt lang text with
  | Some _ -> true
  | None -> false

(******************************************************************************)
(* Entry Point *)
(******************************************************************************)

(* Attempts to render a fix. If unsuccessful, returns None.
 *
 * Failure causes include, but are not limited to:
 * - The fix pattern does not parse.
 * - A metavariable is bound to an AST node that is not suitable for the context
 *   in which it is used in the fix.
 * - Printing of the resulting fix AST fails (probably because there is simply a
 *   node that is unhandled).
 * *)
let render_fix lang metavars ~fix_pattern ~target_contents =
  (* Fixes are not exactly patterns, but they can contain metavariables that
   * should be substituted with the nodes to which they are bound in the match.
   * Because they can contain metavariables, we need to parse them as patterns.
   * *)
  let* fix_pattern_ast = parse_pattern_opt lang fix_pattern in

  (* Look through the pattern and replace metavariables with the nodes to which
   * they are bound in the match. *)
  let* fixed_ast =
    Autofix_metavar_replacement.replace_metavars metavars fix_pattern_ast
  in

  (* Try to print the fixed AST. *)
  let* text =
    Autofix_printer.print_ast ~lang ~metavars ~target_contents ~fix_pattern_ast
      ~fix_pattern fixed_ast
  in

  (* Perform sanity checks for the resulting fix. If they fail, return None *)
  if validate_fix lang text then Some text else None
