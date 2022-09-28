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

let logger = Logging.get_logger [ __MODULE__ ]

(******************************************************************************)
(* Main module for AST-based autofix. This module will attempt to synthesize a
 * fix based a rule's fix pattern and the match's metavariable bindings. *)
(******************************************************************************)

let parse_pattern lang pattern =
  try Ok (Parse_pattern.parse_pattern lang pattern) with
  | Timeout _ as e -> Exception.catch_and_reraise e
  | e ->
      let e = Exception.catch e in
      Error e

(* Check whether the proposed fix results in syntactically valid code *)
let validate_fix lang text =
  (* TODO Apply fix to original text and try parsing the whole program *)
  (* Since the fix is likely a program fragment, and not a valid top level
   * program, attempt to parse it as a pattern *)
  match parse_pattern lang text with
  | Ok _ -> true
  | Error e ->
      logger#info "Rendered autofix does not parse. Aborting: `%s`:\n%s" text
        (Exception.to_string e);
      false

(******************************************************************************)
(* Entry Point *)
(******************************************************************************)

(* Attempts to render a fix. If successful, returns the text that should replace
 * the matched range in the target file. If unsuccessful, returns None.
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
  let* fix_pattern_ast =
    match parse_pattern lang fix_pattern with
    | Ok x -> Some x
    | Error e ->
        logger#info "Failed to parse autofix `%s`:\n%s" fix_pattern
          (Exception.to_string e);
        None
  in

  (* Look through the fix pattern's AST and replace metavariables with the nodes to
   * which they are bound in the match. This should generate a well-formed AST,
   * which when printed to text, should replace the range in the original match.
   *
   * We need to do this instead of just replacing metavars with their original
   * text during printing. It's important for correctness to construct a
   * well-formed AST as an intermediate step. For example, an ellipsis
   * metavariable ($...X) might be bound to zero arguments in a function call
   * (foo(1, $...X) would match foo(1), for example). If we were to skip this
   * step, we would end up printing the extraneous comma before `$...X`.
   *
   * As we improve autofix, we may also want to perform other operations over
   * the fixed AST.
   * *)
  let* fixed_pattern_ast =
    Autofix_metavar_replacement.replace_metavars metavars fix_pattern_ast
  in

  (* Try to print the fixed pattern AST. *)
  let* text =
    Autofix_printer.print_ast ~lang ~metavars ~target_contents ~fix_pattern_ast
      ~fix_pattern fixed_pattern_ast
  in

  (* Perform sanity checks for the resulting fix. If they fail, return None *)
  if validate_fix lang text then Some text else None
