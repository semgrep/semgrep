(* Nat Mote
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
open AST_generic
module MV = Metavariable

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Handles AST printing for the purposes of autofix.
 *
 * The main printing logic happens elsewhere. This module's main purpose is to
 * extend the existing AST printers so that they can avoid printing AST nodes
 * which are lifted unchanged from either the target file (via metavariable
 * bindings) or the fix pattern in the rule. This serves two purposes:
 * - It lets us synthesize autofixes without having to implement printing for
 *   all of the AST nodes in the fixed AST.
 * - It lets us make minimal changes to target files by carrying over
 *   formatting, comments, etc. from the original code.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This lets us avoid the polymorphic hash function and polymorphic equality,
 * which will take into account extraneous information such as e_range, leading
 * to failed lookups. *)
module ASTTable = Hashtbl.Make (struct
  type t = AST_generic.any

  let equal = AST_generic.equal_any
  let hash = AST_generic.hash_any
end)

type ast_node_source =
  (* Indicates that a node came from the target file via a metavar binding *)
  | Target
  (* Indicates that a node came from the fix pattern *)
  | FixPattern

(* So that we can print by lifting the original source for unchanged AST nodes,
 * this indicates whether a given AST node came from the target file (via
 * metavariable bindings) or from the rule's fix pattern. Nodes that do not
 * appear in this table came from neither. *)
type ast_node_table = ast_node_source ASTTable.t

module PythonPrinter = Hybrid_print.Make (struct
  class printer = Ugly_print_AST.python_printer
end)

module JsTsPrinter = Hybrid_print.Make (struct
  class printer = Ugly_print_AST.jsts_printer
end)

module OCamlPrinter = Hybrid_print.Make (struct
  class printer = Ugly_print_AST.ocaml_printer
end)

let get_printer lang external_printer :
    (Ugly_print_AST.printer_t, string) result =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      Ok (new PythonPrinter.printer external_printer)
  | Lang.Js
  | Lang.Ts ->
      Ok (new JsTsPrinter.printer external_printer)
  | Lang.Ocaml -> Ok (new OCamlPrinter.printer external_printer)
  | __else__ -> Error (spf "No printer available for %s" (Lang.to_string lang))

let original_source_of_ast source any =
  let* start, end_ = AST_generic_helpers.range_of_any_opt any in
  let starti = start.Tok.pos.bytepos in
  let _, _, endi = Tok.end_pos_of_loc end_ in
  let len = endi - starti in
  let str = String.sub source starti len in
  Some str

let mvalue_to_any = function
  (* For autofix purposes, it's okay and in fact desirable to drop the info here.
   * Otherwise MV.Id gets converted to an expression before going into the table
   * that holds the original unchanged nodes. That works fine if it is used in an
   * expression in the fixed pattern AST, but if it's a lone identifier part of
   * something else, it gets missed.
   *
   * Concretely, in the pattern `foo.$F()`, `$F` is not its own expression.
   * *)
  | MV.Id (id, _) -> I id
  | other -> MV.mvalue_to_any other

(* Add each metavariable value to the lookup table so that it can be identified
 * during printing *)
let add_metavars (tbl : ast_node_table) metavars =
  List.iter
    (fun (_, mval) ->
      let any = mvalue_to_any mval in
      ASTTable.replace tbl any Target;
      (* For each metavariable binding that is a list of things, we need to
       * iterate through and add each item in the list to the table as well.
       *
       * For example, if `$...BAR` is bound to the separate arguments `1` and
       * `2`, in the example below, then the complete argument list `(1, 2)`
       * would never appear in the resulting AST that we attempt to print.
       * Despite that, we would like to reuse the original text for `1` and `2`.
       *
       * foo($...BAR, 5) -> foo(1, 2, 5)
       *
       * We don't need to recurse any deeper, because individual list items are
       * the smallest components of a metavariable value that will be lifted
       * into the resulting AST.
       * *)
      match mval with
      | MV.Args args ->
          List.iter (fun arg -> ASTTable.replace tbl (Ar arg) Target) args
      (* TODO iterate through other metavariable values that are lists *)
      | Id ((_, _), _)
      | N _
      | E _
      | S _
      | T _
      | P _
      | XmlAt _
      | Raw _
      | Ss _
      | Params _
      | Xmls _
      | Text _
      | Any _ ->
          ())
    metavars

(* Add each AST node from the fix pattern AST to the lookup table so that it can
 * be identified during printing.
 *
 * We add all nodes here, regardless of whether they made it intact into the
 * fixed pattern AST. Despite this, we will only use the original text for nodes
 * that have made it into the fixed pattern AST unchanged. If a node was
 * modified, e.g. if it contained a metavariable that was replaced, that node
 * will not be equal to the original node when we look it up during printing,
 * and therefore we won't get a hashtbl hit, and so we won't use the text for
 * the original node.
 * *)
let add_fix_pattern_ast_nodes (tbl : ast_node_table) ast =
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_argument env arg =
        ASTTable.replace tbl (Ar arg) FixPattern;
        super#visit_argument env arg

      method! visit_expr env e =
        ASTTable.replace tbl (E e) FixPattern;
        super#visit_expr env e
      (* TODO visit every node that is part of AST_generic.any *)
    end
  in
  visitor#visit_any () ast

let make_external_printer ~metavars ~target_contents ~fix_pattern_ast
    ~fix_pattern : AST_generic.any -> (Immutable_buffer.t, string) result =
  let tbl : ast_node_table = ASTTable.create 8 in
  add_metavars tbl metavars;
  add_fix_pattern_ast_nodes tbl fix_pattern_ast;
  fun any ->
    let/ node =
      match ASTTable.find_opt tbl any with
      | Some x -> Ok x
      | None ->
          Error "Node does not appear in the original target or fix pattern"
    in
    let/ str =
      (match node with
      | Target -> original_source_of_ast (Lazy.force target_contents) any
      | FixPattern -> original_source_of_ast fix_pattern any)
      |> Option.to_result
           ~none:"Failed to extract original text for AST node during printing"
    in
    Ok (Immutable_buffer.of_string str)

(*****************************************************************************)
(* Entry Point *)
(*****************************************************************************)

let print_ast ~lang ~metavars ~target_contents ~fix_pattern_ast ~fix_pattern
    fixed_ast =
  let external_printer =
    make_external_printer ~metavars ~target_contents ~fix_pattern_ast
      ~fix_pattern
  in
  let/ printer = get_printer lang external_printer in
  match printer#print_any fixed_ast with
  | Ok print_result -> Ok (Immutable_buffer.to_string print_result)
  | Error err -> Error ("Could not print fixed AST:\n" ^ err)
