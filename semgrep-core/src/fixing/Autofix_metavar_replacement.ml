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

open AST_generic
module MV = Metavariable

(******************************************************************************)
(* Module responsible for traversing a fix pattern AST and replacing
 * metavariables within it with the AST nodes from the target file to which the
 * metavariables are bound. *)
(******************************************************************************)

let make_metavar_tbl bindings =
  let tbl = Hashtbl.create (List.length bindings) in
  List.iter (fun (mvar, mvalue) -> Hashtbl.replace tbl mvar mvalue) bindings;
  tbl

let replace metavar_tbl pattern_ast =
  (* Use a mapper to traverse the AST. For each metavar, look up what it is
   * bound to. If the kind of node matches, replace the metavar in the pattern
   * with the AST node to which it is bound. Note that we can't handle this by
   * simply visiting the identifier nodes, because metavars are often bound to
   * nodes other than identifiers. Instead, we have to inspect certain kinds of
   * nodes for metavars and then replace the entire node. *)
  let mapper =
    Map_AST.(
      mk_visitor
        {
          default_visitor with
          (* TODO handle more kinds of nodes *)
          kargs =
            (fun (k, _) args ->
              (* A metavariable can appear as a single argument, but can be
               * bound to multiple arguments, so we have to handle this case by
               * looking at an argument list as a whole *)
              let map_arg arg =
                match arg with
                | Arg { e = N (Id ((id_str, _), _)); _ } -> (
                    match Hashtbl.find_opt metavar_tbl id_str with
                    | Some (MV.Args args) -> args
                    | _ -> [ arg ])
                | _ -> [ arg ]
              in
              let args = List.concat_map map_arg args in
              k args);
          kexpr =
            (fun (k, _) orig ->
              (* The mapper changes the ID of expressions. This stymies the
               * print-avoidance implemented later in the autofix process,
               * because the resulting AST nodes cannot be recognized as the
               * same as the original ones.
               *
               * To work around this, we use the original expression, except for
               * the mapped kind.
               *
               * There is a note in the mapper expressing uncertainty about
               * whether the ID should be reused or not. We should consider
               * changing that behavior in the mapper, in which case this
               * workaround would no longer be necessary.
               * *)
              let expr = k orig in
              { orig with e = expr.e });
        })
  in
  mapper.Map_AST.vany pattern_ast

(* Check for remaining metavars in the AST. If there are any, that indicates a
 * failure to properly replace them in the previous step, and the autofix
 * attempt should be aborted.
 *
 * This currently works by comparing all identifiers against the list of bound
 * metavariables. Some languages allow identifiers that look like metavariables,
 * so this is less likely to result in a false positive than if we checked for
 * any identifiers that could be metavariables. However, this wouldn't catch the
 * case of a malformed fix pattern where the user wrote a metavariable in the
 * fix that doesn't exist in the rule's pattern.
 * *)
let has_remaining_metavars metavar_tbl ast =
  let saw_metavar = ref false in
  let visitor =
    Visitor_AST.(
      mk_visitor
        {
          default_visitor with
          kident =
            (fun (k, _) id ->
              let idstr, _ = id in
              if Hashtbl.mem metavar_tbl idstr then saw_metavar := true;
              k id);
        })
  in
  visitor ast;
  !saw_metavar

(******************************************************************************)
(* Entry Point *)
(******************************************************************************)

(* Attempt to replace metavars within the AST with the nodes to which they are
 * bound. If this fails, return None.
 *
 * Failure causes include, but are not limited to:
 * - A mismatch between the kind of node expected at the metavar's position and
 *   the actual node to which the metavar is bound. For example, a metavar that
 *   is bound to a statement used in a position that expects an expression.
 * - A failure in this module to handle some specific case, leading to a
 *   metavar's continued presence in the tree after replacement has been
 *   attempted. In this case, this function should detect that and return None.
 * *)
let replace_metavars metavars pattern_ast =
  let metavar_tbl = make_metavar_tbl metavars in
  let res = replace metavar_tbl pattern_ast in
  if has_remaining_metavars metavar_tbl res then None else Some res
