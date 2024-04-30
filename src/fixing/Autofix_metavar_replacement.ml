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
module G = AST_generic
module Log = Log_fixing.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Module responsible for traversing a fix pattern AST and replacing
 * metavariables within it with the AST nodes from the target file to which the
 * metavariables are bound.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let fb = Tok.unsafe_fake_bracket

let replace metavar_tbl pattern_ast =
  (* Use a mapper to traverse the AST. For each metavar, look up what it is
   * bound to. If the kind of node matches, replace the metavar in the pattern
   * with the AST node to which it is bound.
   *
   * Note that we can't handle this by simply visiting the identifier nodes,
   * because metavars are often bound to nodes other than identifiers. Instead,
   * we have to inspect certain kinds of nodes for metavars and then replace the
   * entire node. For example, if `foo(1, $X)` matches `foo(1, bar=5)`, if we
   * only overrode the `kident` function we would encounter `$X` and would then
   * be required to convert it into another value of type `ident`. To avoid this
   * problem, here we would need to check each argument to see if it is a
   * metavariable bound to an argument, and if so, do the replacement at that
   * point in the tree traversal. *)
  let mapper =
    object (_self : 'self)
      inherit [_] AST_generic.map as super

      (* TODO handle:
       * [x] ident
       * [ ] name
       * [x] expr
       * [ ] stmt
       * [ ] type_
       * [ ] pattern
       * [ ] stmt list
       * [x] argument list
       * [ ] parameter list
       * [ ] xml_body list
       * [x] text
       *)
      method! visit_arguments env (l, args, r) =
        (* A metavariable can appear as a single argument, but can be
         * bound to zero or more arguments, so we have to handle this case
         * by looking at an argument list as a whole. For example,
         * `foo(1, $...X)` matches `foo(1)` *)
        let map_arg arg =
          match arg with
          | Arg { e = N (Id ((id_str, _), _)); _ } -> (
              match Hashtbl.find_opt metavar_tbl id_str with
              | Some (MV.Args args) -> args
              | _ -> [ arg ])
          | _ -> [ arg ]
        in
        let args = List.concat_map map_arg args in
        super#visit_arguments env (l, args, r)

      method! visit_expr env orig =
        let replacement =
          match orig.e with
          | N (Id ((id_str, _), _)) -> (
              match Hashtbl.find_opt metavar_tbl id_str with
              | Some (MV.E e) -> Some e
              | _ -> None)
          | _ -> None
        in
        match replacement with
        | None -> super#visit_expr env orig
        | Some e ->
            (* We found a matching metavariable binding. There is no point
             * searching through the metavariable value that came from the
             * target file for more metavariables, so we won't call `k`
             * here. *)
            e

      method! visit_literal env lit =
        match lit with
        | String (_l, (str, _), _r) -> (
            (* TODO handle the case where the metavar appears within the
             * string but is not the entire contents of the string *)
            match Hashtbl.find_opt metavar_tbl str with
            | Some (MV.Text (str, _info, originfo)) ->
                (* Don't use `Metavariable.mvalue_to_any` here. It uses
                 * the modified token info, which drops the quotes. *)
                (* TODO? reuse l and r from String above? *)
                String (fb (str, originfo))
            | _ -> super#visit_literal env lit)
        | _ -> super#visit_literal env lit

      method! visit_name env name =
        match name with
        | Id ((id_str, _), _) -> (
            match Hashtbl.find_opt metavar_tbl id_str with
            | Some (MV.Id (id, info)) ->
                let info =
                  match info with
                  | Some x -> x
                  | None -> G.empty_id_info ()
                in
                Id (id, info)
            | _ -> super#visit_name env name)
        | _ -> super#visit_name env name
    end
  in
  mapper#visit_any () pattern_ast

(* TODO: note that the resulting regexp work only if metavar_tbl is not empty.
 * Otherwise this will generate the .*\(\).* regexp which can match anything
 * TODO: add unit test for this function.
 * TODO: switch to PCRE so can use better way to build a regexp from
 * subparts.
 *)
let mk_str_metavars_regexp metavar_tbl =
  if Hashtbl.length metavar_tbl =|= 0 then
    Log.warn (fun m ->
        m "no metavariables, mk_str_metavars_regexp should not be called");
  lazy
    ((* List of metavars that were bound in this match, quoted so that they
        * can be used safely in a regex *)
     let quoted_metavars =
       Hashtbl.to_seq_keys metavar_tbl |> Seq.map Str.quote |> List.of_seq
     in
     (* One regex string that will match any of the metavars *)
     let regex_body = String.concat "\\|" quoted_metavars in
     (* Match any text before or after the metavars, since Str.string_match
      * looks for the entire string to match, not just a substring like many
      * other tools. *)
     spf ".*\\(%s\\).*" regex_body)

(* Check for remaining metavars in the fixed pattern AST. If there are any, that
 * indicates a failure to properly replace them in the previous step, and the
 * autofix attempt should be aborted.
 *
 * This currently works by comparing all identifiers against the list of bound
 * metavariables. Some languages allow identifiers that look like metavariables,
 * so this is less likely to result in a false positive than if we checked for
 * any identifiers that could be metavariables. However, this wouldn't catch the
 * case of a malformed fix pattern where the user wrote a metavariable in the
 * fix that doesn't exist in the rule's pattern.
 * *)
let find_remaining_metavars metavar_tbl ast =
  let seen_metavars = ref [] in
  let str_metavars_regexp = mk_str_metavars_regexp metavar_tbl in
  let visitor =
    object
      inherit [_] AST_generic.iter_no_id_info as super

      method! visit_ident env id =
        let idstr, _ = id in
        if Hashtbl.mem metavar_tbl idstr then Stack_.push idstr seen_metavars;
        super#visit_ident env id

      method! visit_String env lit =
        let _, (str, _tok), _ = lit in
        (* Textual autofix allows metavars to appear anywhere within string
         * literals. This is useful when the pattern is something like
         * `foo("$X")` and you'd like the fix to modify the string literal, e.g.
         * `foo("bar $X")`. So, we have to look for a lingering metavariable
         * anywhere within a string, and abort if we find one that hasn't been
         * replaced. *)
        if str =~ Lazy.force str_metavars_regexp then
          Stack_.push (Common.matched1 str) seen_metavars;
        super#visit_String env lit
    end
  in
  visitor#visit_any () ast;
  !seen_metavars

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
let replace_metavars (metavars : MV.bindings) (pattern_ast : AST_generic.any) :
    (AST_generic.any, string) result =
  if List_.null metavars then Ok pattern_ast
  else
    let metavar_tbl = Hashtbl_.hash_of_list metavars in
    let res = replace metavar_tbl pattern_ast in
    match find_remaining_metavars metavar_tbl res with
    | [] -> Ok res
    | remaining ->
        let err =
          spf
            "Did not successfully replace metavariable(s) in the fix pattern: \
             %s"
            (String.concat ", " remaining)
        in
        Log.warn (fun m -> m "%s" err);
        Error err
