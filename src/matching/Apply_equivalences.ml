(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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
module G = AST_generic
module H = AST_generic_helpers
module Flag = Flag_semgrep
module MV = Metavariable
module Eq = Equivalence

(*****************************************************************************)
(* Matchers for code equivalence mode *)
(*****************************************************************************)

let match_e_e_for_equivalences _ruleid env a b =
  Common.save_excursion Flag.equivalence_mode true (fun () ->
      Generic_vs_generic.m_expr_root a b env)

(*****************************************************************************)
(* Substituters *)
(*****************************************************************************)
let subst_e (bindings : MV.bindings) e =
  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.map_legacy as super

      method! visit_expr env x =
        match x.e with
        | N (Id ((str, _tok), _id_info)) when Mvar.is_metavar_name str -> (
            match List.assoc_opt str bindings with
            | Some (MV.Id (id, Some idinfo)) ->
                (* less: abstract-line? *)
                N (Id (id, idinfo)) |> G.e
            | Some (MV.E e) ->
                (* less: abstract-line? *)
                e
            | Some _ ->
                failwith
                  (spf "incompatible metavar: %s, was expecting an expr" str)
            | None ->
                failwith
                  (spf "could not find metavariable %s in environment" str))
        | __else__ -> super#visit_expr env x
    end
  in
  visitor#visit_expr () e

let apply equivs lang any =
  let expr_rules = ref [] in
  let stmt_rules = ref [] in

  equivs
  |> List.iter (fun { Eq.left; op; right; _ } ->
         match (left, op, right) with
         | E l, Eq.Equiv, E r ->
             Stack_.push (l, r) expr_rules;
             Stack_.push (r, l) expr_rules
         | E l, Eq.Imply, E r -> Stack_.push (l, r) expr_rules
         | S l, Eq.Equiv, S r ->
             Stack_.push (l, r) stmt_rules;
             Stack_.push (r, l) stmt_rules
         | S l, Eq.Imply, S r -> Stack_.push (l, r) stmt_rules
         | __else__ ->
             failwith "only expr and stmt equivalence patterns are supported");
  (* the order matters, keep the original order reverting Common.push *)
  let expr_rules = List.rev !expr_rules in
  let _stmt_rulesTODO = List.rev !stmt_rules in

  let visitor =
    object (_self : 'self)
      inherit [_] AST_generic.map_legacy as super

      method! visit_expr env x =
        (* transform the children *)
        let x' = super#visit_expr env x in

        let rec aux xs =
          match xs with
          | [] -> x'
          | (l, r) :: xs -> (
              (* look for a match on original x, not x' *)
              let matches_with_env =
                match_e_e_for_equivalences "<equivalence>" env l x
              in
              match matches_with_env with
              (* todo: should generate a Disj for each possibilities? *)
              | env :: _xs ->
                  (* Found a match *)
                  let alt = subst_e env.mv r (* recurse on r? *) in
                  (* TODO: use AST_generic.equal_any*)
                  if
                    H.abstract_for_comparison_any (E x)
                    =*= H.abstract_for_comparison_any (E alt)
                  then x' (* disjunction (if different) *)
                  else DisjExpr (x', alt) |> G.e
              (* no match yet, trying another equivalence *)
              | [] -> aux xs)
        in
        aux expr_rules

      method! visit_stmt _env x = x
    end
  in
  let config =
    { Rule_options.default with go_deeper_expr = false; go_deeper_stmt = false }
  in
  let env = Matching_generic.environment_of_any lang config any in
  visitor#visit_any env any
[@@profiling]
