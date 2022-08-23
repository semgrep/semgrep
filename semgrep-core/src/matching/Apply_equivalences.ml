(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
open Common
open AST_generic
module G = AST_generic
module H = AST_generic_helpers
module Flag = Flag_semgrep
module MV = Metavariable
module M = Map_AST
module Eq = Equivalence
module Env = Metavariable_capture

(*****************************************************************************)
(* Matchers for code equivalence mode *)
(*****************************************************************************)

let match_e_e_for_equivalences _ruleid lang a b =
  Common.save_excursion Flag.equivalence_mode true (fun () ->
      let config =
        {
          Config_semgrep.default_config with
          go_deeper_expr = false;
          go_deeper_stmt = false;
        }
      in
      let cache = None in
      let env = Matching_generic.empty_environment cache lang config in
      Generic_vs_generic.m_expr_root a b env)

(*****************************************************************************)
(* Substituters *)
(*****************************************************************************)
let subst_e (env : Env.t) e =
  let bindings = env.full_env in
  let visitor =
    M.mk_visitor
      {
        M.default_visitor with
        M.kexpr =
          (fun (k, _) x ->
            match x.e with
            | N (Id ((str, _tok), _id_info)) when MV.is_metavar_name str -> (
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
            | _ -> k x);
      }
  in
  visitor.M.vexpr e

let apply equivs lang any =
  let expr_rules = ref [] in
  let stmt_rules = ref [] in

  equivs
  |> List.iter (fun { Eq.left; op; right; _ } ->
         match (left, op, right) with
         | E l, Eq.Equiv, E r ->
             Common.push (l, r) expr_rules;
             Common.push (r, l) expr_rules
         | E l, Eq.Imply, E r -> Common.push (l, r) expr_rules
         | S l, Eq.Equiv, S r ->
             Common.push (l, r) stmt_rules;
             Common.push (r, l) stmt_rules
         | S l, Eq.Imply, S r -> Common.push (l, r) stmt_rules
         | _ -> failwith "only expr and stmt equivalence patterns are supported");
  (* the order matters, keep the original order reverting Common.push *)
  let expr_rules = List.rev !expr_rules in
  let _stmt_rulesTODO = List.rev !stmt_rules in

  let visitor =
    M.mk_visitor
      {
        M.default_visitor with
        M.kexpr =
          (fun (k, _) x ->
            (* transform the children *)
            let x' = k x in

            let rec aux xs =
              match xs with
              | [] -> x'
              | (l, r) :: xs -> (
                  (* look for a match on original x, not x' *)
                  let matches_with_env =
                    match_e_e_for_equivalences "<equivalence>" lang l x
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
            aux expr_rules);
        M.kstmt = (fun (_k, _) x -> x);
      }
  in
  visitor.M.vany any
  [@@profiling]
