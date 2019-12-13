(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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

open Ast_generic
module Ast = Ast_generic
module V = Visitor_ast

module GG = Generic_vs_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://github.com/facebook/pfff/wiki/Sgrep 
 *
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* right now only Expr, Stmt, and Stmts are supported *)
type pattern = Ast.any

type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list

 
let match_e_e pattern e = 
  let env = GG.empty_environment () in
  GG.m_expr pattern e env

let match_st_st pattern e = 
  let env = GG.empty_environment () in
  GG.m_stmt pattern e env

let match_sts_sts pattern e = 
  let env = GG.empty_environment () in
  GG.m_stmts pattern e env

(* for unit testing *)
let match_any_any pattern e = 
  let env = GG.empty_environment () in
  GG.m_any pattern e env

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let sgrep_ast ~hook pattern ast =

  (* rewrite code, e.g., A != B is rewritten as !(A == B) *)
  let pattern = Normalize_ast.normalize pattern in
  let prog = Normalize_ast.normalize (Pr ast) in

  let hook =
    match pattern with
    (* depending on the pattern, we visit every relevant nodes
     * and try the pattern on it.
     *)

    | E pattern_expr ->
        { V.default_visitor with
          V.kexpr = (fun (k, _) x ->
            let matches_with_env = match_e_e pattern_expr  x in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_ast.ii_of_any (E x) in
              matches_with_env |> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | S pattern ->
        { V.default_visitor with
          V.kstmt = (fun (k, _) x ->
            let matches_with_env = match_st_st pattern x in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_ast.ii_of_any (S x) in
              matches_with_env |> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | Ss pattern ->
        { V.default_visitor with
          (* this is potentially slower than what we did in Coccinelle with
           * CTL. We try every sequences. Hopefully the first statement in
           * the pattern will filter lots of sequences so we need to do
           * the heavy stuff (e.g., handling '...' between statements) rarely.
           *)
          V.kstmts = (fun (k, _) x ->
            let matches_with_env = match_sts_sts pattern x in
            if matches_with_env = []
            then k x
            else begin
              (* could also recurse to find nested matching inside
               * the matched code itself.
               *)
              let matched_tokens = Lib_ast.ii_of_any (Ss x) in
              matches_with_env |> List.iter (fun env ->
                hook env matched_tokens
              )
            end
          );
        }

    | _ -> failwith (spf "pattern not yet supported:" )
  in
  (* later: opti: dont analyze certain ASTs if they do not contain
   * certain constants that interect with the pattern?
   * But this requires to analyze the pattern to extract those
   * constants (name of function, field, etc.).
   *)

  (V.mk_visitor hook) prog
