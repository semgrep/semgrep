(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Cst_php
module Ast = Cst_php
module V = Visitor_php
module E = Error_php
module Ent = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * history: repeat what iproctor wanted for his strict-mode that I first
 * coded in hphpi (except it was dynamic checking back then).
 * 
 * related work:
 *  - miyamide
 *  - PHP-sat at http://strategoxt.org/PHP/PhpSat
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let _pr2, pr2_once = Common2.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Arity check helpers *)
(*****************************************************************************)
(* cf also type_php.ml *)

let no_check_when_contain = [
  "func_num_args";
  "func_get_args";
]

let contain_func_name_args_like any =
  let funcalls = Lib_parsing_php.get_funcalls_any any in
  no_check_when_contain |> List.exists (fun danger_func -> 
    List.mem danger_func funcalls
  )

(* introduced in hh *)
let is_var_args_function def =
  def.f_params |> Ast.unparen |> List.exists (function
  (* ... *)
  | Middle3 _ -> true
  | _ -> false
  )

let check_args_vs_params (callname, all_args) (defname, all_params) =

  let info = Cst_php.info_of_name callname in
  let str_def = Ast.str_of_ident defname in

  let rec aux args params = 
    match args, params with
    | [], [] -> ()
    | [], y::ys ->
        if y.p_default = None 
        then E.fatal info (E.NotEnoughArguments str_def)
        else aux [] ys
    | _x::_xs, [] ->
        E.fatal info (E.TooManyArguments str_def)
    | x::xs, y::ys ->
        (match x with
        (* erling's idea of wrong keyword argument check *)
        | Arg(Assign(IdVar(dn, _),_, _expr)) ->
            (match y with
            (* passing a keyword argument for a reference is bad *)
            | { p_ref = Some _; _ } ->
                let loc = Ast.info_of_dname dn in
                E.fatal loc (E.KeywordArgumentForRef);
            | _ -> ()
            );
            if not (Ast.str_of_dname dn =$= Ast.str_of_dname y.p_name)
            then
              let all_params_str = 
                all_params |> List.map (fun p -> Ast.str_of_dname p.p_name) in
              let severity =
                if List.mem (Ast.str_of_dname dn) all_params_str
                then E.ReallyReallyBad
                else 
                  (* todo: edit_distance *)
                  E.Bad
              in
              let loc = Ast.info_of_dname dn in
              let s = Ast.str_of_dname dn in
              let param = Ast.str_of_dname y.p_name in
              E.fatal loc (E.WrongKeywordArgument(s, param, severity))

        (* passing a ref to a function not expecting one is fail.
         *
         * todo: we could also force people to explicitly pass
         * a reference to a function expecting one, making it clearer
         * at the call site that this parameter is special, but
         * this would generate too many errors right now. At least
         * we should be consistent and do it nowhere or everywhere.
         *)
        | ArgRef (tok, _var) ->
            (match y.p_ref with
            | None -> 
                E.fatal tok (E.PassingUnexpectedRef)
            | Some _ -> ()
            )
        | _ -> ()
        );
        aux xs ys
  in
  aux all_args all_params

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

let visit_and_check_funcalls find_entity prog =
  let visitor = V.mk_visitor { V.default_visitor with

    V.kexpr = (fun (k,_) x ->
      match x with
      | Call (Id callname, args)  ->
         E.find_entity_and_warn find_entity (Ent.Function, callname)
         (function Cst_php.FunctionE def ->
           (* todo? memoize ? *)
           if is_var_args_function def ||
              contain_func_name_args_like (Body def.f_body)
           then pr2_once "not checking functions with calls to func_num_args()"
           else 
             check_args_vs_params 
               (callname,   args |> Ast.unparen |> Ast.uncomma)
               (def.f_name, def.f_params |> Ast.unparen |> Ast.uncomma_dots)
         | _ -> raise Impossible
         );
         k x

      | _ -> k x
    );
  } 
  in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let check_program a b = 
  Common.profile_code "Checker.functions" (fun () -> 
    visit_and_check_funcalls a b)
