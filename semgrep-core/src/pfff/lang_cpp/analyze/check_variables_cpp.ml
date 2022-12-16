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

open Ast_cpp

module Ast = Ast_cpp
module V = Visitor_cpp
module E = Error_code
module S = Scope_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A variable checker and local name resolver for C/C++.
 *
 * This file mostly deals with scoping issues. Scoping is different
 * from typing! Those are 2 orthogonal programming language notions.
 *
 * TODO: could move generic code in scope_code.ml
*)

(*****************************************************************************)
(* Types, constants *)
(*****************************************************************************)

type environment =
  (Ast.name * (Scope_code.t * int ref)) list list

(*****************************************************************************)
(* Environment *)
(*****************************************************************************)

(* Looking up a variable in the environment.
*)

let rec lookup_env2 s env =
  match env with
  | [] -> raise Not_found
  | []::zs -> lookup_env2 s zs
  | ((a,b)::xs)::zs ->
      if Ast.string_of_name_tmp a = s
      then b
      else lookup_env2 s (xs::zs)
let lookup_env a b =
  Common.profile_code "CheckVar.lookup_env" (fun () -> lookup_env2  a b)

let lookup_env_opt a b =
  Common2.optionise (fun () -> lookup_env a b)

let is_top_env env =
  List.length env = 1

(*****************************************************************************)
(* (Semi) Globals, Julia's style *)
(*****************************************************************************)

(* use a ref because we may want to modify it *)
let (initial_env: environment ref) =
  (* less:  Env_php.globals_builtins +> List.map (fun s ->
   * fake_dname s, (S.Global, ref 1)
   * )
  *)
  ref [[]]

(* opti: cache ? use hash ? *)
let _scoped_env = ref !initial_env

(* TODO use generic implem of Common ? *)
let new_scope() = _scoped_env := []::!_scoped_env
let del_scope() = _scoped_env := List.tl !_scoped_env
let top_scope() = List.hd !_scoped_env

(*
let do_in_new_scope f =
  begin
    new_scope();
    let res = f() in
    del_scope();
    res
  end
*)

let add_in_scope namedef =
  let (current, older) = Common2.uncons !_scoped_env in
  _scoped_env := (namedef::current)::older


let add_binding2 k v  =
(*
  let info = Ast.info_of_name_tmp k in
  if !Flag.debug_checker
  then pr2 (spf "adding binding %s"
               (Ast.string_of_info info));
*)
  add_in_scope (k, v)

let add_binding k v =
  Common.profile_code "CV.add_binding" (fun () -> add_binding2 k v)

(*****************************************************************************)
(* checks *)
(*****************************************************************************)

let do_in_new_scope_and_check f =
  new_scope();
  let res = f() in

  let top = top_scope () in
  del_scope();

  top |> List.rev |> List.iter (fun (name, (scope, aref)) ->
    if !aref = 0
    then
      let s = Ast.string_of_name_tmp name in
      let ii = List.hd (Ast.ii_of_id_name name) in
      E.error ii (E.UnusedVariable (s, scope))
  );
  res

(*****************************************************************************)
(* Scoped visitor *)
(*****************************************************************************)

(* For each introduced binding (param, exception, foreach, etc),
 * we add the binding in the environment with a counter, a la checkModule.
 * todo: ?(find_entity = None)
*)
let visit_prog prog =

  let hooks = { V.default_visitor with

                (* 1: scoping management *)
                V.kcompound =  (fun (k, _) x ->
                  do_in_new_scope_and_check (fun () -> k x)
                );

                V.kcpp = (fun (k, _) x ->
                  do_in_new_scope_and_check (fun () ->
                    (match x with
                     | Define (_, _id, DefineMacro params, _body) ->
                         params |> Ast.unparen |> List.iter (fun (s, ii) ->
                           add_binding (None, noQscope, IdIdent (s,ii)) (S.Param, ref 0);
                         );
                     | _ -> ()
                    );
                    k x
                  )
                );

                (* 2: adding defs of name in environment *)
                V.kparameter = (fun (k, _) param ->
                  (match param with
                   | P param
                   | ParamVariadic (_, _, param) ->
                       param.p_name  |> Option.iter (fun ident ->
                         add_binding (None, noQscope, IdIdent ident) (S.Param, ref 0);
                       );
                       (*| _ -> () *)
                   | ParamEllipsis _ -> ()
                   | ParamTodo _ -> ()
                  );
                  k param
                );

                V.kdeclaration = (fun (k, _) x ->
                  match x with
                  | DeclList (xs, _) ->
                      xs |> List.iter (fun onedecl ->
                        match onedecl with
                        (* TODO? *)
                        | EmptyDecl _ | TypedefDecl _ -> ()
                        | StructuredBinding _ | BitField _ -> () (* TODO *)
                        | V ({name; specs}, _) ->
                            let scope =
                              if is_top_env !_scoped_env ||
                                 (match specs with
                                  | [ST (Extern,_)] -> true
                                  | _ -> false
                                 )
                              then S.Global
                              else S.Local
                            in
                            add_binding name (scope, ref 0)
                      );
                      k x
                  | _ -> k x
                );


                (* 3: checking uses *)

                V.kexpr = (fun (k, _) x ->
                  match x with
                  | N (name, idinfo) ->
                      (* assert scope_ref = S.Unknown ? *)
                      let s = Ast.string_of_name_tmp name in
                      (match lookup_env_opt s !_scoped_env with
                       | None ->
                           idinfo.i_scope <- S.Global;
                       | Some (scope, _) ->
                           idinfo.i_scope <- scope;
                      );
                      k x
                  | _ -> k x
                );
              }
  in

  let visitor = V.mk_visitor hooks in
  visitor (Program prog)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let check_and_annotate_program2 prog =
  (* globals (re)initialialisation *)
  _scoped_env := !initial_env;
  visit_prog prog;
  ()

let check_and_annotate_program a =
  Common.profile_code "Checker.variables" (fun () ->
    check_and_annotate_program2 a)
