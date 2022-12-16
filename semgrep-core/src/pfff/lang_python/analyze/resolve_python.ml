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
open AST_python
open Common
module Ast = AST_python
module V = Visitor_python

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Identifiers tagger (so we can colorize them differently in codemap/efuns).
 *
 * TODO: switch gradually the code to naming_ast.ml generic tagger.
 * note: This is not called anymore from parse_generic.ml. This is not
 * used anymore by sgrep (but still used by codemap/efuns)
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type context =
  | AtToplevel
  | InClass
  | InFunction
  (* TODO: InLambda *)

type env = {
  ctx: context ref;
  names: (string * resolved_name) list ref;
}

let default_env () = {
  ctx = ref AtToplevel;
  names = ref [];
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* because we use a Visitor instead of a clean recursive
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
*)
let with_added_env xs env f =
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let add_name_env name kind env =
  env.names := (Ast.str_of_name name, kind)::!(env.names)

let with_new_context ctx env f =
  Common.save_excursion env.ctx ctx f

let params_of_parameters tok params =
  let param_pattern_name ix = function
    | PatternName name -> name
    | PatternTuple _names ->
        (* We factor all tuple patterns into a common generic name, so these are useless for further analysis.
         * They will, however, parse.
         * TODO: Add factoring similar to lang_js/analyze/transpile_js patterns
        *)
        (spf "!arg%d!" ix, Parse_info.fake_info tok "tuple")
  in
  let collect_param_names (ix, out) = function
    | ParamPattern (pat, _) ->
        (ix + 1, (param_pattern_name ix pat)::out)
    | ParamDefault ((name, _), _)
    | ParamStar (_, (name, _)) | ParamPow (_, (name, _)) ->
        (ix + 1, name::out)
    | ParamSingleStar _ | ParamSlash _ | ParamEllipsis _ ->
        (ix + 1, out)
  in
  let zipped = List.fold_left collect_param_names (0, []) params
  in
  (match zipped with | (_, names) -> names)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve prog =
  let env = default_env () in

  (* helper to factorize code related to (polymorphic comprehension) *)
  let comprehension ke v (e, xs) =
    let new_vars =
      xs |> Common.map_filter (function
        | CompFor (target, _) ->
            (match target with
             | Name (name, _ctx, _res) -> Some name
             (* tuples? *)
             | _ -> None
            )
        | CompIf _ -> None
      )
    in
    let new_names = new_vars |> List.map (fun name ->
      Ast.str_of_name name, Ast.LocalVar
    ) in
    with_added_env new_names env (fun () -> ke e);

    (* TODO: should fold and use new env *)
    xs |> List.iter (function
      | CompFor (e1, e2) ->
          v (Expr e1); v (Expr e2)
      | CompIf e ->
          v (Expr e)
    )
  in

  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
                               (* old: No need to resolve at the definition sites (for params, locals).
                                * This will be patterned-match specially anyway in the highlighter. What
                                * you want is to tag the use sites, and to maintain the right environment.
                                * update: we need to tag the def sites like the use sites! otherwise sgrep
                                * will not be able to equal a metavar matching a def and later a use.
                                * todo: this should be done better at some point in naming_ast.ml
                               *)
                               V.kexpr = (fun (k, v) x ->
                                 match x with
                                 | Name (name, ctx, resolved) ->
                                     (match ctx with
                                      | Load | AugLoad ->
                                          (* assert resolved = NotResolved *)
                                          let s = Ast.str_of_name name in
                                          (match List.assoc_opt s !(env.names) with
                                           | Some x -> resolved := x
                                           | None -> () (* will be tagged as Error by highlighter later *)
                                          )
                                      | Store | AugStore ->
                                          let kind =
                                            let s = Ast.str_of_name name in
                                            match List.assoc_opt s !(env.names) with
                                            (* can happen if had a 'Global' declaration before *)
                                            | Some x -> x
                                            | None ->
                                                (match !(env.ctx) with
                                                 | AtToplevel -> GlobalVar
                                                 | InClass -> ClassField
                                                 | InFunction -> LocalVar
                                                )
                                          in
                                          env |> add_name_env name kind;
                                          resolved := kind; (* optional *)

                                      | Del -> (* should remove from env *)
                                          ()
                                      | Param ->
                                          resolved := Parameter; (* optional *)
                                     );
                                     k x
                                 | Tuple (CompForIf (_, x, _), _)
                                 | List (CompForIf (_, x, _), _)
                                   (* bugfix: do not pass just k here, because we want to intercept
                                    * the processing of 'e' too!
                                   *)
                                   -> comprehension (fun e -> v (Expr e)) v x
                                 | DictOrSet (CompForIf (_, x, _)) ->
                                     comprehension (fun elt -> v (DictElem elt)) v x

                                 (* general case *)
                                 | _ -> k x
                               );
                               V.kstmt = (fun (k, v) x ->
                                 match x with
                                 | FunctionDef (t, name, params, _typopt, _body, _decorators) ->
                                     let new_params = params_of_parameters t (params: parameters) in
                                     let new_names = new_params |> List.map (fun name ->
                                       Ast.str_of_name name, Ast.Parameter
                                     ) in
                                     with_added_env new_names env (fun () ->
                                       with_new_context InFunction env (fun () ->
                                         k x
                                       ));
                                     (* nested function *)
                                     if !(env.ctx) = InFunction
                                     then env |> add_name_env name (LocalVar);
                                 | ClassDef (_t, name, _bases, _body, _decorators) ->
                                     env |> add_name_env name (GlobalVar);
                                     with_new_context InClass env (fun () ->
                                       k x
                                     )

                                 | ImportAs (_, (dotted_name, _dotsTODO), asname_opt) ->
                                     asname_opt |> Option.iter (fun asname ->
                                       env |> add_name_env asname (ImportedModule dotted_name)
                                     );
                                     k x

                                 | ImportFrom (_, (dotted_name, _dotsTODO), aliases) ->
                                     aliases |> List.iter (fun (name, asname_opt) ->
                                       let entity = dotted_name @ [name] in
                                       (match asname_opt with
                                        | None ->
                                            env |> add_name_env name (ImportedEntity entity);
                                        | Some asname ->
                                            env |> add_name_env asname (ImportedEntity entity)
                                       );
                                     );
                                     k x
                                 | With (_, (e, eopt), stmts) ->
                                     v (Expr e);
                                     (match eopt with
                                      | None -> v (Stmts stmts)
                                      | Some (Name (name, _ctx, _res)) ->
                                          (* the scope of name is valid only inside the body, but the
                                           * body may define variables that are used then outside the with
                                           * so simpler to use add_name_env() here, not with_add_env()
                                             let new_names = (fst name, LocalVar)::!(env.names) in
                                             with_added_env new_names env (fun () ->
                                             v (Stmts stmts)
                                             )
                                          *)
                                          env |> add_name_env name LocalVar;
                                          v (Stmts stmts);
                                          (* todo: tuples? *)
                                      | Some e ->
                                          v (Expr e);
                                          v (Stmts stmts)
                                     )
                                 | TryExcept (_, stmts1, excepts, stmts2) ->
                                     v (Stmts stmts1);
                                     excepts |> List.iter (fun (ExceptHandler (_t, _typ, e, body)) ->
                                       match e with
                                       | None -> v (Stmts body)
                                       | Some name ->
                                           let new_names = (fst name, LocalVar)::!(env.names) in
                                           with_added_env new_names env (fun () ->
                                             v (Stmts body)
                                           )
                                     );
                                     v (Stmts stmts2);

                                 | Global (_, names) ->
                                     names |> List.iter (fun name -> env |> add_name_env name GlobalVar;)

                                 (* TODO: NonLocal!! *)

                                 (* general case *)
                                 | _ -> k x
                               );
                             } in
  visitor (Program prog)
