(* Heejong Lee, Cooper Pierce
 *
 * Copyright (C) 2024-2025 Semgrep Inc.
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
module G = AST_generic
module Log = Log_engine.Log

let hook_is_kind = Hook.create None
let hook_module_resolver = Hook.create None

let is_from_module module_path (e : G.expr) =
  let name =
    match e.e with
    | N name -> Some name
    | DotAccess _ -> AST_generic_helpers.name_of_dot_access e
    | _ -> None
  in
  match name with
  | Some name ->
      let name_path =
        match name with
        | Id ((id, _), { id_resolved; _ }) -> (
            match !id_resolved with
            | Some (GlobalName (cname, _), _) -> cname
            | _ -> [ id ])
        | IdQualified _ ->
            AST_generic_helpers.dotted_ident_of_name name |> List_.map fst
        | IdSpecial ((spec, _), { id_resolved; _ }) -> (
            match !id_resolved with
            | Some (GlobalName (cname, _), _) -> cname
            | _ -> [ G.show_special_ident spec ])
      in
      Base.List.is_prefix ~prefix:module_path ~equal:String.equal name_path
  | None -> false

let match_fqn (env : Match_env.env) lang e pat =
  Log.debug (fun m ->
      m "Matching metavariable-name(fqn):\n  Pattern: %s\n  Target: %s" pat
        (G.show_expr e));
  let/ ast = Parse_pattern.parse_pattern lang pat in
  let/ fqn =
    match ast with
    (* TODO: Should we disallow metavariables in `expr` or only allow
       the anonymous metavariable `$_`? It would probably be okay to
       allow metavariables since there's no way to perform further
       operations (like `metavariable-regex`) on the metavariable
       matched from the `fqn` pattern. However, we might want to
       revisit this issue before making the `fqn` filter publicly
       available. *)
    | G.E ({ e = DotAccess _ | N _; _ } as expr) -> Ok expr
    | _ ->
        Error
          (spf "only (qualified) names are supported in metavariable-name: %s"
             pat)
  in
  let/ canonical_expr =
    match e with
    | {
        G.e =
          N
            ( Id
                ( (_, tok),
                  {
                    id_resolved =
                      {
                        contents =
                          Some
                            ( ( ImportedEntity canonical
                              | ImportedModule canonical
                              | GlobalName (canonical, _) ),
                              _ );
                      };
                    _;
                  } )
            | IdQualified
                {
                  name_last = (_, tok), _;
                  name_info =
                    {
                      id_resolved =
                        {
                          contents =
                            Some
                              ( ( ImportedEntity canonical
                                | ImportedModule canonical
                                | GlobalName (canonical, _) ),
                                _ );
                        };
                      _;
                    };
                  _;
                } );
        _;
      }
    | {
        G.e =
          DotAccess
            ( _,
              _,
              FN
                (Id
                   ( (_, tok),
                     {
                       id_resolved =
                         {
                           contents =
                             Some
                               ( ( ImportedEntity canonical
                                 | ImportedModule canonical
                                 | GlobalName (canonical, _) ),
                                 _ );
                         };
                       _;
                     } )) );
        _;
      } ->
        let name =
          canonical |> G.canonical_to_dotted tok
          |> AST_generic_helpers.name_of_ids
        in
        Ok (N name |> G.e)
    | _ -> Error (spf "cannot find resolved name in mvalue")
  in
  let out =
    Matching_generic.environment_of_any lang
      (env.rule.options ||| Rule_options.default)
      (G.E e)
    |> Pattern_vs_code.m_expr_root fqn canonical_expr
  in
  Ok (out <> [])

(* entry point*)
let find_name (env : Match_env.env) e
    ({ kind; modules; fqns; _ } : Rule.metavar_cond_name) =
  let result =
    let* lang =
      match env.xtarget.analyzer with
      | Analyzer.L (lang, _) -> Some lang
      | Analyzer.LRegex
      | Analyzer.LSpacegrep
      | Analyzer.LAliengrep ->
          Match_env.error env
            "metavariable-name operator is not supported for regex, spacegrep, \
             aliengrep analyzers";
          None
    in
    let kind_matches =
      match kind with
      | None -> true
      | Some kind ->
          let k =
            let* is_kind =
              match Hook.get hook_is_kind with
              | Some f -> Some f
              | None ->
                  Match_env.error env
                    "metavariable-name:kind operator is only supported in the \
                     Pro engine";
                  None
            in
            Some (is_kind kind e)
          in
          k ||| false
    in
    let module_matches =
      match modules with
      | None -> true
      | Some ms ->
          let m =
            let* module_resolver =
              match Hook.get hook_module_resolver with
              | Some f -> Some f
              | None ->
                  Match_env.error env
                    "metavariable-name:module(s) operator is only supported in \
                     the Pro engine";
                  None
            in
            List.exists
              (fun m ->
                let module_path = module_resolver [ m ] in
                is_from_module module_path e)
              ms
            |> Option.some
          in
          m ||| false
    in
    let fqn_matches =
      match fqns with
      | None -> true
      | Some pats ->
          List.exists
            (fun pat ->
              match match_fqn env lang e pat with
              | Ok b ->
                  if b then Log.debug (fun m -> m "found fqn match: %s" pat)
                  else Log.debug (fun m -> m "not found fqn match: %s" pat);
                  b
              | Error str ->
                  Log.debug (fun m -> m "cannot match fqn: %s" str);
                  false)
            pats
    in
    Some (kind_matches && module_matches && fqn_matches)
  in
  result ||| false
