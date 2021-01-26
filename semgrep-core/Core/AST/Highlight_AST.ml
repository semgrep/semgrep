(* Yoann Padioleau
 *
 * Copyright (C) 2020 R2C
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

open AST_generic
open Highlight_code
open Entity_code
module E = Entity_code
module G = AST_generic
module PI = Parse_info
module V = Visitor_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting of AST_generic for codemap (and now also for efuns).
 *
 * This code can also be abused to generate the light database
 * and the TAGS file (because codemap needs to know about
 * def and use of entities), but you should now prefer to
 * base such analysis on graph_code_xxx.ml instead of this file.
 *
 * history:
 *  - generalized from highlight_ml.ml and highlight_js.ml
 *
*)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* totally ocaml specific *)
let h_builtin_modules = Common.hashset_of_list [
  "Pervasives"; "Common";
  "List"; "Hashtbl"; "Array"; "Stack";
  "String"; "Bytes"; "Str";
  "Sys"; "Unix"; "Gc";
  "Filename";
]

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)

let kind_of_body x =
  let def2 = Def2 fake_no_def2 in
  match x with
  | Lambda _ -> Entity (Function, def2)

  (* ocaml specific *)
  | Call (Id ((("ref", _)), _idinfo), _args) ->
      Entity (Global, def2)
  | Call (IdQualified ((("create", _),
                        { name_qualifier = Some (QDots ["Hashtbl", _]); _ }), _idinfo),
          _args) ->
      Entity (Global, def2)
  | _ -> Entity (Constant, def2)

(* todo: actually it can be a typedef alias to a function too
 * but this would require some analysis
*)
let kind_of_ty ty =
  let def2 = Def2 fake_no_def2 in
  match ty with
  | TyFun _ -> (FunctionDecl NoUse)

  (* ocaml specific *)
  | TyNameApply ((("ref", _), _), _) -> Entity (Global, def2)
  (* todo: should handle module aliases there too *)
  | TyNameApply ((("t", _),
                  { name_qualifier = Some (QDots ["Hashtbl", _]); _ }), _)->
      Entity (Global, def2)
  | _ -> Entity (Constant, def2)

let last_id xs =
  match List.rev xs with
  | x::_xs -> x
  | [] -> failwith "last_id: empty list of idents"

let info_of_name ((_s, info), _nameinfo) = info

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* try to better colorize identifiers, which can be many different things:
 * a field, a type, a function, a parameter, a local, a global, etc.
*)
let visit_program
    (already_tagged, tag)
    ast
  =

  let tag_id (_s, ii) categ =
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
    *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in
  let _tag_if_not_tagged ii categ =
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in

  (* ocaml specific *)
  let in_let = ref false in
  let in_try_with = ref false in

  let hooks =
    { V.default_visitor with

      V.kdef = (fun (k, _) x ->
        match x with
        | ({ name = EId (id, _); _}, def) ->
            (match def with
             | Signature ty ->
                 tag_id id (kind_of_ty ty);
                 k x

             | ModuleDef { mbody = body } ->
                 tag_id id (Entity (E.Module, Def2 fake_no_def2));
                 (match body with
                  | ModuleAlias name ->
                      let info = info_of_name name in
                      tag info (Entity (Module, Use2 fake_no_use2));
                  | _ -> ()
                 );
                 k x

             | TypeDef { tbody = G.Exception _ } ->
                 tag_id id (Entity (E.Exception, Def2 fake_no_def2));
                 k x
             | TypeDef { tbody = kind } ->
                 tag_id id (Entity (E.Type, Def2 fake_no_def2));
                 (* todo: ty_params *)
                 (match kind with
                  | OrType xs ->
                      xs |> List.iter (function
                        | OrConstructor (id, _) ->
                            tag_id id (Entity (Constructor, Def2 fake_no_def2))
                        | _ -> ()
                      )
                  | AndType (_, xs, _) ->
                      xs |> List.iter (function
                        | FieldStmt ({s=DefStmt({name=EId (id, _); _}, _);_})->
                            tag_id id (Entity (Field, (Def2 fake_no_def2)));
                        | _ ->  ()
                      );
                  | _ -> ()
                 );
                 k x

             | VarDef { vinit = Some body; vtype = _ }  ->
                 (if not !in_let
                  then tag_id id (kind_of_body body)
                  else tag_id id (Local Def)
                 );
                 Common.save_excursion in_let true (fun () ->
                   k x
                 )

             | FuncDef _ ->
                 (if not !in_let
                  then tag_id id (Entity (Function, (Def2 NoUse)))
                  else tag_id id (Local Def)
                 );
                 Common.save_excursion in_let true (fun () ->
                   k x
                 )

             | _ -> k x
            )
        | _ -> k x
      );
      (* JS
               V.kprop = (fun (k,_) x ->
                 (match x with
                  | Field {fld_name = PN name; fld_body = Some (Fun _); _} ->
                      tag_name name (Entity (E.Method, (Def2 fake_no_def2)));
                  | Field {fld_name = PN name; _ } ->
                      tag_name name (Entity (E.Field, (Def2 fake_no_def2)));
                  | _ -> ()
                 );
                 k x
               );
      *)
      V.kdir = (fun (k, _) x ->
        (match x with
         | ImportAll (_, DottedName xs, _) ->
             let id = last_id xs in
             tag_id id (Entity (Module, Use2 fake_no_use2))
         | _-> ()
        );
        k x
      );

      V.kname = (fun (k, _) x ->
        let (_id, infos) = x in
        (match infos.name_qualifier with
         | Some (QDots xs) ->
             xs |> List.iter (fun id ->
               tag_id id (Entity (Module, Use2 fake_no_use2))
             )
         | _ -> ()
        );
        k x
      );

      V.kparam = (fun (k, _) x ->
        (match x with
         | ParamPattern (PatId (id, _idinfo)) ->
             tag_id id (Parameter Def);
             (* less: let kpattern do its job? *)
         | ParamPattern _ -> ()
         | ParamClassic p | ParamRest (_, p) | ParamHashSplat (_, p) ->
             (match p.pname with
              | Some id ->
                  tag_id id (Parameter Def);
              | None -> ()
             )
         | ParamEllipsis _ | OtherParam _ -> ()
        );
        k x
      );

(*
    V.kargument = (fun (k, _) x ->
      (match x with
      | ArgImplicitTildeExpr (_, name) ->
        let info = Ast.info_of_name name in
        (* todo: could be a Parameter use, need scope analysis *)
        tag info (Local Use)
      | _ -> ()
      );
      k x
    );

      | LetPattern (pat, _tok, body) ->
          (match pat with
          | PatTyped (_, PatVar name, _, _ty, _) ->
              let info = Ast.info_of_name name in
              if not !in_let
              then tag info (kind_of_body body)
              else tag info (Local (Def))
          | _ -> ()
          );
          Common.save_excursion in_let true (fun () ->
            k x
          )
    );
*)

      V.kstmt = (fun (k, _) x ->
        match x.s with
        | Try (_try_tok, _e (*, tok_with*), _match_cases, _finally) ->
            (*tag tok_with (KeywordExn); *)
            (*k (Try (try_tok, e, tok_with, []));*)
            Common.save_excursion in_try_with true (fun () ->
              k x
            )
        | _ -> k x
      );

      V.kexpr = (fun (k, _) x ->
        match x with
        (* TODO: use generic AST based highlighter
              | Id (name, scope) ->
                 (match !scope with
                 | NotResolved | Global _ ->
                    tag_name name (Entity (E.Global, (Use2 fake_no_use2)))
                 | Local -> tag_name name (H.Local Use)
                 | Param -> tag_name name (H.Parameter Use)
                 );
              | Apply (Id (name, {contents = Global _ | NotResolved}), _) ->
                 tag_name name (Entity (E.Function, (Use2 fake_no_use2)));
              | Apply (Id (_name, {contents = Local | Param}), _) ->
                 (* todo: tag_name name PointerCall; *)
                 ()
        *)

        | Id (id, _idinfo) ->
            (* TODO could be a param, could be a local. use scope analysis
             * TODO could also be actually a func passed to a higher
             *  order function, as in List.map snd, or even x |> Common.sort
            *)
            (* could have been tagged as a function name in the rule below *)
            tag_id id (Local Use);
            k x

        | IdSpecial (kind, info) ->
            (match kind with
             | Eval -> tag info BadSmell
             | _ -> tag info Builtin
            );
            k x

        (* pad specific *)
        | Call (Id ((("=~", _)), _idinfo),
                (_, [_arg1; Arg (L (G.String (_, info)))], _)) ->
            tag info Regexp;
            k x
        (* ocaml specific *)
        | Call (Id ((("ref", info)), _idinfo), _args) ->
            tag info UseOfRef;
            k x

        | Call (Id (id, _idinfo), _args) ->
            tag_id id (Entity (Function, (Use2 fake_no_use2)));
            k x
        | Call (IdQualified ((id, {name_qualifier = qu; _}), _idinfo), _args)->
            (match qu with
             | Some (QDots [s2, info2]) when Hashtbl.mem h_builtin_modules s2->
                 tag info2 BuiltinCommentColor;
                 tag_id id Builtin;
             | _ ->
                 tag_id id (Entity (Function, (Use2 fake_no_use2)));
            );
            k x

        (* disambiguate "with" which can be used for match, try, or record *)
        | MatchPattern (_e1, (*tok_with,*) _match_cases) ->
            (*tag tok_with (KeywordConditional); *)
            k x

        (* JS TODO
                    | Apply (ObjAccess (_, _, PN name), _) ->
                        tag_name name (Entity (E.Method, (Use2 fake_no_use2)));
                    | Fun (_, Some name) ->
                        tag_name name (Entity (E.Function, (Use2 fake_no_use2)));
        *)
        | DotAccess (_e, tok, (EId (id, _) | EName (id, _))) ->
            (match PI.str_of_info tok with
             (* ocaml specific *)
             | "#" -> tag_id id (Entity (Method, (Use2 fake_no_use2)))

             | _ -> tag_id id (Entity (Field, (Use2 fake_no_use2)))
            );
            k x
        | G.Constructor (name, _eopt) ->
            let info = info_of_name name in
            tag info (Entity (Constructor,(Use2 fake_no_use2)));
            k x

        | Record (_, xs, _) ->
            xs |> List.iter (fun x ->
              match x with
              | FieldStmt ({s=DefStmt ({ name = EId (id, _idinfo); _}, _);_})->
                  tag_id id (Entity (Field, (Use2 fake_no_use2)));
              | _ -> ()
            );
            k x
        (* coupling: with how record with qualified name in ml_to_generic.ml *)
        | OtherExpr (OE_RecordFieldName, (N name)::_) ->
            let info = info_of_name name in
            tag info (Entity (Field, (Use2 fake_no_use2)));
            k x

        | _ -> k x
      );

      V.kpattern = (fun (k, _) x ->
        (match x with
         | PatConstructor ((id, _name_info), _popt) ->
             if !in_try_with
             then tag_id id (KeywordExn)
             else tag_id id (ConstructorMatch fake_no_use2)
         | PatId (id, _idinfo) ->
             tag_id id (Parameter Def)
         | PatRecord (_, xs, _) ->
             xs |> List.iter (fun (name, _pat) ->
               let info = info_of_name name in
               tag info (Entity (Field, (Use2 fake_no_use2)));
             )

         | _ -> ()
        );
        k x
      );

      V.ktype_ = (fun (k, _) t ->
        (match t with
         | TyId (id, _) ->
             tag_id id (Entity (Type, (Use2 fake_no_use2)))
         | TyIdQualified (name,_idinfo) ->
             let info = info_of_name name in
             tag info (Entity (Type, (Use2 fake_no_use2)))
         | TyNameApply (name, _ty_args) ->
             let info = info_of_name name in
             (* different color for higher-order types *)
             tag info TypeVoid;
             (* todo: ty_args *)
         | TyVar id ->
             tag_id id TypeVoid;
         | _ -> ()
        );
        k t
      );

    }
  in
  let v = V.mk_visitor hooks in
  v (Pr ast);
  ()
