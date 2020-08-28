(* Yoann Padioleau
 *
 * Copyright (C) 2010-2011 Facebook
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
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Checking the use of method calls, member fields,
 * TODO class variables, TODO class constants, SEMI and class names.
 * TODO check interface
 * TODO check traits
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let _pr2, pr2_once = Common2.mk_pr2_wrappers Flag_analyze_php.verbose_checking

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type context_call =
  | StaticCall
  | MethodCall of bool (* abstract class ? *)

type context_access =
  | StaticAccess
  | ObjAccess

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let check_method_call context (aclass, amethod) (name, args) find_entity =
  let loc = Ast.info_of_name name in
  try
    let def =
      Class_php.lookup_method
        (* todo: remove at some point, but too many errors for now *)
        ~case_insensitive:true
        (aclass, amethod) find_entity in
    if Check_functions_php.contain_func_name_args_like (Toplevel (FuncDef def))
    then pr2_once ("not checking calls to code using func_num_args()-like")
    else begin
      (* check if used in the right way ... *)
      (match context, Class_php.is_static_method def with
      | StaticCall, true -> ()
      | MethodCall _, false -> ()
      | StaticCall, false ->
          if amethod =$= Class_php.constructor_name
          then ()
          else E.fatal loc (E.CallingMethodWithQualifier amethod)
      | MethodCall _, true ->
          E.fatal loc (E.CallingStaticMethodWithoutQualifier amethod)
      );
      Check_functions_php.check_args_vs_params
        (name, args |> Ast.unparen |> Ast.uncomma)
        (def.f_name, def.f_params |> Ast.unparen |> Ast.uncomma_dots)
    end
  with
  (* not much we can do then, let's bailout? *)
  | Class_php.Use__Call ->
      pr2_once "not analyzing code using __call"
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal loc (E.UndefinedClassWhileLookup (s))
  | Not_found ->
      (match context with
      | StaticCall ->
          E.fatal loc (E.UndefinedEntity (Ent.Method,amethod))
      | MethodCall false ->
          E.fatal loc (E.UndefinedEntity (Ent.Method,amethod))
      | MethodCall true ->
          E.fatal loc (E.UndefinedMethodInAbstractClass amethod)
      )
  (* this can happen with multiple classes with same name, not our job *)
  | Multi_found -> ()

let check_member_access ctx (aclass, afield) loc find_entity =
  try
    let _ =
      Class_php.lookup_member
        (* todo: remove at some point, but too many errors for now *)
        ~case_insensitive:true
        (aclass, afield) find_entity
    in
    (* todo: check if used in the right way ... *)
    ()
  with
  (* todo: Use__Get and other magic shit?  *)
  | Not_found ->
      (match ctx with
      | StaticAccess ->
          E.fatal loc (E.UndefinedEntity (Ent.Field, afield))
      | ObjAccess ->
          let allmembers = Class_php.collect_members aclass find_entity
            |> List.map Ast.str_of_dname
          in
          (* todo? could also show a strong warning when the list
           * allmembers is big, in which case if most of the
           * fields were defined, it makes sense to force people to
           * define them all.
           *)
          let suggest = Suggest_fix_php.suggest afield allmembers in
          E.fatal loc (E.UseOfUndefinedMember (afield, suggest))
      )
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal loc (E.UndefinedClassWhileLookup s)
  | Multi_found -> ()

let check_class_constant (aclass, s) tok find_entity =
  try
    let _ = Class_php.lookup_constant (aclass, s) find_entity in
    ()
  with
  | Not_found ->
      E.fatal tok (E.UndefinedEntity (Ent.ClassConstant, s))
  | Class_php.UndefinedClassWhileLookup s ->
      E.fatal tok (E.UndefinedClassWhileLookup s)
  | Multi_found -> ()

let extract_required_fields node graph =
  let fields = G.succ node G.Has graph in
  fields |> List.filter (fun node ->
    try
      let nodeinfo = G.nodeinfo node graph in
      let props = nodeinfo.G.props in
      List.mem Ent.Required props
    with Not_found -> false)
  |> List.map (fun (name, _) ->
    if (name =~ ".*\\.\\(.*\\)=")
    then matched1 name
    else failwith "bad field name in check_required_field"
  )

let visit_f_body f_body current_class graph=
  let acc = ref [] in
  let visitor = V.mk_visitor { Visitor_php.default_visitor with
    V.kexpr = (fun (k, _) e ->
      match e with
      | XhpHtml(Xhp((xhp_tag, loc), xhp_attr_list, _, _, _))
      | XhpHtml(XhpSingleton((xhp_tag, loc), xhp_attr_list, _)) ->
        let xhp_str = Ast_php.string_of_xhp_tag xhp_tag in
        let xhp_node = (xhp_str, Ent.Class) in
        if (G.has_node xhp_node graph)
        then
          let required_fields =
            extract_required_fields xhp_node graph in
          let has_fields = xhp_attr_list
            |> List.map (fun ((name, _), _, _) -> name) in
          let (_common, _only_in_first, only_in_second) =
            Common2.diff_set_eff has_fields required_fields
          in
          let tok_and_error = only_in_second |> Common.map_filter (fun f ->
            match current_class with
            | Some class_str ->
              (match Graph_code_php.lookup_inheritance graph 
                  (Graph_code_php.R class_str, f^"=")
                  () with
              | Some ((Graph_code_php.R _full_name, _), _) ->
                (* This is most likely correct, see :ui:base::renderAndProcess()
                   let suggest =
                   (spf "%s=$this->getAttribute(\'%s\')" f f, 1) in
                   Some (loc, (E.UndefinedRequiredField (f, suggest)))
                *)
                None
              | None ->
                Some (loc, (E.UndefinedRequiredField (f, None)))
              )
            | None ->
              Some (loc, (E.UndefinedRequiredField (f, None)))
          ) in
          acc := tok_and_error@(!acc);
          k e
        else
          k e
      | Call(ObjGet(_, _, Id(method_name)), _) ->
        let method_str = Ast.str_of_name method_name in
        if (method_str =~ ".*set.*Attribute.*")
        then acc := []
        else k e
      | _ -> k e
    );
  } in
  visitor (Body f_body);
  List.rev !acc

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

(* pre: have a unsugar AST regarding self/parent *)
let visit_and_check  find_entity prog =

  (* less: similar to what we do in unsugar_self_parent, do "$this"
   * unsugaring there too?
   *)
  let in_class = ref (None: (string * bool) option) in
  let in_trait = ref false in

  let visitor = V.mk_visitor { Visitor_php.default_visitor with

    V.kclass_def = (fun (k, _) def ->
      let is_abstract =
        match def.c_type with ClassAbstract _ -> true | _ -> false in
      let is_trait =
        match def.c_type with Trait _ -> true | _ -> false in

      def.c_extends |> Common.do_option (fun (_tok, parent) ->
        let parent = name_of_class_name parent in
        E.find_entity_and_warn find_entity (Ent.Class, parent)
          (fun _ ->
          ()
        )
      );

      Common.save_excursion in_class (Some (Ast.str_of_ident def.c_name, is_abstract))
      (fun () ->
      Common.save_excursion in_trait is_trait
      (fun () ->
          k def
        ))
    );

    V.kexpr = (fun (k,vx) x ->
      match x with
      | New (_tok, ((Id (class_name))), _args) ->
          (* todo: use lookup_method *)
          E.find_entity_and_warn find_entity (Ent.Class, class_name)
          (function Cst_php.ClassE _def ->
            (*
              Check_functions_php.check_args_vs_params
              (callname,   args +> Ast.unparen +> Ast.uncomma)
              (def.f_name, def.f_params +> Ast.unparen +> Ast.uncomma)
                    TODO: too many FP for now
                       if !Flag.show_analyze_error
                       then pr2_once (spf "Could not find constructor for: %s"
                                         (Ast.name name));

      | New (tok, ((IdSelf _ | IdParent _)), args) ->
          failwith "check_classes_php: call unsugar_self_parent()"

            *)
            ()
          | _ -> raise Impossible
          );
          k x

      | New (_tok, (_ ), _args) ->
          (* can't do much *)
          k x


      | Call (ObjGet(lval, _tok, Id name), args) ->
          (* if one calls a method via $this, then it's quite easy to check
           * the arity (eletuchy's idea?).
           * Being complete and handling any method calls like $o->foo()
           * requires to know what is the type of $o which is quite
           * complicated ... so let's skip that for now.
           *
           * todo: special case also id(new ...)-> ?
           *)
          (match lval, !in_class with
          | This _, Some (aclass, is_abstract) ->
            let amethod = Ast.str_of_name name in
            check_method_call (MethodCall is_abstract)
              (aclass, amethod) (name, args) find_entity

          (* wtf? use of $this outside class ??? *)
          | _, None -> ()
          (* todo: need dataflow ... *)
          | _, _ -> ()
          );
          vx (Expr lval);
          vx (Arguments (Ast.unparen args));


      | Call (ClassGet(qu, _tok, Id name), args) ->
          (match qu with
          | Id (classname) ->
            (match classname with
            | XName [QI classname] ->
              let aclass = Ast.str_of_ident classname in
              let amethod = Ast.str_of_name name in
              check_method_call StaticCall
                (aclass, amethod) (name, args) find_entity
            | XName qu -> raise (TodoNamespace (Ast.info_of_qualified_ident qu))

            | (Self _ | Parent _) ->
              if !in_trait
              (* checking for right method name should be done at use time, it
               * can't be done here, so let's accept any method call here.
               *)
              then ()
              else failwith "check_classes_php: call unsugar_self_parent()"
            (* not much we can do? *)
            | LateStatic _ -> ()
            )
          | _ -> ()
          );
          vx (Arguments (Ast.unparen args));



      | ClassGet (Id _classname, _tok, Id (XName [QI (Name("class", _))])) ->
        ()

      | ClassGet (Id classname, tok, Id name) ->
          check_class_constant (Ast.str_of_name classname, Ast.str_of_name name) tok
            find_entity

      | ClassGet (Id classname, tok, IdVar (dname, _scope)) ->
          check_member_access StaticAccess
            (Ast.str_of_name classname, Ast.str_of_dname dname) tok find_entity



      | ObjGet (lval, tok, Id name) ->
          let field = Ast.str_of_name name in
          (match lval, !in_class with
          | This _, Some (aclass, _is_abstract) ->
              check_member_access ObjAccess (aclass, field) tok find_entity
          (* todo: need dataflow ... *)
          | _, _ -> ()
          )


      | _ -> k x
    );
  } in
  visitor (Program prog)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let check_program a b =
  Common.profile_code "Checker.classes" (fun () -> visit_and_check a b)

let check_required_field graph file =
  let current_class = ref (None: string option) in
  let ast = Parse_php.parse_program file in

  let visitor = V.mk_visitor { Visitor_php.default_visitor with
    V.kclass_def = (fun (k, _) def ->
      let class_str = Ast.str_of_ident def.c_name in
      Common.save_excursion current_class
        (Some class_str)
        (fun () -> k def)
    );

    V.kfunc_def = (fun (k, _) def ->
      let f_body = def.f_body in
      (* (tok * error) list *)
      let acc = visit_f_body f_body !current_class graph in
      acc |> List.iter (fun (tok, error) ->
        E.warning tok error
      );
      k def
    )    
  } in
  visitor (Program ast)

