(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2011 Facebook
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

open Cst_php

module Ast = Cst_php
module V = Visitor_php
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * There are many places where we need to get access to the list of
 * entities defined in a file or used in a file (e.g. for tags,
 * in cmf -deadcode to analyze the defs in a diff, etc)
 * 
 * This file is concerned with entities, that is Ast_php.ident for defs
 * and Ast_php.name for uses.
 * For completness C-s for name in ast_php.ml and see if all uses of 
 * it are covered. Other files are more concerned about checks related 
 * to variables, that is Ast_php.dname, as in check_variables_php.ml
 * 
 * todo: factorize code in
 *  - lib_parsing_php.ml many get_xxx_any ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the name option is a little bit ugly because it's valid only for
 * nested entities like Method. One could make a specific
 * entity_kind for PHP but this will go against the multi-languages
 * factorization we try to do in h_program-lang/
 *)

type def = 
  Cst_php.ident * Cst_php.ident option * Entity_code.entity_kind

type use = 
  Cst_php.name * Entity_code.entity_kind

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
(* 
 * history: was previously duplicated in 
 *  - tags_php.ml
 *  - TODO check_module.ml and defs_module.ml
 *  - TODO database_php_build.ml ?
 *)
let defs_of_any any =
  let current_class = ref (None: Cst_php.ident option) in

  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kfunc_def = (fun (k, _) def ->
      (match def.f_type with
      | FunctionRegular -> 
          Common.push (def.f_name, None, E.Function) aref
      | MethodRegular | MethodAbstract ->
          let classname =
            match !current_class with
            | Some c -> c
            | None -> failwith "impossible: no current_class in defs_use_php.ml"
          in
          (* less:
           * let kind =
           * if Class_php.is_static_method def
           * then E.StaticMethod
           * else E.RegularMethod
           *)
          Common.push (def.f_name, Some classname, E.Method) aref
      | FunctionLambda ->
          (* the f_name is meaningless *)
          ()
      );
      (* could decide to not recurse, but could have nested function ?
       * hmm they are usually under some toplevel ifs, not inside functions.
       *)
      k def
    );
    V.kclass_def = (fun (k, _) def ->
      (* less: use class_php.class_kind_of_ctype
      let kind = 
        match def.c_type with
        | ClassRegular _ | ClassFinal _ | ClassAbstract _ -> E.RegularClass
        | Interface _ -> E.Interface
        | Trait _ -> E.Trait
        in
      *)
      Common.push (def.c_name, None, E.Class) aref;
      Common.save_excursion current_class (Some def.c_name) (fun () ->
          k def;
      );
    );
    V.ktop = (fun (k, _) x ->
      match x with
      (* const of php 5.3 *)
      | ConstantDef def ->
          Common.push (def.cst_name, None, E.Constant) aref;
          k x
      | TypeDef def ->
          Common.push (def.t_name, None, E.Type) aref;
          k x
      | _ -> k x
    );

    V.kexpr = (fun (k, _) x ->
      match x with
      | Call(Id(XName[QI (Name ("define", _tok))]), args) ->
          let args = args |> Ast.unparen |> Ast.uncomma in
          (match args with
          (* Maybe better to have a Define directly in the AST. Note that
           * PHP 5.3 has a new const features that makes the use of define
           * obsolete.
           *)
          | (Arg ((Sc (C (String (s,info))))))::_xs -> 
              (* by default the info contains the '' or "" around the string,
               * which is not the case for s. See ast_php.ml
               *)
              let info' = Parse_info.rewrap_str (s) info in
              Common.push ((Name (s, info')), None, E.Constant) aref;
              k x
          | _ -> k x
          )
      | _ -> k x
    );
    (* less: globals? fields? class constants? *)
  }) any

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)
(* 
 * Cover every cases ? C-s for 'name' in ast_php.ml.
 * update: C-s for 'xhp_tag' too.
 * See unit_foundation_php.ml for the unit tests.
 *
 * history: was previously duplicated in 
 *  - class_php.ml, 
 *  - TODO check_module.ml and uses_module.ml
 * 
 * todo: 
 * - constants too! 
 * - methods, fields, class constants, see Database_code.entity_kind
 * - check_module.ml and the places where we call checkClassName,
 *   same than here ?
 * - less: right now I don't make a difference between Class and Interface.
 *   If want then just intercept in the clas_def hook the processing
 *   of 'implements'.
 * - less: do stuff for dynamic stuff like ClassNameRefDynamic ?
 *   return a special Tag ? DynamicStuff ? So at least know they
 *   are connections to more entities than one can infer statically.
 *)
let uses_of_any ?(verbose=false) any = 
  ignore(verbose);

  V.do_visit_with_ref (fun aref -> { V.default_visitor with

    V.kexpr = (fun (k, _) x ->
      (match x with
      (* todo: what about functions passed as strings? *)
      | Call (Id name, _args) ->
          Common.push (name, E.Function) aref;

    (* This covers
     * - new X, instanceof X 
     *   (via class_name_reference)
     * - X::Cst, X::$var, X::method(), X::$f() 
     *   (via qualifier)
     *)
      | New (_, Id name, _) 
      | InstanceOf (_, _, Id name)
      | AssignNew(_, _, _, _, Id name, _)
      | ClassGet (Id name, _, _) ->
          Common.push (name, E.Class) aref;
      | _ -> ()
      );
      k x
    );
    (* This covers:
     * - extends X, implements X, catch(X)
     * - function foo(X $f)
     *   (via class_name_or_kwd)
     *)
    V.khint_type = (fun (k, _) classname ->
      (* todo? can interface define constant ? in which case
       * there is some ambiguity when seeing X::cst ...
       * could be the use of a Class or Interface.
       * So right now I just merge Class and Interface
       *)
      (match classname with
      | Hint (classname, _targsTODO) ->
          Common.push (classname, E.Class) aref;
      | _ -> ()
      );
      k classname
    );

    (* xhp: there is also implicitely a new when we use a XHP tag *)
    V.kxhp_html = (fun (k, _) x ->
      match x with
      | Xhp (xhp_tag, _attrs, _tok, _body, _end) ->
          Common.push (XName[QI(XhpName xhp_tag)], E.Class) aref;
          k x
      | XhpSingleton (xhp_tag, _attrs, _tok) ->
          Common.push (XName[QI(XhpName xhp_tag)], E.Class) aref;
          k x
      (* todo: do it also for xhp attributes ? kxhp_tag then ?
       * (but take care to not include doublon because have
       * a xhp_tag for the open tag and one for the close.
       *)
    );
  }) any
