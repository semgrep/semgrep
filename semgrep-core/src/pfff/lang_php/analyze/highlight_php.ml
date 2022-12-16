(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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
module T = Parser_php

open Entity_code open Highlight_code
module E = Entity_code
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Visitor to help write an emacs-like PHP font lock mode. It actually
 * offers many  additional coloring and categories compared to font-lock-mode.
 * It offers also now also some semantic coloring using global information
 * computed by codegraph and pfff_db!
 *)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)

(* todo: should do that generically via the light db.
 * look if def in same file of current file
*)
(*
let place_ids current_file ids db =
  match ids with
  | [] -> NoInfoPlace
  | [x] ->
      let other_file = (* DbPHP.filename_of_id x db *) "TODO" in
      if other_file = current_file
      then PlaceLocal
      else
        if Common2.dirname current_file = Common2.dirname other_file
        then PlaceSameDir
        else PlaceExternal
  | x::y::xs ->
      let other_files =
        List.map (fun id -> "TODO" (* DbPHP.filename_of_id id db *)) ids
        +> Common2.uniq
      in
      if List.mem current_file other_files
      then PlaceLocal
      else
        if other_files +> List.exists (fun other_file ->
          Common2.dirname current_file = Common2.dirname other_file
        )
        then PlaceSameDir
        else PlaceExternal
*)

(* obsolete: this is now computed generically in pfff_visual via the light_db
 * in rewrite_categ_using_entities using x.e_number_external_users.
*)
(*
let arity_of_number nbuses =
  match nbuses with
  | 0             -> NoUse
  | 1             -> UniqueUse
  | n when n < 50 -> MultiUse
  | _             -> LotsOfUse
*)

(*
let use_arity_ident_function_or_macro s db =
  let ids = (* DbPHP.function_ids__of_string s db *) [] in
  let nbuses =
    ids +> List.map (fun id ->
      try
        let callers = (* db.DbPHP.uses.DbPHP.callers_of_f#assoc id in*) [] in
        List.length callers
      with Not_found -> 0
    )
    +> Common2.sum_int
  in
  arity_of_number nbuses
*)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
*)
let _fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

let _highlight_funcall_simple ~tag ~hentities f _args info =
(*
  if Hashtbl.mem Env_php.hdynamic_call_wrappers f
  then begin
    match args with
    | [] -> failwith "dynamic call wrappers should have arguments"
    | x::_xs ->
        (* alternative: could also have a FunCallVarBuiltins
         * but any wrappers around call_user_func should also
         * be considered as dangerous. Better to use
         * the ContainDynamicCall of database_code then.
         *)
        let ii = Lib_parsing_php.ii_of_any (Argument x) in
        ii |> List.iter (fun info -> tag info PointerCall);
  end;
*)
  (match () with
   (* security: *)
(*
  | _ when Hashtbl.mem Env_php.hbad_functions f ->
      tag info BadSmell
*)
   | _ ->
       (match Hashtbl.find_all hentities f with
        | [e] ->
            let ps = e.Db.e_properties in
            (* dynamic call *)
            (if List.mem E.ContainDynamicCall ps
            (* todo: should try to find instead which arguments
             * is called dynamically using dataflow analysis
            *)
             then tag info PointerCall
             else tag info (Entity (Function, (Use2 fake_no_use2)))
            );

            (* args by ref *)
(*
          ps |> List.iter (function
          | E.TakeArgNByRef i ->
              (try
                let a = List.nth args i in
                let ii = Lib_parsing_php.ii_of_any (Argument a) in
                ii |> List.iter (fun info -> tag info CallByRef)
              with _exn ->
                pr2_once ("highlight_php: pb with TakeArgNByRef for " ^ f);
              )
          | E.ContainDynamicCall -> ()
          | _ -> raise Todo
          );
*)
            ()
        | _x::_y::_xs ->
            pr2_once ("highlight_php: multiple entities for: " ^ f);
            (* todo: place of id *)
            tag info (Entity (Function, (Use2 fake_no_use2)));
        | [] ->
            (* todo: place of id *)
            tag info (Entity (Function, (Use2 fake_no_use2)));
       );
  );
  ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* strings are more than just strings in PHP (and webapps in general) *)
let tag_string ~tag s ii =
  match s with
  | s when s =~ "/.*"       -> tag ii EmbededUrl
  | s when s =~ "[a-z]+://" -> tag ii EmbededUrl
  (* security: html in strings is BAD ! *)
  | s when s =~ "<" -> tag ii BadSmell
  | _ -> tag ii String

let tag_name ~tag name =
  match name with
  | XName qu ->
      let info = Ast.info_of_qualified_ident qu in
      tag info (Entity (Class, (Use2 fake_no_use2)));
      (* will be highlighted by the 'toks phase 2' *)
  | Self _tok | Parent _tok -> ()
  | LateStatic tok ->
      tag tok BadSmell

let _tag_class_name_reference ~tag qualif =
  match qualif with
  | Id name -> tag_name ~tag name
  | _ -> ()
(*
    let ii = Lib_parsing_php.ii_of_any (Expr qualif) in
    ii |> List.iter (fun info -> tag info PointerCall)
*)

(*****************************************************************************)
(* PHP Code highlighter *)
(*****************************************************************************)

(*
 * design: Can do either a single function that do all, or multiple independent
 * functions that repeatedly visit the same ast and tokens. The
 * former is faster (no forest) but less flexible. If for instance you want
 * to impose an order between the coloring, such as first keywords and
 * then yacfe specific coloring such as "expanded", then you need extra
 * stuff such as priority lists. If you have separate functions for those then
 * the caller can simply choose to call them in the order he wants so that
 * the last color win. This last strategy can easily separate concerns.
 *
 * The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The Ast is better for tagging idents
 * to figure out what kind of ident it is (a function, a class, a constant)
 *
 * history:
 *  - I was using emacs_mode_xxx before but now have inlined the code
 *    and extended it.
 *)
let visit_program ~tag _prefs  _hentities (ast, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    (* with xhp lots of tokens such as 'var' can also be used
     * as attribute name so we must highlight them with their
     * generic keyword category only if there were not already
     * tagged.
     *
     * The same is true for other kinds of tokens.
    *)
    if not (Hashtbl.mem already_tagged ii)
    then begin
      tag ii categ;
      Hashtbl.add already_tagged ii true
    end
  )
  in

  (* -------------------------------------------------------------------- *)
  (* toks phase 1 *)
  (* -------------------------------------------------------------------- *)
  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* a little bit pad specific *)
    |   T.T_COMMENT(ii)
        ::T.TNewline _ii2
        ::T.T_COMMENT(ii3)
        ::T.TNewline _ii4
        ::T.T_COMMENT(ii5)
        ::xs ->
        let s = Parse_info.str_of_info ii in
        let s5 =  Parse_info.str_of_info ii5 in
        (match () with
         | _ when s =~ ".*\\*\\*\\*\\*" && s5 =~ ".*\\*\\*\\*\\*" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection1
         | _ when s =~ ".*------" && s5 =~ ".*------" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection2
         | _ when s =~ ".*####" && s5 =~ ".*####" ->
             tag ii CommentEstet;
             tag ii5 CommentEstet;
             tag ii3 CommentSection0
         | _ ->
             ()
        );
        aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  aux_toks toks;

  (* -------------------------------------------------------------------- *)
  (* ast phase 1 *)
  (* -------------------------------------------------------------------- *)

  (* TODO: use generic AST highlighter

     (* less: some of the logic duplicates what is in check_variables_php.ml
     * where we differentiate the diffent variables uses (parameters, static,
     * global, local, etc). See the pattern for Static, Global, parameter,
     * catch.
     *)
     let hooks = { V.default_visitor with

      (* -------------------------------------------------------------------- *)
      V.ktop = (fun (k, _) top ->
        match top with
        | ConstantDef def ->
          let info = Ast.info_of_ident def.cst_name in
          tag info (Entity (Constant, (Def2 fake_no_def2)));
          k top
        | _ -> k top
      );
      V.kfunc_def = (fun (k, _) def ->
        let info = Ast.info_of_ident def.f_name in
        let kind =
          match def.f_type with
          | FunctionRegular | FunctionLambda ->
            tag def.f_tok Keyword;
            (Entity (Function, (Def2 NoUse)))

          | MethodRegular | MethodAbstract ->
            tag def.f_tok KeywordObject;
     (*
            if Class_php.is_static_method def
            then StaticMethod (Def2 fake_no_def2)
            else
   *)
             Entity (Method, (Def2 fake_no_def2))
        in
        tag info kind;
        k def
      );

      V.kclass_def = (fun (k, _) def ->
        let info = Ast.info_of_ident def.c_name in
        tag info (Entity (Class, (Def2 fake_no_def2)));
        def.c_extends |> Common.do_option (fun (_, name) ->
          let name = name_of_class_name name in
          let info = Ast.info_of_name name in
          tag info (Entity (Class, (Use2 fake_no_use2)));
        );
        def.c_implements |> Common.do_option (fun (_, xs) ->
          xs |> Ast.uncomma |> List.iter (fun name ->
            let name = name_of_class_name name in
            let info = Ast.info_of_name name in
            tag info (Entity (Class, (Use2 fake_no_use2)));
          );
        );
        k def
      );

      (* -------------------------------------------------------------------- *)
      V.kparameter = (fun (k, _) param ->
        let info = Ast.info_of_dname param.p_name in
        (* we highlight parameters passed by ref elsewhere *)
        (if not (Hashtbl.mem already_tagged info)
        then
          if param.p_ref = None
          then tag info (Parameter Def)
          else tag info ParameterRef
        );
        k param
      );

      (* -------------------------------------------------------------------- *)
      V.kclass_stmt = (fun (k, _) x ->
        match x with
        (* done in kfunc_def *)
        | Ast.Method _ -> k x

        | Ast.XhpDecl d ->
          (match d with
          | XhpAttributesDecl _ -> k x
          | XhpChildrenDecl _ -> k x
          | XhpCategoriesDecl (_, decls, _) ->
            decls |> Ast.uncomma |> List.iter (fun (_tag, ii) ->
              tag ii (Entity (Type, (Use2 fake_no_use2)))
            );
          )
        | Ast.ClassConstants (_, _, _, vars, _) ->
          vars |> Ast.uncomma |> List.iter (fun (name, _opt) ->
            let info = Ast.info_of_ident name in
            tag info (Entity (Constant, (Def2 NoUse)));
          );
          k x;
        | Ast.ClassVariables (_modifiers, _opt_ty, vars, _) ->
          vars |> Ast.uncomma |> List.iter (fun (dname, _opt) ->
            let info = Ast.info_of_dname dname in
            tag info (Entity (Field, (Def2 fake_no_def2)));
          );
          k x
        | Ast.UseTrait (_, names, _rules_or_tok) ->
           names |> Ast.uncomma |> List.iter (fun name ->
            let name = name_of_class_name name in
            let info = Ast.info_of_name name in
            tag info (Entity (Class, (Use2 fake_no_use2)));
           );
          k x
        | Ast.TraitConstraint (_, _, _, _)
        | Ast.ClassType _ ->
            k x
      );

      (* -------------------------------------------------------------------- *)
      V.kstmt = (fun (k,_bigf) stmt ->
        k stmt;
        match stmt with
        | Globals (_v1, v2, _v3) ->
          v2 |> Ast.uncomma |> List.iter (fun x ->
            match x  with
            | GlobalVar dname ->
              let info = Ast.info_of_dname dname in
              tag info (Entity (Global, (Def2 NoUse)))

              (* TODO ?? *)
            | GlobalDollar _ -> ()
            | GlobalDollarExpr _ ->  ()
          );
        | StaticVars (_v1, v2, _v3) ->
          v2 |> Ast.uncomma |> List.iter (fun svar ->
            let (dname, _affect_opt) = svar in
            let info = Ast.info_of_dname dname in
            tag info (Local Def);
          );
          ()
        | _ -> ()
      );
      (* -------------------------------------------------------------------- *)
      V.kcatch = (fun (k, _) c ->
        let (_, (_, (cname, dname), _), _stmts) = c in
        let name = name_of_class_name cname in
        let info_class = Ast.info_of_name name in
        tag info_class (Entity (Class, (Use2 fake_no_use2)));

        let info_dname = Ast.info_of_dname dname in
        tag info_dname (Local Use);
        k c
      );

      (* -------------------------------------------------------------------- *)
      V.kexpr = (fun (k,vx) expr ->
        (* do not call k expr; here, let each case call it. Also
         * remember that tag() will not retag something already tagged,
         * so it simplifies a bit the logic where you can visit
         * the children without being scared it will retag things
         * (compared to check_variables_php or graph_code_php).
         *)
        (match expr with
        | This tok ->
          tag tok (Entity (Class, (Use2 fake_no_use2)))

        | ArrayGet (var, exprbracket) ->
          (match Ast.unbracket exprbracket with
          | None ->
            k expr
          | Some exprbis ->
            (match exprbis with
            | Sc (C (Ast.String (_s, info))) ->
              tag info (Entity (Field, (Use2 fake_no_use2)));
              vx (Expr var);

            | Sc (C (Int (_s, _info))) ->
              k expr
            | _ -> k expr
            )
          )

        (* Call *)
        | Call (a, args) ->
          (match a with
          | Id callname ->
            let info = Ast.info_of_name callname in
            let f = Ast.str_of_name callname in
            let args = args |> Ast.unparen |> Ast.uncomma in
            highlight_funcall_simple ~tag ~hentities f args info;
          | ClassGet (_lval, _, Id name) ->
            let info = Ast.info_of_name name in
            tag info (StaticMethod (Use2 fake_no_use2));
          | ClassGet (lval, _, _var) ->
            let ii = Lib_parsing_php.ii_of_any (Expr lval) in
            ii |> List.iter (fun info -> tag info PointerCall);
          | ObjGet(_lval, _tok, Id name) ->
            let info = Ast.info_of_name name in
            tag info (Entity (Method, (Use2 fake_no_use2)));
          | e ->
            (* function pointer call !!! put in big font *)
            let ii = Lib_parsing_php.ii_of_any (Expr e) in
            ii |> List.iter (fun info -> tag info PointerCall);
          );
          k expr

        (* ObjGet *)
        | ObjGet (_lval, _tok, Id name) ->
          let info = Ast.info_of_name name in
          tag info (Entity (Field, (Use2 fake_no_use2)));
          k expr

        (* ClassGet *)
        | ClassGet (qualif, tok, b) ->
          tag_class_name_reference ~tag qualif;
          (match b with
          | Id name ->
              let info = Ast.info_of_name name in
              tag info (Entity (Constant, (Use2 fake_no_use2)))
          | IdVar (dname, _) ->
              let info = Ast.info_of_dname dname in
              (* todo? special category for class variables ? *)
              tag info (Entity (Global, (Use2 fake_no_use2)));
          | _v2 ->
            (* todo? colorize qualif? bad to use dynamic variable ...
               let info = Ast.info_of_dname dname in
               tag info BadSmell
            *)
            tag tok BadSmell;
          );
          k expr
        | New (_, qualif, _) | AssignNew (_, _, _, _, qualif, _)
        | InstanceOf (_, _, qualif)
          ->
          tag_class_name_reference ~tag qualif;
          k expr

        | Id name ->
          (* cf also typing_php.ml *)
          let s = Ast.str_of_name name in
          let info = Ast.info_of_name name in
          (match s with
          | "true" | "false" -> tag info Boolean
          | "null" -> tag info Null
          | _ ->
            if not (Hashtbl.mem already_tagged info)
            then tag info (Entity (Constant, (Use2 fake_no_use2)))
          )

        | IdVar (dname, aref) ->
          (* see check_variables_php.ml *)
          let info = Ast.info_of_dname dname in
          (match !aref with
          | S.Local -> tag info (Local Use)
          | S.Param -> tag info (Parameter Use)
          | S.Class -> tag info (Entity (Field, (Use2 fake_no_use2)))
          (* TODO, need global_used table *)
          | S.Global | S.Closed -> tag info (Entity (Global, (Use2 fake_no_use2)));
          (* less: could invent a Static in highlight_code ? *)
          | S.Static -> tag info (Entity (Global, (Use2 fake_no_use2)))
          | S.ListBinded | S.LocalIterator | S.LocalExn -> tag info (Local Use)
          | S.NoScope -> tag info (NoType)
          )

        | Cast (((_cast, v1), _v2)) ->
          tag v1 (TypeInt);
          k expr;
        | _ ->
          k expr
        )
      );
      (* -------------------------------------------------------------------- *)
      V.kxhp_attribute = (fun (k, _) x ->
        let ((attr_name, _ii_attr_name), _tok_eq, attr_val) = x in
        (match attr_name with
        | "href" | "src" ->
          (match attr_val with
          | XhpAttrString (tok1, xs, tok2) ->
            tag tok1 String;
            tag tok2 String;
            xs |> List.iter (function
            | EncapsString (_s, ii) ->
              tag ii EmbededUrl
            | EncapsExpr (_, _, _) | EncapsDollarCurly (_, _, _)
            | EncapsCurly (_, _, _) | EncapsVar _
              -> ()
            );
          | XhpAttrExpr _e -> ()
          | SgrepXhpAttrValueMvar _ -> ()
          )
        | _ -> ()
        );
        k x
      );

      V.kxhp_attr_decl = (fun (k, _) x ->
        match x with
        | XhpAttrInherit (_xhp_tag, ii) ->
            tag ii (Entity (Class, (Use2 fake_no_use2)));
        | XhpAttrDecl ((_attr_type, (_attr_name, iiname), _affect_opt, _tok_opt))->
            tag iiname (Entity (Field, (Use2 fake_no_use2)));
            k x
      );

      (* -------------------------------------------------------------------- *)
      V.kconstant = (fun (_k, _) e ->
        match e with
        | Int v1 | Double v1 ->
          tag (snd v1) Number

        | Ast.String (s, ii) ->
          (* this can be sometimes tagged as url, or field access in array *)
          if not (Hashtbl.mem already_tagged ii)
          then tag_string ~tag s ii
        | PreProcess v1 -> tag (snd v1) Builtin
        | XdebugClass (_, _) | XdebugResource -> ()
      );

      V.kencaps = (fun (k, _) e ->
        match e with
        | EncapsString (s, ii) ->
          if not (Hashtbl.mem already_tagged ii)
          then tag_string ~tag s ii
        | _ -> k e
      );
      (* -------------------------------------------------------------------- *)
      V.khint_type = (fun (k, _) x ->
        (match x with
        (* TODO: emit info for type args *)
        | Hint (name, _targsTODO) ->
          tag_name ~tag name
        | HintArray _ | HintQuestion _ | HintTuple _
        | HintCallback _
          (* todo: colorize as record the keys? *)
        | HintShape _
        | HintTypeConst _
        | HintVariadic _
          ->
          ()
        );
        k x
      );
     }
     in
     let visitor = V.mk_visitor hooks in
     (try
      visitor (Program ast)
     with Cst_php.TodoNamespace _ -> ()
     );
  *)

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)
  toks |> List.iter (fun tok ->
    match tok with
    | T.TNewline _ii | T.TSpaces _ii | T.EOF _ii -> ()
    (* less: could highlight certain words in the comment? *)
    | T.T_COMMENT (ii)  | T.T_DOC_COMMENT ii  -> tag ii Comment
    | T.TCommentPP _ii -> ()
    | T.TUnknown ii -> tag ii Error

    (* all the name and varname should have been tagged by now. *)
    | T.T_IDENT (_, ii) | T.T_METAVAR (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Error
    (* they should have been covered before *)
    | T.T_VARIABLE (_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Error

    | T.TOPAR ii   | T.TCPAR ii
    | T.T_LAMBDA_OPAR ii | T.T_LAMBDA_CPAR ii
    | T.TOBRACE ii | T.TCBRACE ii
    | T.TOBRA ii   | T.TCBRA ii
    | T.TOATTR ii
      ->tag ii Punctuation

    | T.T_ELLIPSIS ii
    | T.LDots ii | T.RDots ii
      -> tag ii Punctuation
    | T.TANTISLASH ii -> tag ii KeywordModule
    | T.T_NAMESPACE ii -> tag ii Keyword

    | T.TGUIL ii -> tag ii String

    | T.TDOLLAR ii -> tag ii Punctuation
    | T.TDOLLARDOLLAR ii -> tag ii Punctuation
    | T.TSEMICOLON ii -> tag ii Punctuation
    | T.TBACKQUOTE ii -> tag ii Punctuation

    (* we want to highlight code using eval! *)
    | T.T_EVAL ii -> tag ii BadSmell

    | T.T_OPEN_TAG ii ->
        tag ii Keyword

    | T.T_REQUIRE_ONCE ii | T.T_REQUIRE ii
    | T.T_INCLUDE_ONCE ii | T.T_INCLUDE ii
      -> tag ii Include

    | T.T_NEW ii | T.T_CLONE ii -> tag ii KeywordObject
    | T.T_INSTANCEOF ii -> tag ii KeywordObject

    | T.T__AT ii -> tag ii Builtin

    | T.T_IS_NOT_EQUAL ii   | T.T_IS_EQUAL ii
    | T.T_IS_NOT_IDENTICAL ii  | T.T_IS_IDENTICAL ii
    | T.T_ROCKET ii
      -> tag ii Operator

    (* done in Cast *)
    | T.T_UNSET_CAST _ii   | T.T_OBJECT_CAST _ii
    | T.T_ARRAY_CAST _ii   | T.T_STRING_CAST _ii
    | T.T_DOUBLE_CAST _ii   | T.T_INT_CAST _ii
    | T.T_BOOL_CAST _ii
      -> ()

    | T.T_IS_GREATER_OR_EQUAL ii  | T.T_IS_SMALLER_OR_EQUAL ii
    | T.T_SR ii   | T.T_SL ii
    | T.T_LOGICAL_XOR ii  | T.T_LOGICAL_AND ii
    | T.T_LOGICAL_OR ii   | T.T_BOOLEAN_AND ii
    | T.T_BOOLEAN_OR ii
    | T.T_BOOLEAN_PIPE ii
    | T.T_DEC ii  | T.T_INC ii
    | T.T_SR_EQUAL ii   | T.T_SL_EQUAL ii
    | T.T_XOR_EQUAL ii  | T.T_OR_EQUAL ii  | T.T_AND_EQUAL ii
    | T.T_MOD_EQUAL ii
    | T.T_CONCAT_EQUAL ii
    | T.T_DIV_EQUAL ii  | T.T_MUL_EQUAL ii
    | T.T_MINUS_EQUAL ii  | T.T_PLUS_EQUAL ii
    | T.TGREATER ii   | T.TSMALLER ii
    | T.TEQ ii | T.TXOR ii | T.TOR ii | T.TAND ii
    | T.TMOD ii | T.TDIV ii | T.TMUL ii | T.TMINUS ii | T.TPLUS ii | T.TPOW ii
      -> tag ii Operator

    | T.TQUESTION ii  | T.TTILDE ii  | T.TBANG ii  | T.TDOT ii
    | T.TCOMMA ii  | T.TCOLON ii
    | T.TCOLCOL ii
      -> tag ii Punctuation

    | T.T_CURLY_OPEN ii -> tag ii Punctuation
    | T.T_DOLLAR_OPEN_CURLY_BRACES ii -> tag ii Punctuation

    | T.T_END_HEREDOC ii | T.T_START_HEREDOC ii -> tag ii Punctuation
    | T.T_CLOSE_TAG_OF_ECHO ii | T.T_OPEN_TAG_WITH_ECHO ii -> tag ii Punctuation
    | T.T_CLOSE_TAG ii -> tag ii Punctuation

    (* done in PreProcess *)
    | T.T_FILE _ii  | T.T_LINE _ii | T.T_DIR _ii
    | T.T_FUNC_C _ii | T.T_METHOD_C _ii | T.T_CLASS_C _ii | T.T_TRAIT_C _ii
    | T.T_NAMESPACE_C _ii
      -> ()

    (* can be a type hint *)
    | T.T_ARRAY ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Builtin
    | T.T_LIST ii -> tag ii Builtin

    | T.T_ARROW ii ->  tag ii Punctuation
    | T.T_OBJECT_OPERATOR ii -> tag ii Punctuation
    | T.T_DOUBLE_ARROW ii -> tag ii Punctuation

    | T.T_CLASS ii | T.T_TRAIT ii | T.T_ENUM ii -> tag ii KeywordObject

    | T.T_IMPLEMENTS ii | T.T_EXTENDS ii | T.T_INTERFACE ii ->
        tag ii KeywordObject

    | T.T_INSTEADOF ii -> tag ii KeywordObject

    | T.T_TYPE ii -> tag ii Keyword

    | T.T_EMPTY ii  | T.T_ISSET ii | T.T_UNSET ii -> tag ii Builtin

    | T.T_VAR ii -> tag ii Keyword
    | T.T_PUBLIC ii | T.T_PROTECTED ii | T.T_PRIVATE ii -> tag ii Keyword
    | T.T_FINAL ii | T.T_ABSTRACT ii -> tag ii KeywordObject

    | T.T_STATIC ii -> tag ii Keyword
    | T.T_CONST ii -> tag ii Keyword

    | T.T_SELF ii | T.T_PARENT ii ->
        tag ii (Entity (Class, (Use2 fake_no_use2)));

        (* could be for func or method or lambda so tagged via ast *)
    | T.T_FUNCTION ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Keyword

    | T.T_AS ii -> tag ii Keyword
    | T.T_SUPER ii -> tag ii Keyword
    | T.T_GLOBAL ii -> tag ii Keyword
    | T.T_USE ii -> tag ii Keyword
    | T.T_ENDDECLARE ii -> tag ii Keyword
    | T.T_DECLARE ii -> tag ii Keyword
    | T.T_EXIT ii -> tag ii Keyword

    | T.T_THROW ii | T.T_CATCH ii | T.T_FINALLY ii
    | T.T_TRY ii -> tag ii KeywordExn
    | T.T_RETURN ii | T.T_CONTINUE ii | T.T_BREAK ii | T.T_GOTO ii -> tag ii Keyword
    | T.T_DEFAULT ii | T.T_CASE ii -> tag ii Keyword
    | T.T_ENDSWITCH ii | T.T_SWITCH ii -> tag ii KeywordConditional

    | T.T_ENDFOREACH ii | T.T_FOREACH ii
    | T.T_ENDFOR ii | T.T_FOR ii
    | T.T_ENDWHILE ii | T.T_WHILE ii
    | T.T_DO ii
      -> tag ii KeywordLoop

    | T.T_IF ii | T.T_ELSEIF ii  | T.T_ELSE ii | T.T_ENDIF ii
      -> tag ii KeywordConditional

    | T.T_PRINT ii | T.T_ECHO ii -> tag ii Builtin


    | T.T_YIELD ii  | T.T_FROM ii
    | T.T_AWAIT ii | T.T_ASYNC ii -> tag ii Keyword

    (* should have been handled in field *)
    | T.T_STRING_VARNAME _ii -> ()

    | T.T_INLINE_HTML (_, ii) -> tag ii EmbededHtml
    | T.T_NUM_STRING _ii -> ()

    | T.T_ENCAPSED_AND_WHITESPACE (s, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag_string ~tag s ii

    | T.T_CONSTANT_ENCAPSED_STRING (s, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag_string ~tag s ii

    (* should been handled in Constant *)
    | T.T_BOOL _ | T.T_DNUMBER _ | T.T_LNUMBER _ -> ()
  );

  (* -------------------------------------------------------------------- *)
  (* ast phase 2 *)
  (* -------------------------------------------------------------------- *)
  (match ast with
   | NotParsedCorrectly iis::_ ->
       iis |> List.iter (fun ii -> tag ii NotParsed)
   | _ -> ()
  );
  ()
