(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2014 Facebook
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

open Ast_cpp
open Entity_code
open Highlight_code

module S = Scope_code
module PI = Parse_info
module Ast = Ast_cpp
module V = Visitor_cpp
module Lib = Lib_parsing_cpp
module T = Parser_cpp
module TH = Token_helpers_cpp
module Type = Type_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * TODO:
 *  - take into account class qualifier as a use
 *  - take inheritance as a class use
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let (==~) = Common2.(==~)

(*****************************************************************************)
(* Helpers when have global analysis information *)
(*****************************************************************************)
let h_debug_functions = Common.hashset_of_list [
  "DEBUG"
]

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_toplevel ~tag_hook _prefs (*db_opt *) (ast, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.add already_tagged ii true
  )
  in

  (* -------------------------------------------------------------------- *)
  (* Toks phase 1 *)
  (* -------------------------------------------------------------------- *)

  let rec aux_toks xs =
    match xs with
    | [] -> ()

    (* a little bit pad specific *)
    |   T.TComment(ii)
        ::T.TCommentNewline _ii2
        ::T.TComment(ii3)
        ::T.TCommentNewline _ii4
        ::T.TComment(ii5)
        ::xs ->
        let s = PI.str_of_info ii in
        let s5 =  PI.str_of_info ii5 in
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
        aux_toks xs;
        let s = PI.str_of_info ii in
        let s2 = PI.str_of_info ii3 in
        (match () with
         | _ when s =~ "//////////.*"
               && s2 =~ "// .*"
           ->
             tag ii3 CommentSection1
         | _ ->
             ()
        );
        aux_toks xs

    |   T.TComment(ii)::xs when (PI.str_of_info ii) =~ "//IMPORTANT:" ->
        tag ii CommentSection2;
        aux_toks xs

    (* heuristic for class/struct definitions.
     *
     * Must be before the heuristic for function definitions
     * otherwise this pattern will never be exercised
     *
     * the code below has been commented because it generates
     * too much false positives. For instance in 'struct foo * bar(...) '
     * foo would be classified as the start of a struct definition
     *
     * don't want forward class declarations to generate noise
     * | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum(ii))
     * ::T.TCommentSpace ii2::T.TIdent(s, ii3)::T.TPtVirg _::xs ->
     * aux_toks xs
     * | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum (ii)
     * | T.TIdent ("service", ii)
     * )
     * ::T.TCommentSpace ii2::T.TIdent(s, ii3)::xs
     * when Ast.col_of_info ii = 0 ->
     *
     * tag ii3 (Class (Def2 fake_no_def2));
     * aux_toks xs;
    *)

    | (T.Tclass(ii) | T.Tstruct(ii) | T.Tenum ii
      (* thrift stuff *)
      | T.TIdent ("service", ii)
      )
      ::T.TCommentSpace _ii2
      ::T.TIdent(_s, ii3)
      ::T.TCommentSpace _ii4
      ::T.TOBrace _ii5
      ::xs
      when PI.col_of_info ii = 0 ->

        tag ii3 (Entity (Class, (Def2 fake_no_def2)));
        aux_toks xs;


        (* heuristic for function definitions *)
    | t1::xs when (t1 |> TH.info_of_tok |> PI.col_of_info = 0) &&
                  TH.is_not_comment t1 ->
        let line_t1 = TH.line_of_tok t1 in
        let rec find_ident_paren xs =
          match xs with
          | T.TIdent(_s, ii1)::T.TOPar _::_ ->
              tag ii1 (Entity (Function, (Def2 NoUse)));
          | T.TIdent(_s, ii1)::T.TCommentSpace _::T.TOPar _::_ ->
              tag ii1 (Entity (Function, (Def2 NoUse)));
          | _::xs ->
              find_ident_paren xs
          | [] -> ()
        in
        let same_line = (t1::xs) |> Common2.take_while (fun t ->
          TH.line_of_tok t = line_t1)
        in
        find_ident_paren same_line;
        aux_toks xs

    | _x::xs ->
        aux_toks xs
  in
  aux_toks toks;

  let is_at_toplevel = ref true in

  (* -------------------------------------------------------------------- *)
  (* Ast phase 1 *)
  (* -------------------------------------------------------------------- *)

  let visitor = V.mk_visitor { (*V.default_visitor with *)

    V.kinfo =  (fun (_k, _) _x -> () );

    V.kcompound =  (fun (k, _) x ->
      Common.save_excursion is_at_toplevel false (fun () ->
        k x
      )
    );

    V.kdeclaration = (fun (k, _) x ->
      match x with
      | DeclList (xs, _) ->
          xs |> List.iter (fun onedecl ->
            (match onedecl with
             | EmptyDecl _ | TypedefDecl _ -> () (* TODO? *)
             | StructuredBinding _ | BitField _ -> ()
             | V ({ name; specs}, {v_type; _ }) ->
                 let categ =
                   match specs with
                   (* TODO | StoTypedef _ -> Entity (Type, Def2 fake_no_def2) *)
                   | _ when Type.is_function_type v_type ->
                       FunctionDecl NoUse
                   (* could be a global too when the decl is at the top *)
                   | [ST (Extern, _)] -> Entity (Global, (Def2 fake_no_def2))
                   | _ when !is_at_toplevel -> Entity (Global, (Def2 fake_no_def2))
                   | _ -> Local Def
                 in
                 Ast.ii_of_id_name name |>List.iter (fun ii -> tag ii categ)
            );
          );
          k x

      | NotParsedCorrectly ii ->
          ii |> List.iter (fun ii -> tag ii NotParsed)

      | _ -> k x
    );

    V.kstmt = (fun (k, _) x ->
      match x with
      | (Ast.Label ((_, ii1), ii2, _st)) ->
          [ii1;ii2] |> List.iter (fun ii -> tag ii KeywordExn);
          k x
      | Jump (Goto (_, (_s, lblii)), _) ->
          tag lblii KeywordExn;
          k x
      | _ -> k x
    );

    V.kexpr = (fun (k, _) x ->
      match x with
      | N (name, idinfo) ->
          (match name with
           | (_, _, IdIdent (s, ii)) ->
               (* the Call case might have already tagged it with something *)
               if not (Hashtbl.mem already_tagged ii)
               then
                 if s ==~ Parsing_hacks_lib.regexp_macro
                 then tag ii (Entity (Constant, (Use2 fake_no_use2)))
                 else
                   (match idinfo.Ast.i_scope with
                    | S.NoScope -> ()
                    | S.Local -> tag ii (Local Use)
                    | S.Param -> tag ii (Parameter Use)
                    | S.Global -> tag ii (Entity (Global, (Use2 fake_no_use2)));
                        (* todo? could invent a Static in highlight_code ? *)
                    | S.Static -> tag ii (Entity (Global, (Use2 fake_no_use2)));
                        (* TODO *)
                    | S.Class -> ()
                    (* todo? valid only for PHP? *)
                    | (S.ListBinded|S.LocalIterator|S.LocalExn|S.Closed)
                      -> failwith "scope not handled"
                   )
           | _ -> ()
          )

      | Call (e, _args) ->
          (match e with
           | N (name, scope) ->
               (match name with
                | _, _, IdIdent (s, ii) ->
                    if Hashtbl.mem h_debug_functions s
                    then tag ii BuiltinCommentColor
                    else
                      (match scope.i_scope with
                       | S.Local | S.Param ->
                           tag ii PointerCall
                       | _ ->
                           tag ii (Entity (Function, (Use2 fake_no_use2)))
                      )
                | _ -> ()
               );

           | DotAccess (_e, _, name) ->
               Ast.ii_of_id_name name |> List.iter (fun ii ->
                 let file = PI.file_of_info ii in
                 if File_type.file_type_of_file file =*=
                    File_type.PL (File_type.C "c")
                 then tag ii PointerCall
                 else tag ii (Entity (Method, (Use2 fake_no_use2)))
               )
           | _ ->
               (* dynamic stuff, should highlight! *)
               let ii = Lib.ii_of_any (Expr e) in
               ii |> List.iter (fun ii -> tag ii PointerCall);
          );
          k x

      | DotAccess (_e, _, name) ->
          (match name with
           | _, _, IdIdent (_s, ii) ->
               if not (Hashtbl.mem already_tagged ii)
               then tag ii (Entity (Field, (Use2 fake_no_use2)));
           | _ -> ()
          );
          k x

      | New (_colon, _tok, _placement, ft, _args) ->
          (match ft with
           | _nq, ((TypeName name)) ->
               Ast.ii_of_id_name name |> List.iter (fun ii ->
                 tag ii (Entity (Class, (Use2 fake_no_use2)));
               )
           | _ -> ()
          );
          k x
      | _ -> k x
    );
    V.kinit = (fun (k, _) x ->
      match x with
      | InitDesignators (xs, _, _init) ->
          xs |> List.iter (function
            | DesignatorField (_tok, (_s, tok2)) ->
                tag tok2 (Entity (Field, (Use2 fake_no_use2)))
            | _ -> ()
          );
          k x
      | _ -> k x
    );

    V.kparameter = (fun (k, _) x ->
      (match x with
       | P {p_name = Some (_s, ii); _} -> tag ii (Parameter Def)
       | _ -> ()
      );
      k x
    );

    V.ktypeC = (fun (k, _) x ->
      match x with
      | TypeName name ->
          Ast.ii_of_id_name name |> List.iter (fun ii ->
            (* new Xxx and other places have priority *)
            if not (Hashtbl.mem already_tagged ii)
            then tag ii (Entity (Type, Use2 fake_no_use2))
          );
          k x

      | Ast_cpp.EnumName (_tok, name) ->
          Ast.ii_of_id_name name |> List.iter (fun ii ->
            tag ii (Entity (Type, Use2 fake_no_use2))
          )

      | ClassName (_su, _n_was_id_before) ->
          (*tag ii (StructName Use); TODO *)
          k x

      | TypenameKwd (_tok, _t_was_name) ->
          (*Ast.ii_of_id_name name |> List.iter (fun ii ->
            tag ii (Entity (Type, Use2 fake_no_use2))); *)
          k x

      | EnumDef ({enum_body = xs; _}) ->
          xs |> unparen |> List.iter (fun enum_elem ->
            let (_, ii) = enum_elem.e_name in
            tag ii (Entity (Constructor,(Def2 fake_no_def2)))
          );
          k x

      | _ -> k x
    );

    V.kclass_def = (fun (k,_) ((c_nameopt, _def) as x) ->
      c_nameopt |> Option.iter (fun name ->
        Ast.ii_of_id_name name |> List.iter (fun ii ->
          tag ii (Entity (Class, (Def2 fake_no_def2)));
        )
      );
      k x
    );
    V.kfunc_def = (fun (k,_) (({name; specs = _}, _def) as x) ->
      Ast.ii_of_id_name name |> List.iter (fun ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii (Entity (Class, (Def2 fake_no_def2)));
      );
      k x
    );
    V.kclass_member = (fun (k,_) def ->
      (match def with
       | F (Func x) ->
           let ({name = f_name; specs = _}, _def) = x in
           let name = f_name in
           Ast.ii_of_id_name name |> List.iter (fun ii ->
             tag ii (Entity (Method, (Def2 fake_no_def2)))
           );
       | F (DeclList (xs, _)) ->
           xs |> List.iter (fun onedecl ->
             (match onedecl with
              | EmptyDecl _ | TypedefDecl _ -> () (* TODO? *)
              | StructuredBinding _ -> (* TODO *) ()
              | V ({name; _}, { v_type; _}) ->
                  let kind =
                    (* poor's man object using function pointer; classic C idiom *)
                    if Type.is_method_type v_type
                    then Entity (Method, (Def2 fake_no_def2))
                    else Entity (Field, (Def2 NoUse))
                  in
                  Ast.ii_of_id_name name |> List.iter (fun ii -> tag ii kind)
              | BitField (sopt, _tok, _ft, _e) ->
                  (match sopt with
                   | Some (_s, iiname) ->
                       tag iiname (Entity (Field, (Def2 NoUse)))
                   | None -> ()
                  )
             ));
       | _ -> ()
      );
      k def
    );
    V.kcpp = (fun (k,_) def ->
      (match def with
       | Ast.Define (_, _id, DefineMacro params, _body) ->
           params |> Ast.unparen |> List.iter (fun (_s, ii) ->
             tag ii (Parameter Def)
           )
       | _ -> ()
      );
      k def
    );
    V.ktoplevel = (fun (k,_) def ->
      k def
    );
  }
  in
  visitor (Program ast);

  (* -------------------------------------------------------------------- *)
  (* toks phase 2 *)
  (* -------------------------------------------------------------------- *)
  toks |> List.iter (fun tok ->
    match tok with

    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then
          (* a little bit syncweb specific *)
          let s = PI.str_of_info ii in
          (match s with
           (* yep, s e x are the syncweb markers *)
           | _ when s =~ "/\\*[sex]:"  -> tag ii CommentSyncweb
           | _ -> tag ii Comment
          )

    | T.TInt (_,ii) | T.TFloat (_,ii) ->
        tag ii Number
    | T.TString (_s,ii) | T.TChar (_s,ii) ->
        tag ii String
    | T.Tfalse ii | T.Ttrue ii  ->
        tag ii Boolean
    | T.Tnullptr ii | T.Tnull ii ->
        tag ii Null

    | T.TPtVirg ii

    | T.TOPar ii | T.TOPar_CplusplusInit ii | T.TOPar_Define ii | T.TCPar ii
    | T.TOBrace ii | T.TOBrace_DefineInit ii | T.TCBrace ii
    | T.TOCro ii | T.TOCro_Lambda ii | T.TCCro ii

    | T.TDot ii | T.TComma ii | T.TPtrOp ii
    | T.TAssign ((_, ii))
    | T.TEq ii
    | T.TWhy ii | T.TTilde ii | T.TBang ii
    | T.TEllipsis ii | T.LDots ii | T.RDots ii
    | T.TCol ii ->
        tag ii Punctuation

    | T.TInc ii | T.TDec ii
    | T.TOrLog ii | T.TAndLog ii | T.TOr ii
    | T.TXor ii | T.TAnd ii | T.TEqEq ii | T.TNotEq ii
    | T.TInf ii | T.TSup ii | T.TInfEq ii | T.TSupEq ii
    | T.TShl ii | T.TShr ii
    | T.TPlus ii | T.TMinus ii | T.TMul ii | T.TDiv ii | T.TMod ii  ->
        tag ii Operator

    | T.Tshort ii | T.Tint ii ->
        tag ii TypeInt
    | T.Tdouble ii | T.Tfloat ii
    | T.Tlong ii |  T.Tunsigned ii | T.Tsigned ii
    | T.Tchar ii
      -> tag ii TypeInt (* TODO *)
    | T.Tvoid ii
      -> tag ii TypeVoid
    | T.Tbool ii
    | T.Twchar_t ii
      -> tag ii TypeInt
    (* thrift stuff *)

    (* needed only when have FP in the typedef inference *)
    | T.TIdent (
      ("string" | "i32" | "i64" | "i8" | "i16" | "byte"
      (* | "list" | "map" | "set"
         | "binary"
      *)
      ), ii) ->
        tag ii TypeInt

    | T.Tauto ii | T.Tregister ii | T.Textern ii | T.Tstatic ii
    | T.Tconst ii | T.Tconst_MacroDeclConst ii | T.Tvolatile ii
    | T.Tbreak ii | T.Tcontinue ii
    | T.Treturn ii
    | T.Tdefault ii
    | T.Tsizeof ii
    | T.Trestrict ii
    | T.Tconstexpr ii | T.Tthread_local ii
      ->
        tag ii Keyword

    | T.Tgoto ii ->
        (* people often use goto as an try/throw exception mechanism
         * so let's use the same color
        *)
        tag ii Keyword

    | T.Tasm ii | T.Tattribute ii
    | T.Tinline ii | T.Ttypeof ii
      ->
        tag ii Keyword

    (* pp *)
    | T.TDefine ii ->
        tag ii Define
    | T.TUndef (_, ii) ->
        tag ii Define

    (* todo: could be also a MacroFunc *)
    | T.TIdent_Define (_, ii) ->
        tag ii (Entity (Constant, (Def2 NoUse)))

    (* TODO: have 2 tokens?
       | T.TInclude_Filename (_, ii) ->
        tag ii String
       | T.TInclude_Start (ii, _aref) ->
        tag ii Include
    *)
    | T.TInclude (_, _, ii) ->
        tag ii Include

    | T.TIfdef ii | T.TIfdefelse ii | T.TIfdefelif ii | T.TEndif ii ->
        tag ii Ifdef
    | T.TIfdefBool (_, ii) | T.TIfdefMisc (_, ii) | T.TIfdefVersion (_, ii) ->
        tag ii Ifdef

    | T.TCppDirectiveOther ii -> tag ii CppOther
    | T.Tnamespace ii -> tag ii KeywordModule

    | T.Tthis ii -> tag ii (Entity (Class, (Use2 fake_no_use2)))

    | T.Tnew ii | T.Tdelete ii ->
        tag ii KeywordObject
    | T.Tvirtual ii  ->
        tag ii KeywordObject

    | T.Ttemplate ii | T.Ttypeid ii | T.Ttypename ii  | T.Tdecltype ii
    | T.Toperator ii
    | T.Tpublic ii | T.Tprivate ii | T.Tprotected ii | T.Tfriend ii
    | T.Tusing ii
    | T.Tconst_cast ii | T.Tdynamic_cast ii
    | T.Tstatic_cast ii | T.Treinterpret_cast ii
    | T.Texplicit ii | T.Tmutable ii
      ->
        tag ii Keyword

    | T.TPtrOpStar ii | T.TDotStar ii  ->
        tag ii Punctuation

    | T.TColCol ii  | T.TColCol_BeforeTypedef ii ->
        tag ii Punctuation

    | T.Ttypedef ii | T.Tunion ii | T.Tenum ii ->
        tag ii Keyword

    | T.Tif ii | T.Telse ii ->
        tag ii KeywordConditional

    | T.Tswitch ii | T.Tcase ii ->
        tag ii KeywordConditional

    | T.Ttry ii | T.Tcatch ii | T.Tthrow ii ->
        tag ii KeywordExn

    (* thrift *)
    | T.TIdent (("throws" | "exception"), ii) ->
        tag ii KeywordExn

    | T.Tfor ii | T.Tdo ii | T.Twhile ii ->
        tag ii KeywordLoop

    | T.Tclass ii | T.Tstruct ii ->
        tag ii KeywordObject

    (* thrift *)
    | T.TIdent (("service" | "include" | "extends"), ii) ->
        tag ii Keyword

    (* should be covered by the xxx_namei case above *)
    | T.TIdent (_, _ii) ->
        ()
    (* should be covered by TypeName above *)
    | T.TIdent_Typedef _

    | T.TIdent_TemplatenameInQualifier_BeforeTypedef _
    | T.TIdent_TemplatenameInQualifier _
    | T.TIdent_TypedefConstr _
    | T.TIdent_Constructor _
    | T.TIdent_Templatename _
    | T.TIdent_ClassnameInQualifier_BeforeTypedef _
    | T.TIdent_ClassnameInQualifier _

    | T.TIdent_MacroIterator _
    | T.TIdent_MacroDecl _
    | T.TIdent_MacroString _
    | T.TIdent_MacroStmt _
      -> ()

    | T.Tbool_Constr ii | T.Tlong_Constr ii | T.Tshort_Constr ii
    | T.Twchar_t_Constr ii | T.Tdouble_Constr ii | T.Tfloat_Constr ii
    | T.Tint_Constr ii | T.Tchar_Constr ii | T.Tunsigned_Constr ii
    | T.Tsigned_Constr ii
      -> tag ii TypeInt

    | T.TInt_ZeroVirtual _
    | T.TCCro_new _ | T.TOCro_new _ -> ()

    | T.TSup_Template ii | T.TInf_Template ii ->
        tag ii Keyword

    | T.TAny_Action _
    | T.TCPar_EOL _
      -> ()

    (* TODO *)
    | T.TComment_Pp (kind, ii) ->
        (match kind with
         | Token_cpp.CppMacroExpanded | Token_cpp.CppPassingNormal ->
             tag ii Expanded
         | _ -> tag ii Passed
        )
    | T.TComment_Cpp (_kind, ii) ->
        tag ii Passed

    | T.TDefParamVariadic _

    | T.TCommentNewline _ | T.TCommentNewline_DefineEndOfMacro _
    | T.TCppEscapedNewline _
    | T.TCommentSpace _

    | T.EOF _
      -> ()
    | T.TUnknown ii -> tag ii Error
  );
  ()
