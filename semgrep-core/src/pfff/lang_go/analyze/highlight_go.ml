(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

open Highlight_code
module T = Parser_go
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Go code for codemap (and now also efuns).
 *
 * Making a code highlighter, like a deadcode checker, allows to find
 * bugs in the parsing/AST/understanding of the language:
 *  - parameters semantic in Go is special (foo(a,b,c,d int)) or foo(int,int)
 *  - need List.rev for stmts in many more places, not just compound_stmt
 *  -
*)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

(* we generate fake value here because the real one are computed in a
 * later phase in rewrite_categ_using_entities in pfff_visual.
*)
let _def2 = Def2 NoUse
let use2 = Use2 (NoInfoPlace, UniqueDef, MultiUse)


(* coupling: dupe of list in lexer_go.mll comment *)
(* declared in the "universe block"
 *  - true, false
 *  - iota
 *  - new, make,
 *    panic (CFG effect, like goto), recover,
 *    print, println
 *    complex, imag, real
 *    append, cap,
 *    close, delete, copy,
 *    len,
 *  - nil
 *  - _ (blank identifier)
*)
let builtin_functions = Common.hashset_of_list [
  "iota";
  "new"; "make";

  "print"; "println";
  "complex"; "imag"; "real";
  "append"; "cap";
  "close"; "delete";"copy";
  "len";
]

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

(* The idea of the code below is to visit the program either through its
 * AST or its list of tokens. The tokens are easier for tagging keywords,
 * number and basic entities. The AST is better for tagging idents
 * to figure out what kind of ident it is.
*)

let visit_program ~tag_hook _prefs (_program, toks) =
  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    tag_hook ii categ;
    Hashtbl.replace already_tagged ii true
  )
  in
  let tag_if_not_tagged ii categ =
    (* thx to the if below, you can treat the most specific in enclosing code
     * and then not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
    *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ
  in
  (* Resolve_go.resolve program; *)

  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)
  (* try to better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
  *)
  (* TODO: use the generic AST highlighter now
   * or update it to handle the idioms below
     let tag_ident (_s, ii) categ = tag_if_not_tagged ii categ in  let _tag_qid xs categ =
      match xs with
      | [] | _::_::_::_ -> raise Common.Impossible
      | [x] -> tag_ident x categ
      | [x;y] ->
          tag_ident x (Entity (E.Module, use2));
          tag_ident y categ
     in

     let in_toplevel = ref true in

     let visitor = V.mk_visitor { V.default_visitor with
                                 (* use 'k x' as much as possible below. No need to
                                  * do v (Stmt st1); v (Expr e); ... Go deep to tag
                                  * special stuff (e.g., a local var in an exception handler) but then
                                  * just recurse from the top with 'k x', tag_if_not_tagged will
                                  * do its job.
                                 *)

                                 (* defs *)
                                 V.kprogram = (fun (k, _) x ->
                                   let (package, imports) = package_and_imports_of_program x in

                                   tag_ident (snd package) (Entity (E.Module, def2));
                                   imports |> List.iter (fun import ->
                                     tag_ident import.i_path (Entity (E.Module, use2));
                                     match import.i_kind with
                                     | ImportNamed id -> tag_ident id (Entity (E.Module, def2))
                                     | ImportOrig | ImportDot _ -> ()
                                   );
                                   k x
                                 );
                                 V.ktop_decl = (fun (k, _) x ->
                                   (match x with
                                    | DFunc   (_, id,   (_t, _st)) -> tag_ident id (Entity (E.Function, def2))
                                    | DMethod (_, id,_o,(_t, _st)) -> tag_ident id (Entity (E.Method, def2))
                                    | DTop _ | STop _ -> ()
                                    | Package _ | Import _ -> ()
                                   );
                                   Common.save_excursion in_toplevel false (fun () ->
                                     k x
                                   );
                                 );
                                 V.kdecl = (fun (k, _) x ->
                                   (match x with
                                    | DTypeDef (id, _) | DTypeAlias (id, _, _) ->
                                        tag_ident id (Entity (E.Type, def2))
                                    | DConst (id, _, _) -> tag_ident id (Entity (E.Constant, def2))
                                    | DVar (id, _, _) ->
                                        if !in_toplevel
                                        then tag_ident id (Entity (E.Global, def2))
                                        else tag_ident id (Local Def)
                                   );
                                   k x
                                 );
                                 V.kparameter = (fun (k, _) x ->
                                   x.pname |> Option.iter (fun id ->
                                     tag_ident id (Parameter Def)
                                   );
                                   k x
                                 );
                                 V.kstmt = (fun (k, _) x ->
                                   (match x with
                                    | SimpleStmt (DShortVars (xs, _, _)) ->
                                        xs |> List.iter (function
                                          | Id (id) ->
                                              if !in_toplevel
                                              then tag_ident id (Entity (E.Global, def2))
                                              else tag_ident id (Local Def)
                                          | _ -> ()
                                        )
                                    (* general case *)
                                    | _ -> ()
                                   );
                                   k x
                                 );


                                 (* uses *)

                                 V.ktype = (fun (k, _) x ->
                                   (match x with
                                    | TName ([(
                                      "int" | "uint32" | "float" | "double"
                                    ), ii]) -> tag ii TypeInt
                                    | TName qid -> tag_qid qid (Entity (E.Type, use2))

                                    | TStruct (_, flds) ->
                                        flds |> unbracket |> List.iter (fun (fld, tag_opt) ->
                                          tag_opt |> Option.iter (fun tag -> tag_ident tag Attribute);
                                          (match fld with
                                           | Field (id, _) -> tag_ident id (Entity (E.Field, def2));
                                           | EmbeddedField (_, qid) -> tag_qid qid (Entity (E.Type, use2))
                                           | FieldEllipsis _ -> ()
                                          );
                                        );
                                    | TInterface (_, flds) ->
                                        flds |> unbracket |> List.iter (function
                                          | Method (id, _)        -> tag_ident id (Entity (E.Method, def2))
                                          | EmbeddedInterface qid -> tag_qid qid (Entity (E.Type, use2))
                                          | FieldEllipsis2 _ -> ()
                                        );
                                        (* general case *)
                                    | _ -> ()
                                   );
                                   k x
                                 );

                                 V.kexpr = (fun (k, _) x ->
                                   (match x with
                                    (* TODO
                                                                      | Call (Selector (Id (_m, {contents=Some (G.ImportedModule _,_)}),_,fld),_)->
                                                                          tag_ident fld (Entity (E.Function, use2));
                                    *)
                                    | Call (Selector (_, _, fld),_) ->
                                        tag_ident fld (Entity (E.Method, use2));
                                    | Selector (Id (_m), _, fld) ->
                                        tag_ident fld (Entity (E.Field, use2));
                                    | Id (_id) -> ()
     (*
                                        (match !resolved with
                                         | None -> ()
                                         | Some x ->
                                             (match fst x with
                                              | G.ImportedModule _ -> tag_ident id (Entity (E.Module, use2))
                                              | G.Param -> tag_ident id (Parameter (Use))
                                              | G.Local -> tag_ident id (Local (Use))
                                              | G.EnclosedVar -> tag_ident id (Local Use) (* TODO *)
                                              (* unless matched before in a Call *)
                                              | G.Global -> tag_ident id (Entity (E.Global, use2))
                                              | G.ImportedEntity _ -> tag_ident id (Entity (E.Global, use2))
                                              | G.TypeName -> tag_ident id (Entity (E.Type, use2))
                                              | G.Macro | G.EnumConstant -> ()
                                             )
                                        )
   *)

                                    (* general case *)
                                    | _ -> ()
                                   );
                                   k x
                                 );

                                 V.kinit = (fun (k, _) x ->
                                   (match x with
                                    | InitKeyValue (InitExpr (Id (id)), _, _) ->
                                        tag_ident id (Entity (E.Field, use2))
                                    | _ -> ()
                                   );
                                   k x

                                 );

                               } in
     visitor (P program);
  *)

  (* -------------------------------------------------------------------- *)
  (* tokens phase 1 (list of tokens) *)
  (* -------------------------------------------------------------------- *)
  (* -------------------------------------------------------------------- *)
  (* Tokens phase 2 (individual tokens) *)
  (* -------------------------------------------------------------------- *)

  toks |> List.iter (fun tok ->
    match tok with

    (* specials *)
    | T.TUnknown ii ->
        tag ii Error
    | T.EOF _ii ->
        ()

    (* comments *)
    | T.TComment ii ->
        tag_if_not_tagged ii Comment
    | T.TCommentSpace _ | T.TCommentNewline _ -> ()

    (* values  *)
    | T.LSTR (_,ii) ->
        tag_if_not_tagged ii String (* can be a Module in an import, or a Tag *)
    | T.LRUNE (_, ii) ->
        tag ii String
    | T.LFLOAT (_,ii) | T.LINT (_,ii) | T.LIMAG (_,ii) ->
        tag ii Number

    (* ident  *)
    | T.LNAME (s, ii) ->
        (match s with
         | "true" | "false" -> tag_if_not_tagged ii Boolean
         | "nil" -> tag_if_not_tagged ii Null

         | "panic" | "recover" -> tag ii KeywordExn
         | "int" | "uint32" | "string" ->
             tag_if_not_tagged ii (Entity (E.Type, use2))
         | s when Hashtbl.mem builtin_functions s ->
             tag_if_not_tagged ii Builtin

         (* should have been tagged by the AST visitor *)
         | _ ->
             tag_if_not_tagged ii IdentUnknown
        )

    (* keywords  *)
    | T.LFUNC ii | T.LCONST ii | T.LVAR ii | T.LTYPE ii ->
        tag ii Keyword
    | T.LSTRUCT ii -> tag ii Keyword
    | T.LINTERFACE ii
      -> tag ii KeywordObject
    | T.LIF ii | T.LELSE ii
    | T.LSWITCH ii | T.LCASE ii | T.LDEFAULT ii
      ->
        tag ii KeywordConditional
    | T.LFOR ii | T.LRANGE ii
      -> tag ii KeywordLoop
    | T.LPACKAGE ii  | T.LIMPORT ii
      -> tag ii KeywordModule
    | T.LSELECT ii | T.LGO ii | T.LCHAN ii
      -> tag ii KeywordConcurrency
    | T.LCONTINUE ii | T.LBREAK ii
    | T.LFALL ii
    | T.LRETURN ii
      -> tag ii Keyword
    | T.LGOTO ii
      -> tag ii Keyword (* dangerous? *)
    | T.LMAP ii ->
        tag ii (Entity (E.Type, use2))
    | T.LDEFER ii ->
        tag ii KeywordExn

    (* symbols *)
    | T.LEQ ii | T.LCOLAS ii ->
        tag ii Punctuation

    | T.LASOP (_, ii) -> tag ii Punctuation

    | T.LBRACE ii | T.LBODY ii | T.LBRACE_SEMGREP ii
    | T.RBRACE ii
    | T.LBRACKET ii | T.RBRACKET ii
    | T.LPAREN ii | T.RPAREN ii | LPAREN_SEMGREP ii
      -> tag ii Punctuation

    | T.LPLUS ii  -> tag ii Punctuation
    | T.LMINUS ii -> tag ii Punctuation

    | T.LMULT ii | T.LDIV ii
    | T.LPERCENT ii

    | T.LLSH ii | T.LRSH ii
    | T.LINC ii | T.LDEC ii

    | T.LANDAND ii | T.LOROR ii
    | T.LAND ii | T.LPIPE ii | T.LHAT ii | T.LANDNOT ii

    | T.LBANG ii

    | T.LEQEQ ii | T.LNE ii
    | T.LLT ii  | T.LGT ii
    | T.LLE ii | T.LGE ii

    | T.LDOT ii
    | T.LCOLON ii | T.LCOLON_SEMGREP ii
    | T.LCOMMA ii
    | T.LSEMICOLON ii
      ->
        tag ii Punctuation

    | T.LCOMM ii ->
        tag ii KeywordConcurrency

    | T.LDDD ii | T.LDots ii | T.RDots ii ->
        tag ii Punctuation
  );
  ()
