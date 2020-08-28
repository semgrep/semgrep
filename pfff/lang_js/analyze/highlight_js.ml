(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Highlight_code
open Ast_js
module T = Parser_js
module E = Entity_code
module H = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Syntax highlighting for Javascript code for codemap (and now also efuns).
 *)

(*****************************************************************************)
(* Helpers when have global-analysis information *)
(*****************************************************************************)

let fake_no_def2 = NoUse
let fake_no_use2 = (NoInfoPlace, UniqueDef, MultiUse)

(*****************************************************************************)
(* Code highlighter *)
(*****************************************************************************)

let visit_program ~tag_hook _prefs (cst, toks) =

  let already_tagged = Hashtbl.create 101 in
  let tag = (fun ii categ ->
    (match categ with
    (* Javascript allows to use keywords as identifiers for flds/methods/...*)
    | Keyword | KeywordExn | KeywordLoop | KeywordModule
       when Hashtbl.mem already_tagged ii -> ()
    | _ ->
      tag_hook ii categ;
    );
    Hashtbl.add already_tagged ii true
  )
  in
  let tag_name (_s, ii) categ = 
    (* so treat the most specific in the enclosing code and then
     * do not fear to write very general case patterns later because
     * the specific will have priority over the general
     * (e.g., a Method use vs a Field use)
     *)
    if not (Hashtbl.mem already_tagged ii)
    then tag ii categ 
  in
  let tag_if_not_tagged ii categ =
   if not (Hashtbl.mem already_tagged ii)    
   then tag ii categ
  in
  (* -------------------------------------------------------------------- *)
  (* AST phase 1 *)
  (* -------------------------------------------------------------------- *)
  (* try to better colorize identifiers which can be many different things
   * e.g. a field, a type, a function, a parameter, etc
   *)
  let ast = 
     try Ast_js_build.program cst 
     with Ast_js_build.TodoConstruct _ | Ast_js_build.UnhandledConstruct _ 
       -> []
  in
  let visitor = Visitor_ast_js.mk_visitor { Visitor_ast_js.default_visitor with
     Visitor_ast_js.ktop = (fun (k, _) t ->
       (match t with
       | VarDecl {v_name = name; v_kind; v_init; v_resolved = _resolved } ->
           let kind = Graph_code_js.kind_of_expr_opt v_kind v_init in
           tag_name name (Entity (kind, (Def2 fake_no_def2)));
       | _ -> ()
       );
       k t
     );
     Visitor_ast_js.kprop = (fun (k,_) x ->
      (match x with
      | Field (PN name, _, Some (Fun _)) ->
          tag_name name (Entity (E.Method, (Def2 fake_no_def2)));
      | Field (PN name, _, _) ->
          tag_name name (Entity (E.Field, (Def2 fake_no_def2)));
      | _ -> ()
      ); 
      k x
      );
     Visitor_ast_js.kexpr = (fun (k,_) x ->
      (match x with
      | ObjAccess (_, _, PN name) ->
          tag_name name (Entity (E.Field, (Use2 fake_no_use2)));
      | IdSpecial (special, ii) ->
         (match special with
         | Eval -> tag ii BadSmell
         | _ -> tag ii Builtin
         )
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
      | Apply (ObjAccess (_, _, PN name), _) ->
         tag_name name (Entity (E.Method, (Use2 fake_no_use2)));
      | Fun (_, Some name) ->
         tag_name name (Entity (E.Function, (Use2 fake_no_use2)));
      | _ -> ()
      ); k x
     );
     Visitor_ast_js.kstmt = (fun (k,_) x ->
      (match x with
      | VarDecl ({v_name = name; _}) ->
          tag_name name (H.Local Def);
      | _ -> ()
      ); k x
     );
     Visitor_ast_js.kparam = (fun (k, _) x ->
       (match x with
       | {p_name = name; _} ->
           tag_name name (H.Parameter Def);
       ); k x
     );

    } in
  visitor (Program ast);

  (* -------------------------------------------------------------------- *)
  (* token phase 1 (individual tokens) *)
  (* -------------------------------------------------------------------- *)
  toks |> List.iter (fun tok ->
    match tok with
    (* specials *)

    (* less: could highlight certain words in the comment? *)
    | T.TComment ii ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii Comment
    | T.TCommentSpace (_ii) | T.TCommentNewline (_ii) -> ()

    | T.TUnknown (ii) -> tag ii Error
    | T.EOF (_ii) -> ()

    (* values *)

    | T.T_NULL (ii) -> tag ii H.Null
    | T.T_FALSE (ii) | T.T_TRUE (ii) -> tag ii Boolean
    | T.T_NUMBER (_, ii) -> tag ii Number

    (* still? both strings and regular identifiers can be used to represent
     * entities such as classes so have to look for both? *)
    | T.T_STRING (_, ii) | T.T_ENCAPSED_STRING(_, ii) ->
        if not (Hashtbl.mem already_tagged ii)
        then tag ii H.String

    | T.T_BACKQUOTE ii -> tag ii H.String
    | T.T_DOLLARCURLY ii -> tag ii H.String

    | T.T_REGEX (_, ii) -> tag ii H.Regexp

    (* all the name and varname should have been tagged by now. *)
    | T.T_ID (_, ii) ->
       if not (Hashtbl.mem already_tagged ii)
       then tag ii Error

    (* keywords *)

    | T.T_FUNCTION (ii) 
      ->  tag ii Keyword
    | T.T_VAR (ii) | T.T_LET (ii) | T.T_CONST (ii) 
      -> tag ii Keyword
    | T.T_IF (ii)  | T.T_SWITCH (ii) | T.T_ELSE (ii)
      ->  tag ii KeywordConditional
    | T.T_WHILE (ii) | T.T_DO (ii) | T.T_FOR (ii)
      ->  tag ii KeywordLoop
    | T.T_RETURN (ii)
    | T.T_BREAK (ii)  | T.T_CONTINUE (ii)
    | T.T_CASE (ii) | T.T_DEFAULT (ii)
      -> tag ii Keyword
    | T.T_IN (ii) | T.T_OF (ii)
      -> tag ii Keyword
    | T.T_THIS (ii) | T.T_SUPER (ii) | T.T_INSTANCEOF (ii)
    | T.T_NEW (ii)  | T.T_DELETE (ii)
      -> tag ii KeywordObject
    | T.T_THROW (ii) | T.T_TRY (ii) | T.T_CATCH (ii) | T.T_FINALLY (ii)
      -> tag ii KeywordExn
    | T.T_YIELD ii | T.T_ASYNC ii | T.T_AWAIT ii
      -> tag ii Keyword
    | T.T_TYPEOF (ii) 
      -> tag ii Keyword
    | T.T_CLASS ii | T.T_INTERFACE ii
    | T.T_EXTENDS ii | T.T_IMPLEMENTS ii
     -> tag ii KeywordObject
    | T.T_CONSTRUCTOR ii    | T.T_GET ii | T.T_SET ii
     -> tag ii KeywordObject
    | T.T_IMPORT ii | T.T_EXPORT ii 
    | T.T_FROM ii | T.T_AS ii
      -> tag ii KeywordModule
    | T.T_STATIC ii | T.T_WITH (ii) 
       -> tag ii Keyword
    | T.T_TYPE ii | T.T_ENUM ii | T.T_DECLARE ii
      -> tag ii Keyword
    | T.T_MODULE ii ->
        tag ii KeywordModule
    | T.T_PUBLIC ii | T.T_PRIVATE ii | T.T_PROTECTED ii ->
        tag ii KeywordObject
    | T.T_READONLY ii ->
        tag ii Keyword

    | T.T_VOID (ii) ->
        tag ii TypeVoid
    | T.T_NUMBER_TYPE (ii) ->
        tag_if_not_tagged ii TypeInt
    | T.T_ANY_TYPE ii  | T.T_BOOLEAN_TYPE ii | T.T_STRING_TYPE ii
     -> tag_if_not_tagged ii (Entity (E.Type, Use2 fake_no_use2))

    | T.T_XHP_TEXT (_, ii) -> tag ii H.String
    | T.T_XHP_ATTR (_, ii) -> tag ii (Entity (E.Field, (Use2 fake_no_use2)))
    | T.T_XHP_CLOSE_TAG (_, ii) -> tag ii EmbededHtml
    | T.T_XHP_SLASH_GT ii -> tag ii EmbededHtml
    | T.T_XHP_GT ii -> tag ii EmbededHtml
    | T.T_XHP_OPEN_TAG (_, ii) | T.T_XHP_SHORT_FRAGMENT ii
      -> tag ii EmbededHtml

    (* Punctuation *)

    | T.T_LCURLY (ii) | T.T_RCURLY (ii) | T.T_LCURLY_SEMGREP ii
    | T.T_LPAREN (ii) | T.T_LPAREN_ARROW (ii) | T.T_RPAREN (ii)
    | T.T_LBRACKET (ii) | T.T_RBRACKET (ii)
    | T.T_SEMICOLON (ii)
    | T.T_COMMA (ii)
    | T.T_PERIOD (ii)
    | T.T_DOTS ii | T.LDots ii | T.RDots ii
        -> tag ii Punctuation

    (* Operators *)

    | T.T_RSHIFT3_ASSIGN (ii) | T.T_RSHIFT_ASSIGN (ii) | T.T_LSHIFT_ASSIGN (ii)
    | T.T_BIT_XOR_ASSIGN (ii) | T.T_BIT_OR_ASSIGN (ii)| T.T_BIT_AND_ASSIGN (ii)
    | T.T_MOD_ASSIGN (ii)  | T.T_DIV_ASSIGN (ii)
    | T.T_MULT_ASSIGN (ii) | T.T_MINUS_ASSIGN (ii) | T.T_PLUS_ASSIGN (ii)
        -> tag ii Punctuation

    | T.T_ASSIGN (ii)
        -> tag ii Punctuation
    | T.T_PLING (ii)
    | T.T_COLON (ii)
        -> tag ii Punctuation
    | T.T_ARROW ii -> tag ii Punctuation

    | T.T_OR (ii) | T.T_AND (ii)
    | T.T_BIT_OR (ii) | T.T_BIT_XOR (ii) | T.T_BIT_AND (ii)
    | T.T_EQUAL (ii) | T.T_NOT_EQUAL (ii)
    | T.T_STRICT_EQUAL (ii) | T.T_STRICT_NOT_EQUAL (ii)
    | T.T_LESS_THAN_EQUAL (ii) | T.T_GREATER_THAN_EQUAL (ii)
    | T.T_LESS_THAN (ii) | T.T_GREATER_THAN (ii)
    | T.T_LSHIFT (ii) | T.T_RSHIFT (ii) | T.T_RSHIFT3 (ii)
    | T.T_PLUS (ii) | T.T_MINUS (ii) | T.T_DIV (ii) | T.T_MULT (ii)
    | T.T_EXPONENT (ii)
    | T.T_MOD (ii)
    | T.T_NOT (ii) | T.T_BIT_NOT (ii)
        -> tag ii Operator
    | T.T_INCR (ii)| T.T_DECR (ii)
        -> tag ii Punctuation

    | T.T_VIRTUAL_SEMICOLON (_ii)
        -> ()

  );

  ()
