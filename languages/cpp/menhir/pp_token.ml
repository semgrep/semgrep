(* Yoann Padioleau
 *
 * Copyright (C) 2007, 2008 Ecole des Mines de Nantes
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)

open Common
module Flag = Flag_parsing
module Ast = Ast_cpp
module TH = Token_helpers_cpp
module Parser = Parser_cpp
module Hack = Parsing_hacks_lib
open Parser_cpp
open Token_views_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * CPP functions working at the token level. See pp_ast.ml for cpp functions
 * working at the AST level (which is very unusual but makes sense in
 * the coccinelle context for instance).
 *
 * Note that because I use a single lexer to work both at the C and cpp level
 * there are some inconveniencies.
 * For instance 'for' is a valid name for a macro parameter and macro
 * body, but is interpreted in a special way by our single lexer, and
 * so at some places where I expect a TIdent I need also to
 * handle special cases and accept Tfor, Tif, etc at those places.
 *
 * There are multiple issues related to those keywords incorrect tokens.
 * Those keywords can be:
 *
 *   - (1) in the name of the macro as  in  #define inline
 *   - (2) in a parameter of the macro as in #define foo(char)   char x;
 *   - (3) in an argument to a macro call as in   IDENT(if);
 *
 * Case 1 is easy to fix in define_ident in ???
 *
 * Case 2 is easy to fix in define_parse below, where we detect such tokens
 * in the parameters and then replace their occurence in the body with
 * a TIdent.
 *
 * Case 3 is only an issue when the expanded token is not really used
 * as usual but used for instance in concatenation as in  a ## if
 * when expanded. In the case the grammar this time will not be happy
 * so this is also easy to fix in cpp_engine.
 *)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the tokens in the body of the macro are all ExpandedTok *)
type define_body = (unit, string list) Either.t * Parser_cpp.token list

(* TODO:
   type define_def = string * define_param * define_body
   and define_param =
   | NoParam
   | Params of string list
   and define_body =
   | DefineBody of Parser_c.token list
   | DefineHint of parsinghack_hint

   and parsinghack_hint =
     | HintIterator
     | HintDeclarator
     | HintMacroString
     | HintMacroStatement
     | HintAttribute
     | HintMacroIdentBuilder
*)

(*****************************************************************************)
(* Apply macro (using standard.h or other defs) *)
(*****************************************************************************)
(* cpp-builtin part1, macro, using standard.h or other defs *)

(* Thanks to this function many stuff are not anymore hardcoded in
 * OCaml code (but are now hardcoded in standard.h ...)
 *)
let (cpp_engine :
      (string, Parser.token list) Assoc.t ->
      Parser.token list ->
      Parser.token list) =
 fun env xs ->
  xs
  |> List_.map (fun tok ->
         match tok with
         | TIdent (s, _i1) when List.mem_assoc s env -> Common2.assoc s env
         | x -> [ x ])
  |> List_.flatten

(*
 * We apply a macro by generating new ExpandedToken and by
 * commenting the old macro call.
 *
 * no need to take care to substitute the macro name itself
 * that occurs in the macro definition because the macro name is
 * after fix_token_define a TDefineIdent, no more a TIdent.
 *)
let apply_macro_defs defs xs =
  let rec apply_macro_defs xs =
    match xs with
    | [] -> ()
    (* recognized macro of standard.h (or other) *)
    | PToken ({ t = TIdent (s, _i1); _ } as id)
      :: Parenthised (xxs, info_parens)
      :: xs
      when Hashtbl.mem defs s ->
        Hack.pr2_pp ("MACRO: found known macro = " ^ s);
        (match Hashtbl.find defs s with
        | Either.Left (), bodymacro ->
            pr2 ("macro without param used before parenthize, wierd: " ^ s);
            (* ex: PRINTP("NCR53C400 card%s detected\n" ANDP(((struct ... *)
            Hack.set_as_comment Token_cpp.CppMacroExpanded id;
            id.new_tokens_before <- bodymacro
        | Either.Right params, bodymacro ->
            if List.length params =|= List.length xxs then
              let xxs' =
                xxs
                |> List_.map (fun x ->
                       tokens_of_paren_ordered x
                       |> List_.map (fun x ->
                              TH.visitor_info_of_tok Ast.make_expanded x.t))
              in
              id.new_tokens_before <-
                cpp_engine (Common2.zip params xxs') bodymacro
            else (
              pr2 ("macro with wrong number of arguments, wierd: " ^ s);
              id.new_tokens_before <- bodymacro);
            (* important to do that after have apply the macro, otherwise
             * will pass as argument to the macro some tokens that
             * are all TCommentCpp
             *)
            [ Parenthised (xxs, info_parens) ]
            |> iter_token_paren (Hack.set_as_comment Token_cpp.CppMacroExpanded);
            Hack.set_as_comment Token_cpp.CppMacroExpanded id);
        apply_macro_defs xs
    | PToken ({ t = TIdent (s, _i1); _ } as id) :: xs when Hashtbl.mem defs s ->
        Hack.pr2_pp ("MACRO: found known macro = " ^ s);
        (match Hashtbl.find defs s with
        | Either.Right _params, _bodymacro ->
            pr2 ("macro with params but no parens found, wierd: " ^ s);
            (* dont apply the macro, perhaps a redefinition *)
            ()
        | Either.Left (), bodymacro -> (
            (* special case when 1-1 substitution, we reuse the token *)
            match bodymacro with
            | [ newtok ] ->
                id.t <-
                  newtok
                  |> TH.visitor_info_of_tok (fun _ -> TH.info_of_tok id.t)
            | _ ->
                Hack.set_as_comment Token_cpp.CppMacroExpanded id;
                id.new_tokens_before <- bodymacro));
        apply_macro_defs xs
    (* recurse *)
    | PToken _x :: xs -> apply_macro_defs xs
    | Parenthised (xxs, _info_parens) :: xs ->
        xxs |> List.iter apply_macro_defs;
        apply_macro_defs xs
  in

  apply_macro_defs xs

(*****************************************************************************)
(* Extracting macros (from a standard.h) *)
(*****************************************************************************)

(* assumes have called fix_tokens_define before, so have TOPar_Define *)
let rec define_parse xs =
  match xs with
  | [] -> []
  | TDefine _i1 :: TIdent_Define (s, _i2) :: TOPar_Define _i3 :: xs ->
      let tokparams, _, xs =
        xs
        |> Common2.split_when (function
             | TCPar _ -> true
             | _ -> false)
      in
      let body, _, xs =
        xs
        |> Common2.split_when (function
             | TCommentNewline_DefineEndOfMacro _ -> true
             | _ -> false)
      in
      let params =
        tokparams
        |> List_.filter_map (function
             | TComma _ -> None
             | TIdent (s, _) -> Some s
             | x -> Common2.error_cant_have x)
      in
      let body = body |> List_.map (TH.visitor_info_of_tok Ast.make_expanded) in
      let def = (s, (Either.Right params, body)) in
      def :: define_parse xs
  | TDefine _i1 :: TIdent_Define (s, _i2) :: xs ->
      let body, _, xs =
        xs
        |> Common2.split_when (function
             | TCommentNewline_DefineEndOfMacro _ -> true
             | _ -> false)
      in
      let body = body |> List_.map (TH.visitor_info_of_tok Ast.make_expanded) in
      let def = (s, (Either.Left (), body)) in
      def :: define_parse xs
  | TDefine _i1 :: _ -> raise Impossible
  | _x :: xs -> define_parse xs

let extract_macros xs =
  let cleaner = xs |> List.filter (fun x -> not (TH.is_comment x)) in
  define_parse cleaner
