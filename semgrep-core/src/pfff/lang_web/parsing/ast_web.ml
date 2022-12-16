(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This file contains the type definitions for a full web document, that is
 * HTML + JS(script) + CSS(style)
 *
 * The difference with other HTML parsers like html5lib is that this one
 * understands JS and CSS too and the possible HTML strings inside
 * the JS code and so on; it's a recursive parser.
 *)

type web_document = {
  html: Ast_html.html_tree;

  js: (Ast_html.info * Cst_js.program) list;
  css: (Ast_html.info * Ast_css.stylesheet) list;
  stuff_in_js: (Cst_js.tok * web_document) list;
}

(* move in tokens_web.ml ? or fake parser_web.mly ? or token_helpers.ml ? *)
type token =
  | THtml of Parser_html.token
  | TJs of Parser_js.token
  | TCss of Parser_css.token
