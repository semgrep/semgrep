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
open Parser_css

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | TComment _ -> true
  | _ -> false

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | TComment ii -> TComment (f ii)
  | S ii -> S (f ii)
  | CHARSET ii -> CHARSET (f ii)
  | IMPORT ii -> IMPORT (f ii)
  | MEDIA ii -> MEDIA (f ii)
  | PAGE ii -> PAGE (f ii)
  | FONTFACE ii -> FONTFACE (f ii)
  | OPEN_CURLY ii -> OPEN_CURLY (f ii)
  | CLOSE_CURLY ii -> CLOSE_CURLY (f ii)
  | OPEN_ROUND ii -> OPEN_ROUND (f ii)
  | CLOSE_ROUND ii -> CLOSE_ROUND (f ii)
  | OPEN_SQUARE ii -> OPEN_SQUARE (f ii)
  | CLOSE_SQUARE ii -> CLOSE_SQUARE (f ii)
  | SEMICOLON ii -> SEMICOLON (f ii)
  | COLON ii -> COLON (f ii)
  | DOUBLE_COLON ii -> DOUBLE_COLON (f ii)
  | COMMA ii -> COMMA (f ii)
  | PERIOD ii -> PERIOD (f ii)
  | SLASH ii -> SLASH (f ii)
  | ASTERISK ii -> ASTERISK (f ii)
  | QUOTIENT ii -> QUOTIENT (f ii)
  | PLUS ii -> PLUS (f ii)
  | MINUS ii -> MINUS (f ii)
  | TILDE ii -> TILDE (f ii)
  | GT ii -> GT (f ii)
  | IMPORTANT ii -> IMPORTANT (f ii)
  | ATTR_EQUALS ii -> ATTR_EQUALS (f ii)
  | ATTR_INCLUDES ii -> ATTR_INCLUDES (f ii)
  | ATTR_DASHMATCH ii -> ATTR_DASHMATCH (f ii)
  | ATTR_PREFIX ii -> ATTR_PREFIX (f ii)
  | ATTR_SUFFIX ii -> ATTR_SUFFIX (f ii)
  | ATTR_SUBSTRING ii -> ATTR_SUBSTRING (f ii)
  | URI ii -> URI (f ii)

  | TString (s, ii) -> TString (s, f ii)
  | IDENT (s, ii) -> IDENT (s, f ii)
  | NTH (s, ii) -> NTH (s, f ii)
  | HASH (s, ii) -> HASH (s, f ii)
  | VAR (s, ii) -> VAR (s, f ii)
  | SEL_FUNC (s, ii) -> SEL_FUNC (s, f ii)
  | TERM_FUNC (s, ii) -> TERM_FUNC (s, f ii)
  | QUANTITY (s, ii) -> QUANTITY (s, f ii)

  | TUnknown ii -> TUnknown (f ii)
  | EOF(ii) -> EOF(f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  Common2.some !res
