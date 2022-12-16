{
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module Ast = Ast_sql
module Flag = Flag_parsing

open Parser_sql

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let _error = Parse_info.lexical_error


let keyword_table = Common.hash_of_list [

  "select",         (fun ii -> T_SELECT ii);
  "insert",         (fun ii -> T_INSERT ii);
  "update",         (fun ii -> T_UPDATE ii);
  "delete",         (fun ii -> T_DELETE ii);

  "as",             (fun ii -> T_AS ii);
  "from",           (fun ii -> T_FROM ii);
  "where",          (fun ii -> T_WHERE ii);
  "set",            (fun ii -> T_SET ii);

  "null",           (fun ii -> T_NULLX ii);

  "all",            (fun ii -> T_ALL ii);
  "distinct",       (fun ii -> T_DISTINCT ii);

  "into",           (fun ii -> T_INTO ii);
  "replacing",      (fun ii -> T_REPLACING ii);
  "soft",           (fun ii -> T_SOFT ii);

  "or",             (fun ii -> T_OR ii);
  "and",            (fun ii -> T_AND ii);
  "not",            (fun ii -> T_NOT ii);

  "in",             (fun ii -> T_IN ii);
  "values",         (fun ii -> T_VALUES ii);
  "union",          (fun ii -> T_UNION ii);


  "like",           (fun ii -> T_LIKE ii);

  "order",          (fun ii -> T_ORDER ii);
  "by",             (fun ii -> T_BY ii);

  "asc",            (fun ii -> T_ASC ii);
  "desc",           (fun ii -> T_DESC ii);

  "limit",          (fun ii -> T_LIMIT ii);

  "commit",         (fun ii -> T_COMMIT ii);
  "begin",          (fun ii -> T_BEGIN ii);
  "rollback",       (fun ii -> T_ROLLBACK ii);

  "join",           (fun ii -> T_JOIN ii);
  "on",             (fun ii -> T_ON ii);
  "left",           (fun ii -> T_LEFT ii);
  "outer",          (fun ii -> T_OUTER ii);

  "duplicate",      (fun ii -> T_DUPLICATE ii);
  "key",            (fun ii -> T_KEY ii);

  "memcache_dirty", (fun ii -> T_MEMCACHE_DIRTY ii);
]

}

(*****************************************************************************)

let WHITESPACE = [' ' '\n' '\r' '\t']+
let TABS_AND_SPACES = [' ''\t']*

let LABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*

let LNUM =	['0'-'9']+
let DNUM =	(['0'-'9']*['.']['0'-'9']+) | (['0'-'9']+['.']['0'-'9']* )
let EXPONENT_DNUM =	((LNUM|DNUM)['e''E']['+''-']?LNUM)
let HNUM =	"0x"['0'-'9''a'-'f''A'-'F']+

let STRING = '\'' [^ '\'']* '\''

let NAME_BACKQUOTE = '`' [^ '`']* '`'

(*****************************************************************************)

rule lexer = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  | WHITESPACE { lexer lexbuf (* pass whitespaces *) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | '+' { TPLUS(tokinfo lexbuf) }      | '-' { TMINUS(tokinfo lexbuf) }
  | '*' { TMUL(tokinfo lexbuf) }       | '/' { TDIV(tokinfo lexbuf) }

  | '=' { T_COMPARISON(tokinfo lexbuf) }
  | '>' { T_COMPARISON(tokinfo lexbuf) }
  | '<' { T_COMPARISON(tokinfo lexbuf) }
  | "!=" { T_COMPARISON(tokinfo lexbuf) }

  | "!" { TOPNOT(tokinfo lexbuf) }
  | "|" { TOPOR(tokinfo lexbuf) }
  | "&" { TOPAND(tokinfo lexbuf)  }
  | "~" { TOPNEG(tokinfo lexbuf) }

  | '(' { TOPAR(tokinfo lexbuf) }
  | ')' { TCPAR(tokinfo lexbuf) }

  | ';' { TSEMICOLON(tokinfo lexbuf) }
  | ',' { TCOMMA(tokinfo lexbuf) }
  | '.' { TDOT(tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* label *)
  (* ----------------------------------------------------------------------- *)
  | LABEL {

        let info = tokinfo lexbuf in
        let s = tok lexbuf in

          match Common2.optionise (fun () ->
            Hashtbl.find keyword_table (String.lowercase_ascii s))
          with
          | Some f -> f info
          | None -> T_NAME info
        }

  | NAME_BACKQUOTE {
      T_NAME(tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* literals *)
  (* ----------------------------------------------------------------------- *)

  | LNUM { T_INTNUM(tokinfo lexbuf) }
  | HNUM  { T_DNUMBER(tokinfo lexbuf) }

  | DNUM|EXPONENT_DNUM { T_DNUMBER(tokinfo lexbuf) }

  | STRING { T_STRING(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      if !Flag.verbose_lexing
      then pr2_once ("LEXER:unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }
