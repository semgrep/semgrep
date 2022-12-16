{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2015 Facebook
 * Copyright (C) 2018 Yoann Padioleau
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

module Flag = Flag_parsing

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A basic TeX/LaTeX/Noweb lexer.
 *
 * Alternatives:
 *  - Hevea, but the code is quite complicated. I don't need all the
 *    features of TeX
 *  - Extend the parser in syncweb, but it's not a parser; It is just
 *    a very specialized lexer that recognizes only Noweb constructs
 *    I now parse more because I also do the -to_tex part of noweb but
 *    it is still not enough for the parsing and highlighting context here.
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
(* Was in parser_nw.mly but we don't really need an extra file.
 * The only "parsing" we do is just to make a fuzzy AST by parentizing braces.
 *)

type token =
  | TComment of Parse_info.t
  | TCommentSpace of Parse_info.t
  | TCommentNewline of Parse_info.t

  | TWord of (string * Parse_info.t)
  | TNumber of (string * Parse_info.t)
  (* e.g., 12pt *)
  | TUnit of (string * Parse_info.t)
  | TSymbol of (string * Parse_info.t)

  (* \xxx *)
  | TCommand of (string * Parse_info.t)

  | TOBrace of Parse_info.t | TCBrace of Parse_info.t
  | TOBracket of Parse_info.t | TCBracket of Parse_info.t
  (* no TOParen/TCParen, they are not forced to be matching in TeX *)

  (* pad-specific: \t \f \l, see noweblatexpad  *)
  | TFootnote of char * Parse_info.t

  (* verbatim (different lexing rules) *)

  | TBeginVerbatim of Parse_info.t
  | TEndVerbatim of Parse_info.t
  | TVerbatimLine of (string * Parse_info.t)

  (* start of noweb stuff (different lexing rules too) *)

  (* <<...>>= and @ *)
  | TBeginNowebChunk of Parse_info.t
  | TEndNowebChunk of Parse_info.t
  | TNowebChunkStr of (string * Parse_info.t)
  (* << >> when on the same line and inside a noweb chunk *)
  | TNowebChunkName of string * Parse_info.t

  (* [[ ]] *)
  | TNowebCode of string * Parse_info.t
  (* syncweb-specific: *)
  | TNowebCodeLink of string * Parse_info.t

  | TUnknown of Parse_info.t
  | EOF of Parse_info.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* let keyword_table = Common.hash_of_list [] ? not needed. No keyword
 * in TeX, just commands.
 *)

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
type state_mode =
  (* aka TeX mode *)
  | INITIAL
  (* started with begin{verbatim} finished by end{verbatim} *)
  | IN_VERBATIM
  (* started with \verb+, finished by +, or another special char *)
  | IN_VERB of char
  (* started with <<xxx>>=, finished by @ *)
  | IN_NOWEB_CHUNK

let default_state = INITIAL

let _mode_stack =
  ref [default_state]

let reset () =
  _mode_stack := [default_state];
  ()

let rec current_mode () =
  try
    Common2.top !_mode_stack
  with Failure("hd") ->
    pr2("LEXER: mode_stack is empty, defaulting to INITIAL");
    reset();
    current_mode ()

let push_mode mode = Common.push mode _mode_stack
let pop_mode () = Common2.pop2 _mode_stack |> ignore

}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['a'-'z''A'-'Z']
let digit = ['0'-'9']

(*****************************************************************************)
(* Rule in TeX *)
(*****************************************************************************)
rule tex = parse
  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "%" [^'\n' '\r']* {
      TComment(tokinfo lexbuf)
    }
  (* Actually in TeX the space and newlines have a meaning so I should perhaps
   * rename those tokens.
   *)
  | [' ''\t'] { TCommentSpace (tokinfo lexbuf) }
  | "\n" { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
  | "{" { TOBrace (tokinfo lexbuf); }
  | "}" { TCBrace (tokinfo lexbuf); }

  | '[' { TOBracket (tokinfo lexbuf) }
  | ']' { TCBracket (tokinfo lexbuf) }
  (* they don't have to be matching in TeX, so no need for special tokens *)
  | '(' { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | ')' { TSymbol (tok lexbuf, tokinfo lexbuf) }

  (* don't want ~\foo to be tokenized as ~\ *)
  | "~" { TSymbol (tok lexbuf, tokinfo lexbuf) }

  (* bugfix: '\\' can not be in the list below otherwise text like ''foo''\xxx
   * will not parse \xxx as a command but instead ''\ as a symbol
   *)
  | ['-' '+' '=' '\'' '.' '@' ',' '/' ':' '<' '>' '*' ';' '#' '"'
     '_' '`' '?' '^' '|' '!' '&' ]+ {
      TSymbol (tok lexbuf, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Commands and words (=~ Keywords and indent in other PL) *)
  (* ----------------------------------------------------------------------- *)
  (* very pad-specific, for noweblatexpad, todo, less, note shortcuts *)
  | "\\" (['t''l''n'] as kind) [' ''\t'] [^'\n' '\r']*
      { TFootnote (kind, tokinfo lexbuf) }

  | "\\" ((letter (letter | '*')*) as cmd) { TCommand (cmd, tokinfo lexbuf)}
  | letter+ { TWord(tok lexbuf, tokinfo lexbuf) }

  | "\\\\" { TSymbol (tok lexbuf, tokinfo lexbuf) }
  | "\\" { TSymbol (tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
  | digit+ ("pt" | "cm" | "px") { TUnit(tok lexbuf, tokinfo lexbuf) }

  | digit+ { TNumber(tok lexbuf, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Noweb *)
  (* ----------------------------------------------------------------------- *)

  | "<<" ([^'>']+ as _tagname) ">>=" {
      push_mode IN_NOWEB_CHUNK;
      TBeginNowebChunk (tokinfo lexbuf)
    }

  | "[[" ([^'\n' '\r' ']']+ as str) "]]" {
      TNowebCode (str, tokinfo lexbuf);
    }
  (* syncweb: *)
  | "[<" ([^'\n' '\r' '>']+ as str) ">]" {
      TNowebCodeLink (str, tokinfo lexbuf);
    }

  (* ----------------------------------------------------------------------- *)
  (* Special modes *)
  (* ----------------------------------------------------------------------- *)
  | "\\begin{verbatim}"
      {
        push_mode (IN_VERBATIM);
        TBeginVerbatim (tokinfo lexbuf)
      }
  | "\\verb" (_ as c)
      {
        push_mode (IN_VERB c);
        TBeginVerbatim (tokinfo lexbuf)
      }


  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf |> Parse_info.rewrap_str "") }
  | _ {
        error ("unrecognised symbol, in token rule:"^tok lexbuf) lexbuf;
        TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule in Code noweb *)
(*****************************************************************************)
and noweb = parse
  | "\n@" {
      pop_mode ();
      TEndNowebChunk (tokinfo lexbuf)
    }
  (* less: they should be alone on their line, with space and newline after *)
  | "<<" ([^'\n' '\r']+ as name) ">>" {
      TNowebChunkName (name, tokinfo lexbuf);
    }
  | ([^'\n''<']+ as str) { TNowebChunkStr (str, tokinfo lexbuf) }
  | '\n' { TCommentNewline (tokinfo lexbuf) }
  | '<'  { TNowebChunkStr ("<", tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf |> Parse_info.rewrap_str "") }
  | _ {
      error ("unrecognised symbol, in noweb chunkname rule:"^tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }


(*****************************************************************************)
(* Rule in verbatim *)
(*****************************************************************************)
and verbatim = parse
  | "\\end{verbatim}" {
      pop_mode ();
      TEndVerbatim (tokinfo lexbuf)
    }
  (* note: if end{verbatim} is not alone on its line then
   * this regexp will take precedence because of the longest-match
   * behavior of lex. So keep \end{verbatim} alone on its line!
   *)
  | ([^'\n']+ as line) { TVerbatimLine (line, tokinfo lexbuf) }
  | '\n' { TCommentNewline (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  | eof { EOF (tokinfo lexbuf |> Parse_info.rewrap_str "") }
  | _ {
      error ("unrecognised symbol, in verbatim rule:"^tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }

and verb c = parse
  | _ as c2 {
      if c = c2 then begin
        pop_mode ();
        TEndVerbatim (tokinfo lexbuf)
      end else (TVerbatimLine (spf "%c" c, tokinfo lexbuf))
    }
  | eof { EOF (tokinfo lexbuf |> Parse_info.rewrap_str "") }
