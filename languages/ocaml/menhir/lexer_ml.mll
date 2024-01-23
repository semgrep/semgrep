{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2012 Facebook
 * Copyright (C) 2020 R2C
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
module Ast = AST_ocaml
module Flag = Flag_parsing

open Parser_ml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* src: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
(* src: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html *)
let keyword_table = Hashtbl_.hash_of_list [

  "fun", (fun ii -> Tfun ii);
  "function", (fun ii -> Tfunction ii);
  "rec", (fun ii -> Trec ii);

  "type", (fun ii -> Ttype ii);
  "of", (fun ii -> Tof ii);

  "if", (fun ii -> Tif ii);
  "then", (fun ii -> Tthen ii);
  "else", (fun ii -> Telse ii);

  "match", (fun ii -> Tmatch ii);
  "with", (fun ii -> Twith ii);
  "when", (fun ii -> Twhen ii);

  "let", (fun ii -> Tlet ii);
  "in", (fun ii -> Tin ii);

  "as", (fun ii -> Tas ii);

  "try", (fun ii -> Ttry ii);
  "exception", (fun ii -> Texception ii);

  "begin", (fun ii -> Tbegin ii);
  "end", (fun ii -> Tend ii);

  "for", (fun ii -> Tfor ii);
  "do", (fun ii -> Tdo ii);
  "done", (fun ii -> Tdone ii);
  "downto", (fun ii -> Tdownto ii);
  "while", (fun ii -> Twhile ii);
  "to", (fun ii -> Tto ii);

  "val", (fun ii -> Tval ii);
  "external", (fun ii -> Texternal ii);

  "true", (fun ii -> Ttrue ii);
  "false", (fun ii -> Tfalse ii);

  "module", (fun ii -> Tmodule ii);
  "open", (fun ii -> Topen ii);
  "functor", (fun ii -> Tfunctor ii);
  "include", (fun ii -> Tinclude ii);
  "sig", (fun ii -> Tsig ii);
  "struct", (fun ii -> Tstruct ii);

  "class", (fun ii -> Tclass ii);
  "new", (fun ii -> Tnew ii);
  "inherit", (fun ii -> Tinherit ii);
  "constraint", (fun ii -> Tconstraint ii);
  "initializer", (fun ii -> Tinitializer ii);
  "method", (fun ii -> Tmethod ii);
  "object", (fun ii -> Tobject ii);
  "private", (fun ii -> Tprivate ii);
  "virtual", (fun ii -> Tvirtual ii);

  "lazy", (fun ii -> Tlazy ii);
  "mutable", (fun ii -> Tmutable ii);
  "assert", (fun ii -> Tassert ii);

   (* used when doing mutually recursive definitions *)
  "and", (fun ii -> Tand ii);

  (* was INFIXOP3 and INFIXOP4 in original grammar apparently *)
  "or", (fun ii -> Tor ii);
  "mod", (fun ii -> Tmod ii);
  "lor", (fun ii -> Tlor ii);
  "lsl", (fun ii -> Tlsl ii);
  "lsr", (fun ii -> Tlsr ii);
  "lxor", (fun ii -> Tlxor ii);
  "asr", (fun ii -> Tasr ii);
  "land", (fun ii -> Tland ii);

  (* "ref" is not a keyword ? it's actually a function returning
   * a {mutable contents = ...}
   *)
]

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']
let hexa = digit | ['A'-'F'] | ['a'-'f']
(* could add ['\n' '\r'], or just use dos2unix on your files *)
let newline = '\n'
let space = [' ' '\t']

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']

let ident      = (lowerletter | '_') (letter | digit | '_' | "'")*
let upperident = upperletter (letter | digit | '_' | "'")*
let label_name = (lowerletter | '_') (letter | digit | '_' | "'")*

let operator_char =
 '!'| '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/'
  | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~'
let prefix_symbol =
  ('!' | '?' | '~') operator_char*
let infix_symbol =
  ('=' | '<' | '>' | '@' | '^' | '|'| '&' | '+' | '-' | '*'| '/' | '$'|'%' )
   operator_char*

(* for let/and operators *)
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|']
let kwdopchar =
  ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|']

(*****************************************************************************)
(* Rule token *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generate tokens for comments, so you can not
   * give this lexer as-is to the parsing function. You must have an
   * intermediate layer that filter those tokens.
   *)

  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }
  | "(*" {
      let info = tokinfo lexbuf in
      let com = comment lexbuf in
      TComment(info |> Tok.tok_add_s com)
    }

  (* ext: fsharp *)
  | "///" [^ '\n']* { TComment (tokinfo lexbuf) }

  | "#" space* digit+ space* ("\"" [^ '"']* "\"")?
      { TCommentMisc (tokinfo lexbuf) }

  (* just doing "#"[^'\n']+  can also match method calls. Would be good
   * to enforce in first column but ocamllex can't do that natively
   *)
  | "#!"[^'\n']+
      { TSharpDirective (tokinfo lexbuf) }

  (* todo: hmmm but could be ambiguous with method call too ? *)
  | "#load" space+ [^'\n']+
      { TSharpDirective (tokinfo lexbuf) }
  | "#directory" space+ [^'\n']+
      { TSharpDirective (tokinfo lexbuf) }

  (* many people use sometimes -pp cpp so let's support simple cpp idioms *)
  | "#if" [^ '\n']* { TCommentMisc (tokinfo lexbuf) }
  | "#else" [^ '\n']* { TCommentMisc (tokinfo lexbuf) }
  | "#endif" [^ '\n']* { TCommentMisc (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (*
   * !=    #     &     &&    '     (     )     *     +     ,     -
   * -.    ->    .     ..    :     ::    :=    :>    ;     ;;    <
   * <-    =     >     >]    >}    ?     ??    [     [<    [>    [|
   * ]     _     `     {     {<    |     |]    }     ~
   *)

  | "="  { TEq (tokinfo lexbuf) }

  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }
  | "[<" { TOBracketLess(tokinfo lexbuf) }  | ">]" { TGreaterCBracket(tokinfo lexbuf) }
  | "[|" { TOBracketPipe(tokinfo lexbuf) }  | "|]" { TPipeCBracket(tokinfo lexbuf) }
  | "{<" { TOBraceLess(tokinfo lexbuf) }  | ">}" { TGreaterCBrace(tokinfo lexbuf) }

  | "[>" { TOBracketGreater(tokinfo lexbuf) }

  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TGreater(tokinfo lexbuf) }

  | "!=" { TBangEq(tokinfo lexbuf) (* INFIXOP0 *) }

  | "#" { TSharp(tokinfo lexbuf) }
  | "&" { TAnd(tokinfo lexbuf) }
  | "&&" { TAndAnd(tokinfo lexbuf) }

  (* also used as beginning of a char *)
  | "'" { TQuote(tokinfo lexbuf) }

  | "`" { TBackQuote(tokinfo lexbuf) }

  | "*" { TStar(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }
  | "->" { TArrow(tokinfo lexbuf) }
  | "." { TDot(tokinfo lexbuf) }
  | ".." { TDotDot(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | "::" { TColonColon(tokinfo lexbuf) }
  | ";" { TSemiColon(tokinfo lexbuf) }
  | ";;" { TSemiColonSemiColon(tokinfo lexbuf) }
  | "?" { TQuestion(tokinfo lexbuf) }
  | "??" { TQuestionQuestion(tokinfo lexbuf) }
  | "_" { TUnderscore(tokinfo lexbuf) }
  | "|" { TPipe(tokinfo lexbuf) }
  | "~" { TTilde(tokinfo lexbuf) }

  | ":=" { TAssign (tokinfo lexbuf) }
  | "<-" { TAssignMutable (tokinfo lexbuf) }

  | ":>" { TColonGreater(tokinfo lexbuf) }

  (* why -. is mentionned in grammar as a keyword and not +. ? *)
  | "-." { TMinusDot(tokinfo lexbuf) }

  | "+." { TPlusDot(tokinfo lexbuf) }

  (* why ! is not mentionned as a keyword ? *)
  | "!" { TBang(tokinfo lexbuf) }

  (* attributes *)
  | "[@" { TBracketAt(tokinfo lexbuf) }
  | "[@@"  { TBracketAtAt(tokinfo lexbuf) }
  | "[@@@" { TBracketAtAtAt(tokinfo lexbuf) }
  | "[%"   { TBracketPercent(tokinfo lexbuf) }
  | "[%%"  { TBracketPercentPercent(tokinfo lexbuf) }

  | prefix_symbol { TPrefixOperator (tok lexbuf, tokinfo lexbuf) }
  | infix_symbol { TInfixOperator (tok lexbuf, tokinfo lexbuf) }
  (* pad: used in js_of_ocaml, not sure why not part of infix_symbol *)
  | "##" { TInfixOperator (tok lexbuf, tokinfo lexbuf) }

  (* since 4.08 *)
  | "let" kwdopchar dotsymbolchar * as op
            { LETOP (op, tokinfo lexbuf) }
  | "and" kwdopchar dotsymbolchar * as op
            { ANDOP (op, tokinfo lexbuf) }

  (* camlp4 reserved:
   * parser    <<    <:    >>    $     $$    $:
   *)

  (* sgrep-ext: *)
  | "..." { Flag_parsing.sgrep_guard (TDots (tokinfo lexbuf)) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }


  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident {
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> TLowerIdent (s, info)
    }
  (* sgrep-ext: *)
  (* We need a way to differentiate metavariables for:
   *  - uppercase things (module names, constructors, exceptions)
   *  - lowercase things (entity names, parameters, full expression/types).
   * The OCaml grammar relies on the first letter of the identifier
   * to resolve ambiguities between those two big categories, so we
   * need to reproduce this with metavariables.
   *
   * Ideally we would use $XX for uppercase stuff, and $xx for lowercase
   * stuff, but this would not be consistent with what we do for the
   * other languages (we opted for $XXX and not $xxx because of possible
   * ambiguities in languages like PHP which already use $xxx for variables).
   *
   * TODO: right now it's ugly, if it's $X_... then it's considered
   * an uppercase metavariable. We should use $XXX and $xx but then
   * we need to revisit lots of code like Metavariable.metavar_regexp
   *)
  | '$' upperletter '_' (upperletter | '_' | digit)*
     { Flag.sgrep_guard (TUpperIdent (tok lexbuf, tokinfo lexbuf)) }
  | '$' (upperletter | '_') (upperletter | '_' | digit)*
     { Flag.sgrep_guard (TLowerIdent (tok lexbuf, tokinfo lexbuf)) }
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (TLowerIdent (tok lexbuf, tokinfo lexbuf)) }


  | upperident as s           { TUpperIdent (s, tokinfo lexbuf) }
  (* less: should split in different tokens, like in tree-sitter-ocaml *)
  | '~' (label_name as s) ':' { TLabelDecl (s, tokinfo lexbuf)  }
  | '?' (label_name as s) ':' { TOptLabelDecl (s, tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | '-'? digit
        (digit | '_')*
  | '-'? ("0x" | "0X") (digit | ['A' 'F' 'a' 'f'])
                       (digit | ['A' 'F' 'a' 'f'] | '_')*
  | '-'? ("0o" | "0O")   ['0'-'7']
                       ( ['0'-'7'] | '_')*
  | '-'? ("0b" | "0B")   ['0'-'1']
                       ( ['0'-'1'] | '_')*
   { let s = tok lexbuf in
     let t = tokinfo lexbuf in
     TInt (Parsed_int.parse (s, t))
   }

  | '-'?
    digit (digit | '_')*
    ('.' (digit | '_')*)?
    ( ('e' |'E') ['+' '-']? digit (digit | '_')* )?
    { let s = tok lexbuf in
      TFloat (float_of_string_opt s, tokinfo lexbuf)
    }


  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | '"' {
     (* opti: use Buffer because some generated mlfiles contain huge strings *)
      let info = tokinfo lexbuf in
      let buf = Buffer.create 100 in
      string buf lexbuf;
      let s = Buffer.contents buf in
      TString (s, info |> Tok.tok_add_s (s ^ "\""))
    }

  (* new feature I discovered thx to McPeak.
   * I just support {| |} not the one with also arbitrary delimiters
   *)
  | "{|" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 100 in
      quoted_string buf lexbuf;
      let s = Buffer.contents buf in
      TString (s, info |> Tok.tok_add_s (s ^ "|}"))
  }
  (* ----------------------------------------------------------------------- *)
  (* Chars *)
  (* ----------------------------------------------------------------------- *)

  | "'" (_ as c) "'" {
      TChar (String.make 1 c, tokinfo lexbuf)
    }

  | "'"
    (
        '\\' ( '\\' | '"' | "'" | 'n' | 't' | 'b' | 'r')
      | '\\' digit digit digit
      | '\\' 'x' hexa hexa
    )
    "'"
   {
      let s = tok lexbuf in
      TChar (s, tokinfo lexbuf)
    }

  | "'" "\\" _ {
      error ("unrecognised escape, in token rule:"^tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)

  | eof { EOF (tokinfo lexbuf) }

  | _ {
      error ("unrecognised symbol, in token rule:"^tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
(* we need to use a Buffer to parse strings as lex and yacc autogenerated
 * ml files contains big strings with many \\ characters
 *)

and string buf = parse
  | '"'           { () }
  (* opti: *)
  | [^ '"' '\\']+ {
      Buffer.add_string buf (tok lexbuf);
      string buf lexbuf
    }

  | ("\\" (_ as v)) as x {
      (* todo: check char ? *)
      (match v with
      | _ -> ()
      );
      Buffer.add_string buf x;
      string buf lexbuf
    }
  | eof { error "WEIRD end of file in double quoted string" lexbuf }

and quoted_string buf = parse
  | "|}" { () }
  (* opti: *)
  | [^ '|' '}']+ {
      Buffer.add_string buf (tok lexbuf);
      quoted_string buf lexbuf
    }

  | ['|' '}'] as x {
      Buffer.add_char buf x;
      quoted_string buf lexbuf
    }
  | eof { error "WEIRD end of file in quoted string" lexbuf }


(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
and comment = parse
  | "*)" { tok lexbuf }

  | "(*" {
      (* in ocaml comments are nestable *)
      let s = comment lexbuf in
      "(*" ^ s ^ comment lexbuf
    }

  (* noteopti: bugfix, need add '(' too *)

  | [^'*''(']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | "*"     { let s = tok lexbuf in s ^ comment lexbuf }
  | "("     { let s = tok lexbuf in s ^ comment lexbuf }
  | eof {
      error "end of file in comment" lexbuf;
      "*)"
    }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s) lexbuf;
      s ^ comment lexbuf
    }
