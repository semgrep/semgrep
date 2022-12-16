{
(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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

module Flag = Flag_parsing
open Parser_skip

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A lexer for Skip.
 *
 * reference:
 *  - https://github.com/skiplang/skip/blob/master/docs/specification/Lexical-Structure.md
 *    but not up to date
 *  - skip/src/frontend/keywords.sk
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z']
let digit  = ['0'-'9']
let hexa = digit | ['A' 'F' 'a' 'f']
(* could add ['\n' '\r'], or just use dos2unix on your files *)
let newline = '\n'
let space = [' ' '\t']

let lowerletter = ['a'-'z']
let upperletter = ['A'-'Z']

let ident      = (lowerletter | '_') (letter | digit | '_')*
let upperident = upperletter (letter | digit | '_')*


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

  (* don't keep the trailing \n; it will be in another token *)
  | "//" [^'\n']* { TComment (tokinfo lexbuf) }
  | "/*" {
      let info = tokinfo lexbuf in
      let com = comment lexbuf in
      TComment(info |> Parse_info.tok_add_s com)
    }

  | newline { TCommentNewline (tokinfo lexbuf) }
  | space+ { TCommentSpace (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)
  (* operator-or-punctuator:: one of
    =  +  -  *  /  %  .  ->  ~>  =>  {  }  (  )  [  ]  ;  :  ::  ,
    ^  |  !  ||  &&  ==  !=  <  <=  >  >=  =.  &
  *)
  | "="  { TEq (tokinfo lexbuf) }

  | "+" { TPlus(tokinfo lexbuf) }  | "-" { TMinus(tokinfo lexbuf) }
  | "*" { TStar(tokinfo lexbuf) } | "/" { TDiv(tokinfo lexbuf) }
  | "%" { TMod(tokinfo lexbuf) }

  | "." { TDot(tokinfo lexbuf) }
  | "->" { TArrow(tokinfo lexbuf) }
  | "~>" { TTildeArrow(tokinfo lexbuf) }
  | "=>" { TEqualArrow(tokinfo lexbuf) }

  | "{" { TOBrace(tokinfo lexbuf) }  | "}" { TCBrace(tokinfo lexbuf) }
  | "(" { TOParen(tokinfo lexbuf) }  | ")" { TCParen(tokinfo lexbuf) }
  | "[" { TOBracket(tokinfo lexbuf) }  | "]" { TCBracket(tokinfo lexbuf) }

  | ";" { TSemiColon(tokinfo lexbuf) }
  | ":" { TColon(tokinfo lexbuf) }
  | "::" { TColonColon(tokinfo lexbuf) }
  | "," { TComma(tokinfo lexbuf) }

  | "^" { THat(tokinfo lexbuf) }
  | "|" { TPipe(tokinfo lexbuf) }
  | "!" { TBang(tokinfo lexbuf) }

  | "||" { TPipePipe(tokinfo lexbuf) }
  | "&&" { TAndAnd(tokinfo lexbuf) }

  | "==" { TEqEq(tokinfo lexbuf) }
  | "!=" { TBangEq(tokinfo lexbuf) }
  | "<" { TLess(tokinfo lexbuf) }  | ">" { TGreater(tokinfo lexbuf) }
  | "<=" { TLessEq(tokinfo lexbuf) }  | ">=" { TGreaterEq(tokinfo lexbuf) }
  | "=." { TEqDot(tokinfo lexbuf) }

  | "&" { TAnd(tokinfo lexbuf) }

  (* not in reference *)
  | "?" { TQuestion(tokinfo lexbuf) }
  | "@" { TAt(tokinfo lexbuf) }
  | "#" { TSharp(tokinfo lexbuf) }
  | "$" { TDollar(tokinfo lexbuf) }
  | "`" { TBackquote(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  | ident as id {
      let ii = tokinfo lexbuf in
      match id with
      | "alias" -> Talias ii
      | "as" -> Tas ii
      | "async" -> Tasync ii
      | "await" -> Tawait ii
      | "catch" -> Tcatch ii
      | "children" -> Tchildren ii
      | "class" -> Tclass ii
      | "const" -> Tconst ii
      | "else" -> Telse ii
      | "extends" -> Textends ii
      | "final" -> Tfinal ii
      | "from" -> Tfrom ii
      | "fun" -> Tfun ii
      | "if" -> Tif ii
      | "match" -> Tmatch ii
      | "module" -> Tmodule ii
      | "mutable" -> Tmutable ii
      | "native" -> Tnative ii
      | "private" -> Tprivate ii
      | "protected" -> Tprotected ii
      | "uses" -> Tuses ii
      | "static" -> Tstatic ii
      | "this" -> Tthis ii (* conditional? *)
      | "throw" -> Tthrow ii
      | "trait" -> Ttrait ii
      | "try" -> Ttry ii
      | "type" -> Ttype ii
      | "void" -> Tvoid ii
      | "watch" -> Twatch ii
      | "when" -> Twhen ii
      | "with" -> Twith ii

      (* conditional keywords *)
      | "base" -> Tbase ii
      | "capture" -> Tcapture ii
      | "default" -> Tdefault ii
      | "deferred" -> Tdeferred ii
      | "untracked" -> Tuntracked ii

      | "inst" -> Tinst ii
      | "nonNullable" -> TnonNullable ii
      | "value" -> Tvalue ii

      (* not in reference *)
      | "true" -> Ttrue ii
      | "false" -> Tfalse ii
      | "overridable" -> Toverridable ii
      | "memoized" -> Tmemoized ii
      | "frozen" -> Tfrozen ii
      | "readonly" -> Treadonly ii
      | "macro" -> Tmacro ii
      | "extension" -> Textension ii
      | "in" -> Tin ii
      | "for" -> Tfor ii
      | "do" -> Tdo ii
      | "while" -> Twhile ii
      | "loop" -> Tloop ii
      | "yield" -> Tyield ii
      | "break" -> Tbreak ii
      | "continue" -> Tcontinue ii


      | _ -> TLowerIdent (id, ii)
    }

  | upperident as id {
      TUpperIdent (id, tokinfo lexbuf)
    }

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
   {
     let s = tok lexbuf in
     TInt (s, tokinfo lexbuf)
   }

  | '-'?
    digit (digit | '_')*
    ('.' (digit | '_')*)?
    ( ('e' |'E') ['+' '-']? digit (digit | '_')* )?
     {
     let s = tok lexbuf in
     TFloat (s, tokinfo lexbuf)
    }


  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | '"' {
     (* opti: use Buffer because some generated files can contain big strings*)
      let info = tokinfo lexbuf in
      let buf = Buffer.create 100 in
      string buf lexbuf;
      let s = Buffer.contents buf in
      TString (s, info |> Parse_info.tok_add_s (s ^ "\""))
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
      | '\\' digit+
      | '\\' 'x' hexa+
      | '\\' 'u' hexa+
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
(* we need to use a Buffer to parse strings as some autogenerated
 * files can contain big strings with many \\ characters
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


(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
and comment = parse
  | "*/" { tok lexbuf }

  | [^'*''/']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | "*"     { let s = tok lexbuf in s ^ comment lexbuf }
  | "/"     { let s = tok lexbuf in s ^ comment lexbuf }
  | eof {
      error "end of file in comment" lexbuf;
      "*/"
    }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s) lexbuf;
      s ^ comment lexbuf
    }
