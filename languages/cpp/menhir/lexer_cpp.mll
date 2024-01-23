{
(* Yoann Padioleau
 *
 * Copyright (C) 2002      Yoann Padioleau
 * Copyright (C) 2006-2007 Ecole des Mines de Nantes
 * Copyright (C) 2008-2009 University of Urbana Champaign
 * Copyright (C) 2010-2013 Facebook
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

open Parser_cpp
open Ast_cpp (* to factorise tokens with OpAssign, ... *)

module Flag = Flag_parsing
module Flag_cpp = Flag_parsing_cpp
module Ast = Ast_cpp

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The C/cpp/C++ lexer.
 *
 * This lexer generates tokens for C (int, while, ...), C++ (new, delete, ...),
 * and CPP (#define, #ifdef, ...).
 * It also generate tokens for comments and spaces. This means that
 * it can not be used as-is. Some post-filtering
 * has to be done to feed it to a parser.
 *
 * Note that C and C++ are not context-free languages and so some idents
 * must be disambiguated in some ways. TIdent below must thus be
 * post-processed too (as well as other tokens like '<' for C++).
 * See parsing_hack.ml for examples.
 *
 * note: We can't use Lexer_parser._lexer_hint here to do different
 * things because we now call the lexer to get all the tokens
 * and then only we parse. So we can use the hint only
 * in parse_cpp.ml. For the same reason, we don't handle typedefs
 * here anymore. We really just tokenize ...
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error
let tok_add_s = Tok.tok_add_s

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)

(* opti: less convenient, but using a hash is faster than using a match *)
let keyword_table = Hashtbl_.hash_of_list [

  "void",   (fun ii -> Tvoid ii);
  "char",   (fun ii -> Tchar ii);
  "short",  (fun ii -> Tshort ii); "int",    (fun ii -> Tint ii);
  "long",   (fun ii -> Tlong ii);
  "float",  (fun ii -> Tfloat ii); "double", (fun ii -> Tdouble ii);

  (* c++ext: *)
  "bool", (fun ii -> Tbool ii);
  "true", (fun ii -> Ttrue ii); "false", (fun ii -> Tfalse ii);

  (* tree-sitter-c: *)
  "TRUE", (fun ii -> Ttrue ii); "FALSE", (fun ii -> Tfalse ii);
  "NULL", (fun ii -> Tnull ii);

  "wchar_t", (fun ii -> Twchar_t ii);
  (* c++0x: TODO *)
  "char16_t", (fun ii -> Twchar_t ii); "char32_t", (fun ii -> Twchar_t ii);
  "nullptr", (fun ii -> Tnullptr ii);

  "unsigned", (fun ii -> Tunsigned ii); "signed",   (fun ii -> Tsigned ii);

  (* c: and also now a c++0x: *)
  "auto",     (fun ii -> Tauto ii);

  "register", (fun ii -> Tregister ii);
  "extern",   (fun ii -> Textern ii);
  "static",   (fun ii -> Tstatic ii);

  "const",    (fun ii -> Tconst ii);
  "volatile", (fun ii -> Tvolatile ii);

  (* c99:  *)
  "__restrict__",    (fun ii -> Trestrict ii);
  (* gccext: and also a c++ext: *)
  "inline",     (fun ii -> Tinline ii);
  (* c++ext:  *)
  "friend"    , (fun ii -> Tfriend ii);
  "explicit", (fun ii -> Texplicit ii);
  "mutable", (fun ii -> Tmutable ii);
  "virtual", (fun ii -> Tvirtual ii);

  (* virtual specifier *)
  "final", (fun ii -> Tfinal ii);
  "override", (fun ii -> Toverride ii);

  (* c++0x:  *)
  "constexpr", (fun ii -> Tconstexpr ii);
  "thread_local", (fun ii -> Tthread_local ii);

  "struct",  (fun ii -> Tstruct ii);
  "union",   (fun ii -> Tunion ii);
  "enum",    (fun ii -> Tenum ii);

  "typedef", (fun ii -> Ttypedef ii);

  "if",      (fun ii -> Tif ii); "else",     (fun ii -> Telse ii);
  "break",   (fun ii -> Tbreak ii); "continue", (fun ii -> Tcontinue ii);
  "switch",  (fun ii -> Tswitch ii);
  "case",     (fun ii -> Tcase ii); "default", (fun ii -> Tdefault ii);
  "for",     (fun ii -> Tfor ii);
  "do",      (fun ii -> Tdo ii);
  "while",   (fun ii -> Twhile ii);
  "return",  (fun ii -> Treturn ii);
  "goto",    (fun ii -> Tgoto ii);

  "sizeof", (fun ii -> Tsizeof ii);
  (* gccext: *)
  "typeof", (fun ii -> Ttypeof ii);

  (* c++ext: *)
  "typename" ,   (fun ii -> Ttypename ii);
  (* c++0x: *)
  "decltype" ,   (fun ii -> Tdecltype ii);

  (* gccext: more (cpp) aliases are in macros.h *)
  "asm",     (fun ii -> Tasm ii);
  "__attribute__", (fun ii -> Tattribute ii);

  (* c++ext: see also TH.is_cpp_keyword *)
  "class", (fun ii -> Tclass ii);
  "this", (fun ii -> Tthis ii);

  "new"    ,   (fun ii -> Tnew ii);
  "delete" ,   (fun ii -> Tdelete ii);

  "template" ,   (fun ii -> Ttemplate ii);
  "typeid"   ,   (fun ii -> Ttypeid ii);

  "catch" , (fun ii -> Tcatch ii);
  "try"   , (fun ii -> Ttry ii);
  "throw" , (fun ii -> Tthrow ii);

  "operator", (fun ii -> Toperator ii);

  "public"    , (fun ii -> Tpublic ii);
  "private"   , (fun ii -> Tprivate ii);
  "protected" , (fun ii -> Tprotected ii);

  "namespace", (fun ii -> Tnamespace ii);
  "using", (fun ii -> Tusing ii);

  "const_cast"       , (fun ii -> Tconst_cast ii);
  "dynamic_cast"     , (fun ii -> Tdynamic_cast ii);
  "static_cast"      , (fun ii -> Tstatic_cast ii);
  "reinterpret_cast" , (fun ii -> Treinterpret_cast ii);

 ]

let error_radix s =
  ("numeric " ^ s ^ " constant contains digits beyond the radix:")

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let letter = ['A'-'Z' 'a'-'z' '_']
let digit  = ['0'-'9']

(* not used for the moment *)
let punctuation = ['!' '"' '#' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':'
		   ';' '<' '=' '>' '?' '[' '\\' ']' '^' '{' '|' '}' '~']
let space = [' ' '\t' '\n' '\r' '\011' '\012' ]
let additionnal = [ ' ' '\b' '\t' '\011' '\n' '\r' '\007' ]
(* 7 = \a = bell in C. this is not the only char allowed !!
 * ex @ and $ ` are valid too
 *)

let cchar = (letter | digit | punctuation | additionnal)

let sp = [' ' '\t']+
let spopt = [' ' '\t']*

let dec = ['0'-'9']
let oct = ['0'-'7']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal = ('0' | (['1'-'9'] dec*))
let octal   = ['0']        oct+
let hexa    = ("0x" |"0X") hex+

let pent   = dec+
let pfract = dec+
let sign = ['-' '+']
let exp  = ['e''E'] sign? dec+
let real = pent exp | ((pent? '.' pfract | pent '.' pfract? ) exp?)

let id = letter (letter | digit) *

(*****************************************************************************)
(* Rule token *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* Spaces, comments *)
  (* ----------------------------------------------------------------------- *)

  (* note: this lexer generate tokens for comments! So we can not give
   * this lexer as-is to the parsing function. We must postprocess it, and
   * use techniques like cur_tok ref in parse_cpp.ml
   *)

  | [' ' '\t' ]+
      { TCommentSpace (tokinfo lexbuf) }

  (* see also TCppEscapedNewline below *)
  | [ '\n' '\r' '\011' '\012']
      { TCommentNewline (tokinfo lexbuf) }

  | "/*"
      { let info = tokinfo lexbuf in
        let com = comment lexbuf in
        TComment(info |> tok_add_s com)
      }

  (* C++ comments are allowed via gccext, but normally they are deleted by cpp.
   * So we need this here only because we dont call cpp before.
   * Note that we don't keep the trailing \n; it will be in another token.
   *)
  | "//" [^'\r' '\n' '\011']*    { TComment (tokinfo lexbuf) }

  (* ---------------------- *)
  (* #include *)
  (* ---------------------- *)

  (* The difference between a local "" and standard <> include is computed
   * later in parser_cpp.mly. So we redo a little bit of lexing there. It's
   * ugly but simpler to generate a single token here. *)
  | (("#" [' ''\t']* ("include" | "include_next" | "import")
     [' ' '\t']*) as includes)
    (('"' ([^ '"']+) '"' |
     '<' [^ '>']+ '>' |
      (* semgrep: added $ *)
      ['A'-'Z''_''$']+
    ) as filename)
      { (* less: generate 2 info so highlight_cpp.ml can colorize the
         * directive and the filename differently
         *)
        TInclude (includes, filename, tokinfo lexbuf)
      }

  (* ---------------------- *)
  (* #ifdef *)
  (* ---------------------- *)

  | "#" [' ' '\t']* "if" [' ' '\t']* '0'   (* [^'\n']*  '\n' *)
      { let info = tokinfo lexbuf in
        TIfdefBool (false, info(* +> tok_add_s (cpp_eat_until_nl lexbuf)*))
      }
  | "#" [' ' '\t']* "if" [' ' '\t']* '1'   (* [^'\n']*  '\n' *)
      { let info = tokinfo lexbuf in
        TIfdefBool (true, info)
      }
  | "#" [' ' '\t']* "ifdef" [' ' '\t']* "__cplusplus"   [^'\n']*  '\n'
      { let info = tokinfo lexbuf in
        TIfdefMisc (false, info)
      }

  (* can have some ifdef 0  hence the letter|digit even at beginning of word *)
  | "#" [' ''\t']* "ifdef" [' ''\t']+ (letter|digit)((letter|digit)*) [' ''\t']*
      { TIfdef (tokinfo lexbuf) }
  | "#" [' ''\t']* "ifndef" [' ''\t']+ (letter|digit)((letter|digit)*)[' ''\t']*
      { TIfdef (tokinfo lexbuf) }
  | "#" [' ''\t']* "if" [' ' '\t']+
      { let info = tokinfo lexbuf in
        TIfdef (info |> tok_add_s (cpp_eat_until_nl lexbuf))
      }
  | "#" [' ' '\t']* "if" '('
      { let info = tokinfo lexbuf in
        TIfdef (info |> tok_add_s (cpp_eat_until_nl lexbuf))
      }

  | "#" [' ' '\t']* "elif" [' ' '\t']+
      { let info = tokinfo lexbuf in
        TIfdefelif (info |> tok_add_s (cpp_eat_until_nl lexbuf))
      }

  (* bugfix: can have #endif LINUX  but at the same time if I eat everything
   * until next line, I may miss some TComment which for some tools
   * are important such as aComment
   *)
  | "#" [' ' '\t']* "endif" (*[^'\n']* '\n'*)
      { TEndif     (tokinfo lexbuf) }
  | "#" [' ' '\t']* "else" [' ' '\t' '\n']
      { TIfdefelse (tokinfo lexbuf) }

  (* ---------------------- *)
  (* #define, #undef *)
  (* ---------------------- *)

  (* The rest of the lexing/parsing of #define is done in fix_tokens_define
   * where we parse all TCppEscapedNewline and finally generate a TDefEol
   *)
  | "#" [' ' '\t']* "define" { TDefine (tokinfo lexbuf) }

  (* note: in some cases we can have stuff after the ident as in #undef XXX 50,
   * but I currently don't handle it cos I think it's bad code.
   *)
  | (("#" [' ' '\t']* "undef" [' ' '\t']+) as _undef) (id as id)
     (* alt: |> tok_add_s (cpp_eat_until_nl lexbuf)) *)
      { TUndef (id, tokinfo lexbuf) }

  (* ---------------------- *)
  (* #define body *)
  (* ---------------------- *)

  (* We could generate separate tokens for #, ## and then extend
   * the grammar, but there can be ident in many different places, in
   * expression but also in declaration, in function name. So having 3 tokens
   * for an ident does not work well with how we add info in
   * ast_cpp.ml. So it's better to generate just one token, just one info,
   * even if have later to reanalyse those tokens and unsplit.
   *
   * less: do as in yacfe, generate multiple tokens for those constructs?
   *)

  | ((id as s)  "...")
      { TDefParamVariadic (s, tokinfo lexbuf) }

  (* cppext: string concatenation *)
  |  id   ([' ''\t']* "##" [' ''\t']* id)+
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* cppext: stringification
   * bugfix: this case must be after the other cases such as #endif
   * otherwise take precedent.
   *)
  |  "#" (*spopt*) id
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* cppext: gccext: ##args for variadic macro *)
  |  "##" [' ''\t']* id
      { let info = tokinfo lexbuf in
        TIdent (tok lexbuf, info)
      }

  (* only in define body normally *)
  | "\\" '\n' { TCppEscapedNewline (tokinfo lexbuf) }

  (* ---------------------- *)
  (* cpp pragmas *)
  (* ---------------------- *)

   (* bugfix: I want to keep comments so cant do a    sp [^'\n']+ '\n'
    * http://gcc.gnu.org/onlinedocs/gcc/Pragmas.html
    *)
  | "#" spopt "pragma"  sp [^'\n']*  '\n'
  | "#" spopt "ident"   sp  [^'\n']* '\n'
  | "#" spopt "line"    sp  [^'\n']* '\n'
  | "#" spopt "error"   sp  [^'\n']* '\n'
  | "#" spopt "warning" sp  [^'\n']* '\n'
  | "#" spopt "abort"   sp  [^'\n']* '\n'
      { TCppDirectiveOther (tokinfo lexbuf) }

  (* This appears only after calling cpp cpp, as in:
   *   # 1 "include/linux/module.h" 1
   * Because we handle cpp ourselves, why handle it here?
   * Why not ... also one could want to use our parser on
   * expanded files sometimes.
   *)
  | "#" sp pent sp  '"' [^ '"']* '"' (spopt pent)*  spopt '\n'
      { TCppDirectiveOther (tokinfo lexbuf) }

  (* ?? *)
  | "#" [' ' '\t']* '\n'
      { TCppDirectiveOther (tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* C symbols *)
  (* ----------------------------------------------------------------------- *)
  (* stdC:
   * ...   &&   -=   >=   ~   +   ;   ]
   * <<=   &=   ->   >>   %   ,   <   ^
   * >>=   *=   /=   ^=   &   -   =   {
   * !=    ++   <<   |=   (   .   >   |
   * %=    +=   <=   ||   )   /   ?   }
   *     --   ==   !    *   :   [
   * recent addition:    <:  :>  <%  %>
   * only at processing: %:  %:%: # ##
   *)

  | '[' { TOCro(tokinfo lexbuf) }   | ']' { TCCro(tokinfo lexbuf) }
  | '(' { TOPar(tokinfo lexbuf)   } | ')' { TCPar(tokinfo lexbuf)   }
  | '{' { TOBrace(tokinfo lexbuf) } | '}' { TCBrace(tokinfo lexbuf) }

  | '+' { TPlus(tokinfo lexbuf) }   | '*' { TMul(tokinfo lexbuf) }
  | '-' { TMinus(tokinfo lexbuf) }  | '/' { TDiv(tokinfo lexbuf) }
  | '%' { TMod(tokinfo lexbuf) }

  | "++"{ TInc(tokinfo lexbuf) }    | "--"{ TDec(tokinfo lexbuf) }

  | "="  { TEq(tokinfo lexbuf) }

  | "-=" { TAssign ((Minus, (tokinfo lexbuf)))}
  | "+=" { TAssign ((Plus, (tokinfo lexbuf)))}
  | "*=" { TAssign ((Mul, (tokinfo lexbuf)))}
  | "/=" { TAssign ((Div, (tokinfo lexbuf)))}
  | "%=" { TAssign ((Mod, (tokinfo lexbuf)))}
  | "&=" { TAssign ((And, (tokinfo lexbuf)))}
  | "|=" { TAssign ((Or, (tokinfo lexbuf))) }
  | "^=" { TAssign ((Xor, (tokinfo lexbuf)))}
  | "<<=" {TAssign ((DecLeft, (tokinfo lexbuf))) }
  | ">>=" {TAssign ((DecRight, (tokinfo lexbuf)))}

  | "==" { TEqEq(tokinfo lexbuf) }  | "!=" { TNotEq(tokinfo lexbuf) }
  | ">=" { TSupEq(tokinfo lexbuf) } | "<=" { TInfEq(tokinfo lexbuf) }
  (* c++ext: transformed in TInf_Template in parsing_hacks_cpp.ml *)
  | "<"  { TInf(tokinfo lexbuf) }   | ">"  { TSup(tokinfo lexbuf) }

  | "&&" { TAndLog(tokinfo lexbuf) } | "||" { TOrLog(tokinfo lexbuf) }
  | ">>" { TShr(tokinfo lexbuf) }    | "<<" { TShl(tokinfo lexbuf) }
  | "&"  { TAnd(tokinfo lexbuf) }    | "|" { TOr(tokinfo lexbuf) }
  | "^"  { TXor(tokinfo lexbuf) }
  | "->"   { TPtrOp(tokinfo lexbuf) }  | '.'  { TDot(tokinfo lexbuf) }
  | ','    { TComma(tokinfo lexbuf) }
  | ";"    { TPtVirg(tokinfo lexbuf) }
  | "?"    { TWhy(tokinfo lexbuf) }    | ":"   { TCol(tokinfo lexbuf) }
  | "!"    { TBang(tokinfo lexbuf) }   | "~"   { TTilde(tokinfo lexbuf) }


  | "<:" { TOCro(tokinfo lexbuf) } | ":>" { TCCro(tokinfo lexbuf) }
  | "<%" { TOBrace(tokinfo lexbuf) } | "%>" { TCBrace(tokinfo lexbuf) }

  (* c++ext: *)
  | "::" { TColCol(tokinfo lexbuf) }
  | "->*" { TPtrOpStar(tokinfo lexbuf) }   | ".*" { TDotStar(tokinfo lexbuf) }

  (* a valid C construct, also used by semgrep! *)
  | "..." { TEllipsis(tokinfo lexbuf) }
  (* semgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* C keywords and ident *)
  (* ----------------------------------------------------------------------- *)

  (* StdC: "must handle at least name of length > 509, but can
   * truncate to 31 when compare and truncate to 6 and even lowerise
   * in the external linkage phase"
   *)
  | letter (letter | digit) *
      { let info = tokinfo lexbuf in
        let s = tok lexbuf in
        Profiling.profile_code "C parsing.lex_ident" (fun () ->
          match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
          | Some f -> f info

           (* typedef_hack. note: now this is no more useful, cos
            * as we use tokens_all, we first parse then all as idents and
            * later transform some idents into typedefs. So this job is
            * now done in parse_cpp.ml.
            *
            * old:
            *    if Lexer_parser.is_typedef s
            *    then Ident_Typedef (s, info)
            *    else TIdent (s, info)
            *)
          | None -> TIdent (s, info)
        )
      }

  (* gccext: apparently gcc allows dollar in variable names. I've found such
   * things a few times in Linux and in glibc.
   * No need to look in keyword_table here; definitly a TIdent.
   * sgrep-ext: can use $X in sgrep too.
   *)
  | (letter | '$') (letter | digit | '$')*
      {
        let s = tok lexbuf in
        if not !Flag_parsing.sgrep_mode
        then error ("identifier with dollar: "  ^ s) lexbuf;
        TIdent (s, tokinfo lexbuf)
      }
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (TIdent (tok lexbuf, tokinfo lexbuf)) }


  (* ----------------------------------------------------------------------- *)
  (* C constant *)
  (* ----------------------------------------------------------------------- *)

  | "'"
      { let info = tokinfo lexbuf in
        let s = char lexbuf   in
        let t = info |> tok_add_s (s ^ "'") in
        TChar (s, t)
      }
  | '"'
      { let info = tokinfo lexbuf in
        let s = string lexbuf in
        let t = info |> tok_add_s (s ^ "\"") in
        TString (s, t)
      }
  (* wide character encoding, TODO L'toto' valid ? what is allowed ? *)
  | 'L' "'"
      { let info = tokinfo lexbuf in
        let s = char lexbuf   in
        let t = info |> tok_add_s (s ^ "'") in
        TChar (s,t)
      }
  | 'L' '"'
      { let info = tokinfo lexbuf in
        let s = string lexbuf in
        let t = info |> tok_add_s (s ^ "\"") in
        TString (s,t)
      }

  (* Take care of the order ? No because lex try the longest match. The
   * strange diff between decimal and octal constant semantic is not
   * understood too by refman :) refman:11.1.4, and ritchie.
   *)
  (* this is also part of the case below, but we specialize it here to use the
   * right int_of_string *)
  | "0" (oct+ as n)
     { TInt (Parsed_int.parse ( "0o" ^ n, tokinfo lexbuf)) }

  | (( decimal | hexa | octal)
        ( ['u' 'U']
        | ['l' 'L']
        | (['l' 'L'] ['u' 'U'])
        | (['u' 'U'] ['l' 'L'])
        | (['u' 'U'] ['l' 'L'] ['l' 'L'])
        | (['l' 'L'] ['l' 'L'])
        )?
    ) as x { TInt (Parsed_int.parse (x, tokinfo lexbuf)) }

  | (real ['f' 'F']) as x { TFloat ((float_of_string_opt x, tokinfo lexbuf)) }
  | (real ['l' 'L']) as x { TFloat ((float_of_string_opt x, tokinfo lexbuf)) }
  | real as x             { TFloat ((float_of_string_opt x, tokinfo lexbuf)) }

  | ['0'] ['0'-'9']+
      { error (error_radix "octal" ^ tok lexbuf) lexbuf;
        TUnknown (tokinfo lexbuf)
      }
  | ("0x" |"0X") ['0'-'9' 'a'-'z' 'A'-'Z']+
      { error (error_radix "hexa" ^ tok lexbuf) lexbuf;
        TUnknown (tokinfo lexbuf)
      }

 (* !put after other rules! otherwise 0xff will be parsed as an ident *)
  | ['0'-'9']+ letter (letter | digit) *
      { error ("ZARB integer_string, certainly a macro:" ^ tok lexbuf) lexbuf;
        TUnknown (tokinfo lexbuf)
      }

(* gccext: http://gcc.gnu.org/onlinedocs/gcc/Binary-constants.html *)
(*
 | "0b" ['0'-'1'] { TInt (((tok lexbuf)<!!>(??,??)) |> int_of_stringbits) }
 | ['0'-'1']+'b' { TInt (((tok lexbuf)<!!>(0,-2)) |> int_of_stringbits) }
*)
  (*------------------------------------------------------------------------ *)
  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }

  | _ {
      error("unrecognised symbol, in token rule:" ^ tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule char *)
(*****************************************************************************)
and char = parse
(* c++ext: or firefoxext: unicode char may take multiple char as in 'MOSS'
  | (_ as x)                                    "'"  { String.make 1 x }

  (* todo?: as for octal, do exception  beyond radix exception ? *)
  | (("\\" (oct | oct oct | oct oct oct)) as x  "'") { x }
  (* this rule must be after the one with octal, lex try first longest
   * and when \7  we want an octal, not an exn.
   *)
  | (("\\x" (hex | hex hex)) as x        "'")      { x }
  | (("\\" (_ as v))           as x        "'")
	{
          (match v with (* Machine specific ? *)
          | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
          | 'f' -> () | 'a' -> ()
	  | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
          | 'e' -> () (* linuxext: ? *)
	  | _ ->
              error ("unrecognised symbol in char:"^tok lexbuf);
	  );
          x
	}
  | _
      { error ("unrecognised symbol in char:"^tok lexbuf);
        tok lexbuf
      }
*)
(* c++ext: mostly copy paste of string but s/"/'/  " and s/string/char *)
  | '\''                                      { "" }
  | (_ as x)
      { Common2.string_of_char x^char lexbuf}

  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ char lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ char lexbuf }
  | ("\\" (_ as v)) as x
       {
         (match v with (* Machine specific ? *)
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
         | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> () (* linuxext: ? *)

         (* old: "x" -> 10 gccext ? todo ugly, I put a fake value *)

         (* cppext:  can have   \ for multiline in string too *)
         | '\n' -> ()
         | _ -> error ("unrecognised symbol in char:"^tok lexbuf) lexbuf;
         );
          x ^ char lexbuf
       }
  | eof { error "WEIRD end of file in char" lexbuf; ""}

(*****************************************************************************)
(* Rule string *)
(*****************************************************************************)
(* less? factorise code with char ? but not same ending token so hard. *)
and string  = parse
  | '"'                                       { "" }
  | (_ as x)
      { Common2.string_of_char x^string lexbuf}

  | ("\\" (oct | oct oct | oct oct oct)) as x { x ^ string lexbuf }
  | ("\\x" (hex | hex hex)) as x              { x ^ string lexbuf }
  (* unicode *)
  | ("\\u" (hex hex hex hex)) as x { x ^ string lexbuf }
  | ("\\U" (hex hex hex hex hex hex hex hex)) as x { x ^ string lexbuf }
  | ("\\" (_ as v)) as x
       {
         (match v with (* Machine specific ? *)
         | 'n' -> ()  | 't' -> ()   | 'v' -> ()  | 'b' -> () | 'r' -> ()
         | 'f' -> () | 'a' -> ()
	 | '\\' -> () | '?'  -> () | '\'' -> ()  | '"' -> ()
         | 'e' -> () (* linuxext: ? *)

         (* old: "x" -> 10 gccext ? todo ugly, I put a fake value *)

         (* cppext:  can have   \ for multiline in string too *)
         | '\n' -> ()
         | _ -> error ("unrecognised symbol in string:"^tok lexbuf) lexbuf;
         );
         x ^ string lexbuf
       }
  | eof { error "WEIRD end of file in string" lexbuf; ""}

 (* Bug if add following code, cos match also the '"' that is needed
  * to finish the string, and so go until end of file.
  *)
 (*
  | [^ '\\']+
    { let cs = lexbuf |> tok |> list_of_string |> List.map Char.code in
      cs ++ string lexbuf
    }
  *)

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

(* less: allow only char-'*' ? *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | _
      { let s = tok lexbuf in
        error ("unrecognised symbol in comment:"^s) lexbuf;
        s ^ comment lexbuf
      }
  | eof { error "WEIRD end of file in comment" lexbuf; ""}

(*****************************************************************************)
(* Rule cpp_eat_until_nl *)
(*****************************************************************************)

(* cpp recognize C comments, so when #define xx (yy) /* comment \n ... */
 * then he has already erased the /* comment. So:
 * - dont eat the start of the comment otherwise afterwards we are in the middle
 *   of a comment and so we will problably get a parse error somewhere.
 * - have to recognize comments in cpp_eat_until_nl.
 *
 * note: I was using cpp_eat_until_nl for #define before, but now I
 * try also to parse define body so cpp_eat_until_nl is used only for the "body"
 * of other uninteresting directtives like #ifdef, #else where can have
 * stuff on the right on such directive.
 *)
and cpp_eat_until_nl = parse
  (* bugfix: need to handle comments too *)
  | "/*"
      { let s = tok lexbuf in
        let s2 = comment lexbuf in
        let s3 = cpp_eat_until_nl lexbuf in
        s ^ s2 ^ s3
      }
  | '\\' "\n" { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }

  | "\n"      { tok lexbuf }
  (* noteopti:
   * update: need also deal with comments chars now
   *)
  | [^ '\n' '\\'      '/' '*'  ]+
     { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }

  | eof { error "end of file in cpp_eat_until_nl" lexbuf; ""}
  | _   { let s = tok lexbuf in s ^ cpp_eat_until_nl lexbuf }
