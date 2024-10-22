{
(* Yoann Padioleau
 *
 * Copyright (C) 2009-2012 Facebook
 * Copyright (C) 2020-2022 r2c
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

open Parser_php

module Ast = Cst_php
module Flag = Flag_parsing
module Flag_php = Flag_parsing_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The PHP lexer.
 *
 * There are a few tricks to go around ocamllex restrictions
 * because PHP has different lexing rules depending on some "contexts"
 * (this is similar to Perl, e.g. the <<<END context).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* todo: reuse PI.Lexical_error *)
exception Lexical of string

let error s =
  if !Flag_php.strict_lexer
  then raise (Lexical s)
  else
    if !Flag.verbose_lexing
    then UCommon.pr2 ("LEXER: " ^ s)

(* pad: hack around ocamllex to emulate the yyless() of flex. The semantic
 * is not exactly the same than yyless(), so I use yyback() instead.
 * http://my.safaribooksonline.com/book/programming/flex/9780596805418/a-reference-for-flex-specifications/yyless
 *)
let yyback n lexbuf =
  lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - n;
  let currp = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { currp with
    Lexing.pos_cnum = currp.Lexing.pos_cnum - n;
  }
(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf

let tokinfo_file_str_pos (file : Fpath.t) (str : string) (bytepos : int) : Tok.t
  = Tok.make ~str ~bytepos ~file

(* all string passed to T_IDENT or T_VARIABLE should go through case_str *)
let case_str s =
  if !Flag_php.case_sensitive
  then s
  else String.lowercase_ascii s

let lang_ext_or_t_ident ii fii =
  if !Flag_php.facebook_lang_extensions
  then fii ii
  else T_IDENT(case_str (Tok.content_of_tok ii), ii)

let t_variable_or_metavar s info =
  (* sgrep-ext: we used to generate a T_IDENT here, so a metavariable
   * could be used where a T_IDENT was expected, e.g., a function name,
   * but we also want to use a metavariable where a T_VARIABLE could
   * be used, e.g., a parameter, so simpler to use a separate
   * token for metavariables. That way we also avoid certain
   * conflicts in the grammar.
   *)
  if AST_generic.is_metavar_name ("$" ^ s) && !Flag.sgrep_mode
  then T_METAVAR (case_str ("$" ^ s), info)
  else T_VARIABLE(case_str s, info)

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
(* opti: less convenient, but using a hash is faster than using a match.
 * Note that PHP allows those keywords to be used in certain places,
 * for instance as object fields as in $o->while, so the transformation
 * from a LABEL to those keywords is done only in a few cases.
 *
 * note: PHP is case insensitive so this hash table is used on
 * a lowercased string so don't put strings in uppercase below because
 * such keyword would never be reached!
 *
 * http://php.net/manual/en/reserved.keywords.php
 *
 * todo: callable, goto
 *)
let keyword_table = Hashtbl_.hash_of_list [

  "while",   (fun ii -> T_WHILE ii);   "endwhile", (fun ii -> T_ENDWHILE ii);
  "do",      (fun ii -> T_DO ii);
  "for",     (fun ii -> T_FOR ii);     "endfor", (fun ii -> T_ENDFOR ii);
  "foreach", (fun ii -> T_FOREACH ii); "endforeach",(fun ii -> T_ENDFOREACH ii);

  (* Those tokens were not in the original PHP lexer. This allowed to
   * have "self"/"parent" to be used at more places, e.g. as a function
   * name which is tolerated by PHP but should not IMHO. Those idents
   * have a special meaning and this should be reflected in the lexer,
   * especially since PHP 5.3 which allows static:: in addition to
   * self::, parent::. 'static' is a keyword so there is no reason
   * to not make self/parent keywords too.
   *
   * todo: should do something similar for $this.
   *)
  "self", (fun ii -> T_SELF ii); "parent", (fun ii -> T_PARENT ii);

  "if",       (fun ii -> T_IF ii);     "else", (fun ii -> T_ELSE ii);
  "elseif",   (fun ii -> T_ELSEIF ii); "endif",      (fun ii -> T_ENDIF ii);
  "break",    (fun ii -> T_BREAK ii);  "continue",   (fun ii -> T_CONTINUE ii);
  "switch",   (fun ii -> T_SWITCH ii); "endswitch",(fun ii -> T_ENDSWITCH ii);
  "case",       (fun ii -> T_CASE ii); "default",    (fun ii -> T_DEFAULT ii);

  "goto",     (fun ii -> T_GOTO ii);

  "return",     (fun ii -> T_RETURN ii);

  "try",        (fun ii -> T_TRY ii);     "catch",   (fun ii -> T_CATCH ii);
  "finally",    (fun ii -> T_FINALLY ii); "throw",   (fun ii -> T_THROW ii);

  "exit",       (fun ii -> T_EXIT ii); "die",        (fun ii -> T_EXIT ii);

  "array",      (fun ii -> T_ARRAY ii); "list",       (fun ii -> T_LIST ii);

  (* used for traits too *)
  "as",         (fun ii -> T_AS ii);

  "super",         (fun ii -> T_SUPER ii);

  "include",(fun ii ->T_INCLUDE ii);"include_once",(fun ii ->T_INCLUDE_ONCE ii);
  "require",(fun ii ->T_REQUIRE ii);"require_once",(fun ii ->T_REQUIRE_ONCE ii);

  "class",           (fun ii -> T_CLASS ii);
  "interface", (fun ii -> T_INTERFACE ii);
  "extends",         (fun ii -> T_EXTENDS ii);
  "implements",      (fun ii -> T_IMPLEMENTS ii);
  "new",             (fun ii -> T_NEW ii);
  "clone",           (fun ii -> T_CLONE ii);
  "instanceof",      (fun ii -> T_INSTANCEOF ii);

  (* php 5.4 traits ('use' and 'as' are used for traits and other things) *)
  "trait",           (fun ii -> T_TRAIT ii);
  "insteadof",       (fun ii -> T_INSTEADOF ii);

  (* php 5.3 namespace *)
  "namespace",       (fun ii -> T_NAMESPACE ii);
  (* used for traits and namespace *)
  "use",             (fun ii -> T_USE ii);

  "abstract", (fun ii -> T_ABSTRACT ii); "final", (fun ii -> T_FINAL ii);
  (* ugly: need a special lexing trick for async, see some code below
   * "async", (fun ii -> T_ASYNC ii);
   *)

  "public",          (fun ii -> T_PUBLIC ii);
  "protected",       (fun ii -> T_PROTECTED ii);
  "private",         (fun ii -> T_PRIVATE ii);

  "echo",            (fun ii -> T_ECHO ii); "print", (fun ii -> T_PRINT ii);

  "eval",            (fun ii -> T_EVAL ii);

  "global",          (fun ii -> T_GLOBAL ii);
  "function",        (fun ii -> T_FUNCTION ii);
  "empty",           (fun ii -> T_EMPTY ii);
  "const",           (fun ii -> T_CONST ii);
  "var",             (fun ii -> T_VAR ii); (* was VARTOKEN *)
  "declare", (fun ii -> T_DECLARE ii); "enddeclare",(fun ii ->T_ENDDECLARE ii);
  "static",          (fun ii -> T_STATIC ii);
  "unset",           (fun ii -> T_UNSET ii);
  "isset",           (fun ii -> T_ISSET ii);

  "__line__", (fun ii -> T_LINE ii);
  "__file__", (fun ii -> T_FILE ii); "__dir__",   (fun ii -> T_DIR ii);
  "__function__", (fun ii ->T_FUNC_C ii); "__method__",(fun ii ->T_METHOD_C ii);
  "__class__",  (fun ii -> T_CLASS_C ii);" __trait__", (fun ii ->T_TRAIT_C ii);
  "__namespace__", (fun ii -> T_NAMESPACE_C ii); (* was called T_NS_C *)

  (* recent PHP extensions (some were php-facebook-ext: before) *)
  "yield", (fun ii -> T_YIELD ii);
  "from", (fun ii -> T_FROM ii);

  (* old: "__halt_compiler", (fun ii -> T_HALT_COMPILER ii); *)

  (* php-facebook-ext: *)
  "await", (fun ii -> lang_ext_or_t_ident ii (fun x -> T_AWAIT x));
  "type",    (fun ii -> lang_ext_or_t_ident ii (fun x -> T_TYPE x));
  (* for attribute declarations and Hack first class enums *)
  "enum", (fun ii -> T_ENUM ii);
]

let _ = assert ((Common2.hkeys keyword_table) |>
                 List.for_all (fun s -> s = String.lowercase_ascii s))

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(* In most languages the lexer has no state and all strings are always
 * encoded in the same way, in the same token, wherever the string is
 * located in the file (except for strings inside comments). In PHP
 * some characters, e.g. "'", as in "I don't like you" or "'foo'" can
 * mean different things. Indeed the PHP language in fact supports
 * multiple languages or "modes" inside the same script (which also
 * make emacs mode for such language harder to define).
 *
 * Inside the PHP script code part, the quote is the start of a string
 * and there must be a corresponding quote ending the string. Inside
 * the HTML part of a PHP file it's just a character like any other
 * character. Inside heredocs (the '<<<XXX' construct) it is again
 * considered as any other character. In the same way some strings such
 * as 'if' can again mean different things; when they are preceded by a
 * '->' they correspond to the possible name of a field, otherwise
 * they are special PHP keywords.
 *
 * Because all of this, the lexer has multiple states which are
 * represented below and adjusted via some push/pop_mode function
 * below. Depending on the state the lexer behaves differently.
 *)

type state_mode =
  (* aka HTML mode *)
  | INITIAL
  (* started with <?php or <?, finished by ?> *)
  | ST_IN_SCRIPTING
  (* started with <?=, finished by ?> *)
  | ST_IN_SCRIPTING2
  (* handled by using ocamllex ability to define multiple lexers
   * | ST_COMMENT
   * | ST_DOC_COMMENT
   * | ST_ONE_LINE_COMMENT
   *)
  (* started with ", finished with ". In most languages strings
   * are a single tokens but PHP allow interpolation which means
   * a string can contain nested PHP variables or expressions.
   *)
  | ST_DOUBLE_QUOTES
  (* started with "`", finished with "`" *)
  | ST_BACKQUOTE
  (* started with ->, finished after reading one fieldname *)
  | ST_LOOKING_FOR_PROPERTY
  (* started with ${ *)
  | ST_LOOKING_FOR_VARNAME
  (* started with $xxx[ *)
  | ST_VAR_OFFSET
  (* started with <<<XXX, finished by XXX; *)
  | ST_START_HEREDOC of string
  (* started with <<<'XXX', finished by XXX; *)
  | ST_START_NOWDOC of string


let default_state = INITIAL

let _mode_stack =
  ref [default_state]
(* todo: now that I have yyback, maybe I should revisit this code. *)
let _pending_tokens =
  ref ([]: Parser_php.token list)

(* The logic to modify _last_non_whitespace_like_token is in the
 * caller of the lexer, that is in Parse_php.tokens.
 * Used for XHP.
 *)
let _last_non_whitespace_like_token =
  ref (None: Parser_php.token option)
let reset () =
  _mode_stack := [default_state];
    _pending_tokens := [];
   _last_non_whitespace_like_token := None;
  ()

let rec current_mode () =
  match !_mode_stack with
  | top :: _ -> top
  | [] ->
      error("mode_stack is empty, defaulting to INITIAL");
      reset();
      current_mode ()
let push_mode mode = Stack_.push mode _mode_stack
let pop_mode () = ignore(Stack_.pop _mode_stack)

(* What is the semantic of BEGIN() in flex ? start from scratch with empty
 * stack ?
 *)
let set_mode mode =
  pop_mode();
  push_mode mode;
  ()

(* Here is an example of state transition. Given a php file like:
 *
 *   <?php return <x>foo<y>bar</y></x>; ?>
 *
 * we start with the stack in [INITIAL]. The transitions are then:
 *
 * '<?php'  -> [IN_SCRIPTING], via set_mode()
 * ' '      -> [IN_SCRIPTING]
 * 'return' -> [IN_SCRIPTING]
 * '<x'     -> [IN_XHP_TAG "x"; IN_SCRIPTING], via push_mode()
 * '>'      -> [IN_XHP_TEXT "x"; IN_SCRIPTING], via set_mode()
 * 'foo'    -> [IN_XHP_TEXT "x"; IN_SCRIPTING]
 * '<y'     -> [IN_XHP_TAG "y";IN_XHP_TEXT "x"; IN_SCRIPTING], via push_mode()
 * '>'      -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x";IN_SCRIPTING], via set_mode()
 * 'bar'    -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x"; IN_SCRIPTING]
 * '</y>'   -> [IN_XHP_TEXT "x"; IN_SCRIPTING], via pop_mode()
 * '</x>'   -> [IN_SCRIPTING], via pop_mode()
 * ';'      -> [IN_SCRIPTING]
 * ' '      -> [IN_SCRIPTING]
 * '?>      -> [INITIAL], via set_mode()
 *
 *)

let push_token tok =
  _pending_tokens := tok::!_pending_tokens


(* ugly: in code like 'function foo( (function(string):string) $callback){}'
 * we want to parse the '(string)' not as a T_STRING_CAST but
 * as an open paren followed by other tokens. The right fix would
 * be to not have those ugly lexing rules for cast, but this would
 * lead to some grammar ambiguities or require other parsing hacks anyway.
*)
let lang_ext_or_cast t lexbuf =
  if !Flag_php.facebook_lang_extensions
  then
    (match !_last_non_whitespace_like_token with
    | Some (T_FUNCTION _) ->
      let s = tok lexbuf in
      (* just keep the open parenthesis *)
      yyback (String.length s - 1) lexbuf;
      TOPAR (tokinfo lexbuf)
    | _ ->
      t
    )
  else t
}

(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let ANY_CHAR = (_ | ['\n'] )
(* \x7f-\xff ???*)
let WHITESPACE = [' ' '\n' '\r' '\t']+
let TABS_AND_SPACES = [' ''\t']*
let NEWLINE = ("\r"|"\n"|"\r\n")
let WHITESPACEOPT = [' ' '\n' '\r' '\t']*
let LABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let BOOL = (['T''t']['R''r']['U''u']['E''e']) | (['F''f']['A''a']['L''l']['S''s']['E''e'])
let LNUM =	['0'-'9']+
let DNUM =	(['0'-'9']*['.']['0'-'9']+) | (['0'-'9']+['.']['0'-'9']* )

let EXPONENT_DNUM =	((LNUM|DNUM)['e''E']['+''-']?LNUM)
let HEXNUM =	("0x" | "0X")['0'-'9''a'-'f''A'-'F']+
let BINNUM =	"0b"['0'-'1']+
(*/*
 * LITERAL_DOLLAR matches unescaped $ that aren't followed by a label character
 * or a { and therefore will be taken literally. The case of literal $ before
 * a variable or "${" is handled in a rule for each string type
 *
 * TODO: \x7f-\xff
 */
 *)
let DOUBLE_QUOTES_LITERAL_DOLLAR =
  ("$"+([^'a'-'z''A'-'Z''_''$''"''\\' '{']|('\\' ANY_CHAR)))
let BACKQUOTE_LITERAL_DOLLAR =
  ("$"+([^'a'-'z''A'-'Z''_''$''`''\\' '{']|('\\' ANY_CHAR)))
(*/*
 * CHARS matches everything up to a variable or "{$"
 * {'s are matched as long as they aren't followed by a $
 * The case of { before "{$" is handled in a rule for each string type
 *
 * For heredocs, matching continues across/after newlines if/when it's known
 * that the next line doesn't contain a possible ending label
 */
 *)
let DOUBLE_QUOTES_CHARS =
  ("{"*([^'$''"''\\''{']|
    ("\\" ANY_CHAR))| DOUBLE_QUOTES_LITERAL_DOLLAR)
let BACKQUOTE_CHARS =
  ("{"*([^'$' '`' '\\' '{']|('\\' ANY_CHAR))| BACKQUOTE_LITERAL_DOLLAR)
let XHPLABEL = LABEL
let XHPTAG = XHPLABEL ([':''-'] XHPLABEL)*
let XHPATTR = XHPTAG

(*****************************************************************************)
(* Rule in script *)
(*****************************************************************************)
rule st_in_scripting = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
    | "/*" {
        let info = tokinfo lexbuf in
        let com = st_comment lexbuf in
        T_COMMENT(info |> Tok.tok_add_s com)
      }
    | "/**/" { T_COMMENT(tokinfo lexbuf) }

    | "/**" { (* RESET_DOC_COMMENT(); *)
        let info = tokinfo lexbuf in
        let com = st_comment lexbuf in
        T_DOC_COMMENT(info |> Tok.tok_add_s com)
      }
    | "#"|"//" {
        let info = tokinfo lexbuf in
        let com = st_one_line_comment lexbuf in
        T_COMMENT(info |> Tok.tok_add_s com)
      }

    (* old: | WHITESPACE { T_WHITESPACE(tokinfo lexbuf) } *)
    | [' '  '\t']+ { TSpaces(tokinfo lexbuf) }
    | ['\n' '\r']  { TNewline(tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)
    | '+' { TPLUS(tokinfo lexbuf) }      | '-' { TMINUS(tokinfo lexbuf) }
    | '*' { TMUL(tokinfo lexbuf) }       | '/' { TDIV(tokinfo lexbuf) }
    | '%' { TMOD(tokinfo lexbuf) }       | "**" { TPOW(tokinfo lexbuf) }

    | "++" { T_INC(tokinfo lexbuf) }   | "--" { T_DEC(tokinfo lexbuf) }

    | "="  { TEQ(tokinfo lexbuf) }

      | "+="  { T_PLUS_EQUAL(tokinfo lexbuf) }
      | "-="  { T_MINUS_EQUAL(tokinfo lexbuf) }
      | "*="  { T_MUL_EQUAL(tokinfo lexbuf) }
      | "/="  { T_DIV_EQUAL(tokinfo lexbuf) }
      | "%="  { T_MOD_EQUAL(tokinfo lexbuf) }
      | "&="  { T_AND_EQUAL(tokinfo lexbuf) }
      | "|="  { T_OR_EQUAL(tokinfo lexbuf) }
      | "^="  { T_XOR_EQUAL(tokinfo lexbuf) }
      | "<<=" { T_SL_EQUAL(tokinfo lexbuf) }
      | ">>=" { T_SR_EQUAL(tokinfo lexbuf) }
      | ".="  { T_CONCAT_EQUAL(tokinfo lexbuf) }

      | "=="  { T_IS_EQUAL(tokinfo lexbuf) }
      | "!="  { T_IS_NOT_EQUAL(tokinfo lexbuf) }
      | "===" { T_IS_IDENTICAL(tokinfo lexbuf) }
      | "!==" { T_IS_NOT_IDENTICAL(tokinfo lexbuf) }
      | "<>"  { T_IS_NOT_EQUAL(tokinfo lexbuf) }
      | "<=>" { T_ROCKET(tokinfo lexbuf) }

      | "<=" { T_IS_SMALLER_OR_EQUAL(tokinfo lexbuf) }
      | ">=" { T_IS_GREATER_OR_EQUAL(tokinfo lexbuf) }

      | "<"  { TSMALLER(tokinfo lexbuf) }
      | ">"  { TGREATER(tokinfo lexbuf) }

      | "&&" { T_BOOLEAN_AND(tokinfo lexbuf) }
      | "||" { T_BOOLEAN_OR(tokinfo lexbuf) }

      | "<<" { T_SL(tokinfo lexbuf) }
      | ">>" { T_SR(tokinfo lexbuf) }
      | "&"  { TAND(tokinfo lexbuf) }
      | "|"  { TOR(tokinfo lexbuf) }
      | "^"  { TXOR(tokinfo lexbuf) }

      | "OR"  { T_LOGICAL_OR(tokinfo lexbuf) }
      | "AND" { T_LOGICAL_AND(tokinfo lexbuf) }
      | "XOR" { T_LOGICAL_XOR(tokinfo lexbuf) }

      | "or"  { T_LOGICAL_OR(tokinfo lexbuf) }
      | "and" { T_LOGICAL_AND(tokinfo lexbuf) }
      | "xor" { T_LOGICAL_XOR(tokinfo lexbuf) }
   (* Flex/Bison allow to use single characters directly as-is in the grammar
    * by adding this in the lexer:
    *
    *       <ST_IN_SCRIPTING>{TOKENS} { return yytext[0];}
    *
    * We don't, so we have transformed all those tokens in proper tokens with
    * a name in the parser, and return them in the lexer.
    *)

    | '.'  { TDOT(tokinfo lexbuf) }
    | ','  { TCOMMA(tokinfo lexbuf) }
    | '@'  { T__AT(tokinfo lexbuf) }

    (* was called T_DOUBLE_ARROW but we actually now have a real ==> *)
    | "=>" { T_ARROW(tokinfo lexbuf) }
    | "~"  { TTILDE(tokinfo lexbuf) }
    | ";"  { TSEMICOLON(tokinfo lexbuf) }
    | "!"  { TBANG(tokinfo lexbuf) }
    | "::" { TCOLCOL (tokinfo lexbuf) } (* was called T_PAAMAYIM_NEKUDOTAYIM *)
    | "\\" { TANTISLASH (tokinfo lexbuf) } (* was called T_NS_SEPARATOR *)

    | '(' { TOPAR(tokinfo lexbuf) }  | ')' { TCPAR(tokinfo lexbuf) }
    | '[' { TOBRA(tokinfo lexbuf) }  | ']' { TCBRA(tokinfo lexbuf) }

    | ":" { TCOLON(tokinfo lexbuf) }
    | "?" { TQUESTION(tokinfo lexbuf) }
    (* PHP 8 attributes *)
    | "#[" { TOATTR (tokinfo lexbuf) }

    (* semgrep-ext: or var args extension *)
    | "..." { T_ELLIPSIS(tokinfo lexbuf) }
    (* facebook-ext: short lambdas *)
    | "==>" { T_DOUBLE_ARROW(tokinfo lexbuf) }
    (* sgrep-ext: *)
    | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
    | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

    (* we may come from a st_looking_for_xxx context, like in string
     * interpolation, so seeing a } we pop_mode!
     *)
    | '}' {
        pop_mode ();
        (* RESET_DOC_COMMENT(); ??? *)
        TCBRACE(tokinfo lexbuf)
      }
    | '{' {
        push_mode ST_IN_SCRIPTING;
        TOBRACE(tokinfo lexbuf)
      }
    (* TODO: alt: remove this and interpret LABEL differently when
     * the _last_non_whitespace_like_token is '->' (we should
     * also look for '::', 'function', '\\', etc. to transform more
     * keywords as identifiers.
     *)
    | (("->" | "?->") as sym) (WHITESPACEOPT as white) (LABEL as label) {
     (* todo: use yyback() instead of using pending_token with push_token.
      * buggy: push_mode ST_LOOKING_FOR_PROPERTY;
      *)
        let info = tokinfo lexbuf in

        let syminfo = Tok.rewrap_str sym info in

        let file = Tok.file_of_tok info in
        let parse_info = Tok.unsafe_loc_of_tok info in
        let pos_after_sym   =
          parse_info.Tok.pos.bytepos + String.length sym in
        let pos_after_white = pos_after_sym + String.length white in

        let whiteinfo = tokinfo_file_str_pos file white pos_after_sym in
        let lblinfo = tokinfo_file_str_pos file label pos_after_white in

        push_token (T_IDENT (case_str label, lblinfo));
       (* todo: could be newline ... *)
        push_token (TSpaces whiteinfo);

        T_OBJECT_OPERATOR(syminfo)
      }
    | "->" | "?->" {
        T_OBJECT_OPERATOR(tokinfo lexbuf)
      }
    (* see also T_VARIABLE below. lex use longest matching strings so this
     * rule is used only in a last resort, for code such as $$x, ${, etc
     *)
    | "$" { TDOLLAR(tokinfo lexbuf) }
    | "$$" { TDOLLARDOLLAR(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)
    | BOOL
        {
          let s = tok lexbuf in
          let ii = tokinfo lexbuf in
          T_BOOL (bool_of_string s, ii)
        }
    | LNUM | BINNUM | HEXNUM
        {
          (* more? cf original lexer *)
          let s = tok lexbuf in
          let ii = tokinfo lexbuf in
          T_LNUMBER (Parsed_int.parse (s, ii))
        }
    | DNUM | EXPONENT_DNUM
        { T_DNUMBER(float_of_string_opt ( tok lexbuf), tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
    (* ugly: 'self' and 'parent' should be keywords forbidden to be used
     * as regular identifiers. But PHP is case insensitive and does not
     * consider self/parent or SELF/PARENT as keywords. I think it's
     * bad so I now consider self/parent as keywords, but still allow
     * at least the uppercase form to be used as identifier, hence those
     * two rules below.
     *)
    | "SELF"   { T_IDENT (case_str (tok lexbuf), tokinfo lexbuf) }
    | "PARENT" { T_IDENT (case_str (tok lexbuf), tokinfo lexbuf) }

    (* ugly: some code is using ASYNC as a constant, so one way to fix
     * the conflict is to return the T_ASYNC only when it's used
     * as lowercase. Note that because some code is using 'async'
     * as a method we then need to extend ident_method_name
     * in parser_php.mly. The alternative would be to lex
     * "async" as a T_ASYNC only when it's followed by a T_FUNCTION
     * but this is also ugly.
     *)
    | "async" { T_ASYNC (tokinfo lexbuf) }

    | LABEL
        { let info = tokinfo lexbuf in
          let s = tok lexbuf in
          match Common2.optionise (fun () ->
            (* PHP is case insensitive ... it's ok to write IF(...) { ... } *)
            Hashtbl.find keyword_table (String.lowercase_ascii s))
          with
          | Some f -> f info
          (* was called T_STRING in original grammar *)
          | None ->
              T_IDENT (case_str s, info)
        }

    (* Could put a special rule for "$this", but there are multiple places here
     * where we can generate a T_VARIABLE, and we can have even expressions
     * like ${this}, so it is simpler to do the "this-analysis" in the grammar,
     * later when we generate a Var or This.
     *)
    | "$" (LABEL as s) {
          let info = tokinfo lexbuf in
          t_variable_or_metavar s info
          }
    | ("$" as dollar) "$" (LABEL as s) {
        let info = tokinfo lexbuf in
        let dollarinfo = Tok.rewrap_str (String.make 1 dollar) info in
        let parse_info = Tok.unsafe_loc_of_tok info in
        let file = Tok.file_of_tok info in
        let pos_after_sym = parse_info.Tok.pos.bytepos + 2 in
        let lblinfo = tokinfo_file_str_pos file s pos_after_sym in

        push_token (T_VARIABLE(case_str s, lblinfo));
        TDOLLAR dollarinfo
                  }
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (T_METAVAR (tok lexbuf, tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
    (*
     * The original PHP lexer does a few things to make the
     * difference at parsing time between static strings (which do not
     * contain any interpolation) and dynamic strings. So some regexps
     * below are quite hard to understand ... but apparently it works.
     * When the lexer thinks it's a dynamic strings, it let the grammar
     * do most of the hard work. See the rules using TGUIL in the grammar
     * (and here in the lexer).
     *
     * The optional  'b' at the beginning is for binary strings.
     *
     * /*
     *   ("{"*|"$"* ) handles { or $ at the end of a string (or the entire
     *  contents)
     *
     *
     * int bprefix = (yytext[0] != '"') ? 1 : 0;
     * zend_scan_escape_string(zendlval, yytext+bprefix+1, yyleng-bprefix-2, '"' TSRMLS_CC);
     */
     *)

    (* static strings *)
    | 'b'? (['"'] ((DOUBLE_QUOTES_CHARS* ("{"*|"$"* )) as s) ['"'])
        { T_CONSTANT_ENCAPSED_STRING(s, tokinfo lexbuf) }

    | 'b'? (['\''] (([^'\'' '\\']|('\\' ANY_CHAR))* as s)  ['\''])
        {
          (* more? cf original lexer *)
          T_CONSTANT_ENCAPSED_STRING(s, tokinfo lexbuf)
        }
    (* dynamic strings *)
    | ['"'] {
        push_mode ST_DOUBLE_QUOTES;
        TGUIL(tokinfo lexbuf)
      }
    | ['`'] {
        push_mode ST_BACKQUOTE;
        TBACKQUOTE(tokinfo lexbuf)
      }
    | 'b'? "<<<" TABS_AND_SPACES (LABEL as s) NEWLINE {
        set_mode (ST_START_HEREDOC s);
        T_START_HEREDOC (tokinfo lexbuf)
      }

    | 'b'? "<<<" TABS_AND_SPACES "'" (LABEL as s) "'" NEWLINE {
        set_mode (ST_START_NOWDOC s);
        (* could use another token, but simpler to reuse *)
        T_START_HEREDOC (tokinfo lexbuf)
      }


  (* ----------------------------------------------------------------------- *)
  (* Misc *)
  (* ----------------------------------------------------------------------- *)
    (* ugly: the cast syntax in PHP is newline and even comment sensitive. Hmm.
     * You cannot write for instance '$o = (int/*comment*/) foo();'.
     * We would really like to have different tokens for '(', space,
     * idents, and a grammar rule like 'expr: TOPAR TIdent TCPAR'
     * but then the grammar would be ambiguous with 'expr: TOPAR expr TCPAR'
     * unless like in C typenames have a special token type and you can
     * have a rule like 'expr: TOPAR TTypename TCPAR.
     * This could have been done in PHP if those typenames were reserved
     * tokens, but PHP allows to have functions or methods called e.g.
     * string(). So what they have done if this ugly lexing hack.
     *)
    | "(" TABS_AND_SPACES ("int"|"integer") TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_INT_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES ("real"|"double"|"float") TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_DOUBLE_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES "string" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_STRING_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES "binary" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_STRING_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES "array" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_ARRAY_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES "object" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_OBJECT_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES ("bool"|"boolean") TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_BOOL_CAST(tokinfo lexbuf)) lexbuf }

    (* PHP is case insensitive for many things *)
    | "(" TABS_AND_SPACES "Array" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_ARRAY_CAST(tokinfo lexbuf)) lexbuf }
    | "(" TABS_AND_SPACES "Object" TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_OBJECT_CAST(tokinfo lexbuf)) lexbuf }
    | "(" TABS_AND_SPACES ("Bool"|"Boolean") TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_BOOL_CAST(tokinfo lexbuf)) lexbuf }

    | "(" TABS_AND_SPACES ("unset") TABS_AND_SPACES ")"
        { lang_ext_or_cast (T_UNSET_CAST(tokinfo lexbuf)) lexbuf }
    | "?>"
        {
          (* because of XHP and my token merger:
           * old: | "</script"WHITESPACE*">")NEWLINE?
           *  see tests/xhp/pb_cant_merge2.php
          *)
          match current_mode () with
          | ST_IN_SCRIPTING ->
              set_mode INITIAL;
              (*/* implicit ';' at php-end tag */*)
              (* todo? ugly, could instead generate a FakeToken or
               * ExpandedToken, but then some code later may assume
               * right now that all tokens from the lexer are
               * origin tokens, so may be hard to change.
               *
               * old: (T_CLOSE_TAG(tokinfo lexbuf))
               * note that T_CLOSE_TAG was skipped anyway in Parse_php.parse_php
               *)
              TSEMICOLON(tokinfo lexbuf)

          | ST_IN_SCRIPTING2 ->
              set_mode INITIAL;
              T_CLOSE_TAG_OF_ECHO(tokinfo lexbuf)
          | _ ->
              raise Impossible
        }

  (* ----------------------------------------------------------------------- *)
    | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
    | _ {
        error ("unrecognised symbol, in token rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }

(*****************************************************************************)
(* Rule initial (html) *)
(*****************************************************************************)
and initial = parse

  | "<?php" ([' ''\t']|NEWLINE)
  (* php-facebook-ext: fbstrict extensions *)
  | "<?hh" ([' ''\t']|NEWLINE)
      {
        (* I now do a yyback to not eat the newline which is more
         * consistent with how I treat newlines elsewhere
         *)
        yyback 1 lexbuf;
        set_mode ST_IN_SCRIPTING;
        T_OPEN_TAG(tokinfo lexbuf)
      }

  | "<?PHP"([' ''\t']|NEWLINE)
  | "<?Php"([' ''\t']|NEWLINE)
      {
        (* "BAD USE OF <PHP at initial state, replace by <?php"; *)
        set_mode ST_IN_SCRIPTING;
        T_OPEN_TAG(tokinfo lexbuf)
      }

  | (([^'<']|"<"[^'?''%''s''<'])+(*{1,400}*))|"<s"|"<" {
      (* more? cf orinal lexer  *)
    T_INLINE_HTML(tok lexbuf, tokinfo lexbuf)
    }

  | "<?=" {
      (* less: if short_tags normally, otherwise T_INLINE_HTML *)
      set_mode ST_IN_SCRIPTING2;
      (* todo? ugly, may be better ot generate a real T_ECHO token
       * with maybe a FakeTok or ExpandeTok.
       *)
      T_OPEN_TAG_WITH_ECHO(tokinfo lexbuf);
    }

  | "<?" | "<script" WHITESPACE+ "language" WHITESPACE* "=" WHITESPACE *
           ("php"|"\"php\""|"\'php\'") WHITESPACE*">"
     {
       (* XXX if short_tags normally otherwise T_INLINE_HTML *)
       (* UCommon.pr2 "BAD USE OF <? at initial state, replace by <?php"; *)
       set_mode ST_IN_SCRIPTING;
       T_OPEN_TAG(tokinfo lexbuf);
     }

  (*------------------------------------------------------------------------ *)

  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
  | _ (* ANY_CHAR *) {
      error("unrecognised symbol, in token rule:"^tok lexbuf);
      TUnknown (tokinfo lexbuf)
    }



(*****************************************************************************)
(* Rule looking_for_xxx *)
(*****************************************************************************)

(* TODO not used for now *)
and st_looking_for_property = parse
  | "->" | "?->" {
      T_OBJECT_OPERATOR(tokinfo lexbuf)
    }
  | LABEL {
      pop_mode();
      T_IDENT(case_str (tok lexbuf), tokinfo lexbuf)
    }
(*
  | ANY_CHAR {
      (* XXX yyback(0) ?? *)
      pop_mode();
    }
*)


(*****************************************************************************)
and st_looking_for_varname = parse
  | LABEL {
      set_mode ST_IN_SCRIPTING;
      T_STRING_VARNAME(tok lexbuf, tokinfo lexbuf)
    }
  | _ {
      yyback 1 lexbuf;
      set_mode ST_IN_SCRIPTING;
      st_in_scripting lexbuf
    }

(*****************************************************************************)
and st_var_offset = parse

  | LNUM | HEXNUM | BINNUM { (* /* Offset must be treated as a string */ *)
    T_NUM_STRING (tok lexbuf, tokinfo lexbuf)
  }

  | "$" (LABEL as s) { T_VARIABLE(case_str s, tokinfo lexbuf) }
  | LABEL            { T_IDENT(case_str (tok lexbuf), tokinfo lexbuf)  }

  | "]" {
      pop_mode();
      TCBRA(tokinfo lexbuf);
    }
   | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
   | _ {
       error ("unrecognised symbol, in st_var_offset rule:"^tok lexbuf);
       TUnknown (tokinfo lexbuf)
     }

(*****************************************************************************)
(* Rule strings *)
(*****************************************************************************)

and st_double_quotes = parse

  | DOUBLE_QUOTES_CHARS+ {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }

  (* todo? was in original scanner ? *)
  | "{" {  T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)  }

    | "$" (LABEL as s)     { t_variable_or_metavar s (tokinfo lexbuf) }
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in
          let file = Tok.file_of_tok info in
          let varinfo = Tok.rewrap_str ("$" ^ s) info in
          let charpos_info = Tok.bytepos_of_tok varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = tokinfo_file_str_pos file "[" pos_after_label in
          push_token (TOBRA bra_info);
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(case_str s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$" *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

    | "{$" {
        yyback 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }

  | ['"'] {
      (* was originally set_mode ST_IN_SCRIPTING, but with XHP
       * the context for a double quote may not be anymore always
       * ST_IN_SCRIPTING
       *)
      pop_mode ();
      TGUIL(tokinfo lexbuf)
    }
   | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
   | _ {
       error("unrecognised symbol, in st_double_quotes rule:"^tok lexbuf);
       TUnknown (tokinfo lexbuf)
     }

(* ----------------------------------------------------------------------- *)
(* mostly copy paste of st_double_quotes; just the end regexp is different *)
and st_backquote = parse

  | BACKQUOTE_CHARS+ {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }
    (* TODO? should we use t_variable_or_metavar? *)
    | "$" (LABEL as s)     { T_VARIABLE (case_str s, (tokinfo lexbuf)) }
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in

          let file = Tok.file_of_tok info in
          let varinfo = Tok.rewrap_str ("$" ^ s) info in
          let charpos_info = Tok.bytepos_of_tok varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = tokinfo_file_str_pos file "[" pos_after_label in
          push_token (TOBRA bra_info);
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(case_str s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$" *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

    | "{$" {
        yyback 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }

  | ['`'] {
      set_mode ST_IN_SCRIPTING;
      TBACKQUOTE(tokinfo lexbuf)
    }

    | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
    | _ {
        error ("unrecognised symbol, in st_backquote rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }

(* ----------------------------------------------------------------------- *)
(* As heredoc have some of the semantic of double quote strings, again some
 * rules from st_double_quotes are copy pasted here.
 *
 * todo? the rules below are not what was in the original Zend lexer,
 * but the original lexer was doing very complicated stuff ...
 *)
and st_start_heredoc stopdoc = parse

  | (LABEL as s) (";"? as semi) (['\n' '\r'] as space) {
      let info = tokinfo lexbuf in

      let lbl_info = Tok.rewrap_str s info in

      let file = Tok.file_of_tok info in
      let pos = Tok.bytepos_of_tok info in
      let pos_after_label = pos + String.length s in
      let pos_after_semi = pos_after_label + String.length semi in

      let colon_info =
        tokinfo_file_str_pos file semi pos_after_label in
      let space_info =
        tokinfo_file_str_pos file (Common2.string_of_char space) pos_after_semi in

      if s = stopdoc
      then begin
        set_mode ST_IN_SCRIPTING;
        push_token (TNewline space_info);
        if semi = ";"
        then push_token (TSEMICOLON colon_info);

        T_END_HEREDOC(lbl_info)
      end else
        T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }

  | [^ '\n' '\r' '$' '{' '\\']+ {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }
  | "\\" ANY_CHAR { T_ENCAPSED_AND_WHITESPACE (tok lexbuf, tokinfo lexbuf) }

    | "$" (LABEL as s)     { t_variable_or_metavar s (tokinfo lexbuf) }
    | "$" (LABEL as s) "[" {
          let info = tokinfo lexbuf in

          let file = Tok.file_of_tok info in
          let varinfo = Tok.rewrap_str ("$" ^ s) info in
          let charpos_info = Tok.bytepos_of_tok varinfo in
          let pos_after_label = charpos_info + String.length ("$" ^ s) in

          let bra_info = tokinfo_file_str_pos file "[" pos_after_label in
          push_token (TOBRA bra_info);
          push_mode ST_VAR_OFFSET;
          T_VARIABLE(case_str s, varinfo)
      }
    (* bugfix: can have strings like "$$foo$", or {{$foo}} *)
    | "$" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }
    | "{" { T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf) }

  | ['\n' '\r'] { TNewline (tokinfo lexbuf) }

    | "{$" {
        yyback 1 lexbuf;
        push_mode ST_IN_SCRIPTING;
        T_CURLY_OPEN(tokinfo lexbuf);
      }
    | "${" {
        push_mode ST_LOOKING_FOR_VARNAME;
        T_DOLLAR_OPEN_CURLY_BRACES(tokinfo lexbuf);
      }

    | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
    | _ {
        error("unrecognised symbol, in st_start_heredoc rule:"^tok lexbuf);
        TUnknown (tokinfo lexbuf)
      }

(* ----------------------------------------------------------------------- *)
(* todo? this is not what was in the original lexer, but the original lexer
 * does complicated stuff ...
 *)
and st_start_nowdoc stopdoc = parse

  | (LABEL as s) (";"? as semi) (['\n' '\r'] as space) {
      let info = tokinfo lexbuf in

      let lbl_info = Tok.rewrap_str s info in

      let file = Tok.file_of_tok info in
      let pos = Tok.bytepos_of_tok info in
      let pos_after_label = pos + String.length s in
      let pos_after_semi = pos_after_label + String.length semi in

      let colon_info =
        tokinfo_file_str_pos file semi pos_after_label in
      let space_info =
        tokinfo_file_str_pos file (Common2.string_of_char space) pos_after_semi in

      if s = stopdoc
      then begin
        set_mode ST_IN_SCRIPTING;
        push_token (TNewline space_info);
        if semi = ";"
        then push_token (TSEMICOLON colon_info);
        (* reuse same token than for heredocs *)
        T_END_HEREDOC(lbl_info)
      end else
        T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }
  | [^ '\n' '\r']+ {
      T_ENCAPSED_AND_WHITESPACE(tok lexbuf, tokinfo lexbuf)
    }

  | ['\n' '\r'] {
      TNewline (tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
  | _ {
       error ("unrecognised symbol, in st_start_nowdoc rule:"^tok lexbuf);
       TUnknown (tokinfo lexbuf)
    }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)
and st_comment = parse
  | "*/" { tok lexbuf }

  (* noteopti: *)
  | [^'*']+ { let s = tok lexbuf in s ^ st_comment lexbuf }
  | "*"     { let s = tok lexbuf in s ^ st_comment lexbuf }

    | eof { error "end of file in comment"; "*/"}
    | _  {
        let s = tok lexbuf in
        error("unrecognised symbol in comment:"^s);
        s ^ st_comment lexbuf
      }

and st_one_line_comment = parse
  | "?"|"%"|">" { let s = tok lexbuf in s ^ st_one_line_comment lexbuf }
  | ([^'\n' '\r' '?''%''>']* as start) (ANY_CHAR as x)
      {
        (match x with
        | '?' | '%' | '>' ->
            yyback 1 lexbuf;
            start ^ st_one_line_comment lexbuf
        (* end of recursion when new line or other character  *)
        | '\n' ->
            (* don't want the newline to be part of the comment *)
            yyback 1 lexbuf;
            start
        | c -> start ^ String.make 1 c
        )
      }
  | NEWLINE {
      (* don't want the newline to be part of the comment *)
      yyback 1 lexbuf;
      ""
    }
  | "?>" {
      (* "%>" is only when use asp_tags *)
      yyback 2 lexbuf;
      ""
    }

    | eof { error "end of file in comment"; "*/" }
    | _ {
        error ("unrecognised symbol, in st_one_line_comment rule:"^tok lexbuf);
        tok lexbuf
      }
