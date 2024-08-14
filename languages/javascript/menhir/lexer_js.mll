{
(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013, 2014 Facebook
 * Copyright (C) 2019 Semgrep Inc.
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

open Parser_js
module Flag = Flag_parsing
module Log = Log_parser_javascript.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Javascript lexer.
 *
 * src: originally ocamllexified from Marcel Laverdet 'fbjs2'
 *
 * There are a few tricks to go around ocamllex restrictions
 * because Javascript now have different lexing rules depending on some
 * "contexts", especially for JSX/XHP (this is similar to Perl
 * with its <<<END context).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Tok.tok_of_lexbuf
let error = Parsing_error.lexical_error

(* less: should use Buffer and not ^ so we should not need that *)
let tok_add_s = Tok.tok_add_s

let hexa_to_int = function
  | '0'..'9' as x -> Char.code x - Char.code '0'
  | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
  | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
  | _ -> assert false;;

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let keyword_table = Hashtbl_.hash_of_list [

  "if",         (fun ii -> T_IF ii);
  "else",       (fun ii -> T_ELSE ii);

  "while",      (fun ii -> T_WHILE ii);
  "do",         (fun ii -> T_DO ii);
  "for",        (fun ii -> T_FOR ii);

  "switch",     (fun ii -> T_SWITCH ii);
  "case",       (fun ii -> T_CASE ii);
  "default",    (fun ii -> T_DEFAULT ii);

  "break",      (fun ii -> T_BREAK ii);
  "continue",   (fun ii -> T_CONTINUE ii);

  "return",     (fun ii -> T_RETURN ii);

  "throw",      (fun ii -> T_THROW ii);
  "try",        (fun ii -> T_TRY ii);
  "catch",      (fun ii -> T_CATCH ii);
  "finally",    (fun ii -> T_FINALLY ii);

  "function",   (fun ii -> T_FUNCTION ii);
  "var",        (fun ii -> T_VAR ii);
  (* es6: *)
  "const",      (fun ii -> T_CONST ii);
  "let",        (fun ii -> T_LET ii);

  "new",        (fun ii -> T_NEW ii);
  "delete",     (fun ii -> T_DELETE ii);

  "void",       (fun ii -> T_VOID ii);
  "null",       (fun ii -> T_NULL ii);

  "false",      (fun ii -> T_FALSE ii);
  "true",       (fun ii -> T_TRUE ii);

  "in",         (fun ii -> T_IN ii);
  (* es6: *)
  "of",         (fun ii -> T_OF ii);

  "this",       (fun ii -> T_THIS ii);
  (* es6: *)
  "super",       (fun ii -> T_SUPER ii);

  "instanceof", (fun ii -> T_INSTANCEOF ii);
  "typeof",     (fun ii -> T_TYPEOF ii);

  "with",       (fun ii -> T_WITH ii);

  (* es6: *)

  "yield",       (fun ii -> T_YIELD ii);
  "async",       (fun ii -> T_ASYNC ii);
  "await",       (fun ii -> T_AWAIT ii);

  "class",      (fun ii -> T_CLASS ii);
  "extends",    (fun ii -> T_EXTENDS ii);

  "static",     (fun ii -> T_STATIC ii);

  "get",     (fun ii -> T_GET ii);
  "set",     (fun ii -> T_SET ii);

  "import",     (fun ii -> T_IMPORT ii);
  "export",     (fun ii -> T_EXPORT ii);
  "from",       (fun ii -> T_FROM ii);
  "as",       (fun ii -> T_AS ii);

  (* typescript: *)

  "interface",      (fun ii -> T_INTERFACE ii);
  "implements",     (fun ii -> T_IMPLEMENTS ii);
  "constructor",     (fun ii -> T_CONSTRUCTOR ii);

  "public",     (fun ii -> T_PUBLIC ii);
  "private",     (fun ii -> T_PRIVATE ii);
  "protected",     (fun ii -> T_PROTECTED ii);

  "readonly", (fun ii -> T_READONLY ii);

  "type",     (fun ii -> T_TYPE ii);

  "any",     (fun ii -> T_ANY_TYPE ii);
  "number",     (fun ii -> T_NUMBER_TYPE ii);
  "boolean",     (fun ii -> T_BOOLEAN_TYPE ii);
  "string",     (fun ii -> T_STRING_TYPE ii);
  (* void before *)

  "enum",     (fun ii -> T_ENUM ii);

  "module",     (fun ii -> T_MODULE ii);
  "declare",     (fun ii -> T_DECLARE ii);

  (* less: debugger, opaque, package, require *)
]

(* ---------------------------------------------------------------------- *)
(* Lexer State *)
(* ---------------------------------------------------------------------- *)
(* note: mostly a copy paste of the trick used in the lexer for PHP *)

type state_mode =
  (* Regular Javascript mode *)
  | ST_IN_CODE

  (* started with <xx when preceded by a certain token (e.g. 'return' '<xx'),
   * finished by '>' by transiting to ST_IN_XHP_TEXT, or really finished
   * by '/>'.
   *)
  | ST_IN_XHP_TAG of string (* the current tag, e,g, "x_frag" *)
  (* started with the '>' of an opening tag, finished when '</x>' *)
  | ST_IN_XHP_TEXT of string (* the current tag *)

  (* started with "`", finished with "`" *)
  | ST_IN_BACKQUOTE

let default_state = ST_IN_CODE

let _mode_stack =
  ref [default_state]

(* The logic to modify _last_non_whitespace_like_token is in the
 * caller of the lexer, that is in Parse_js.tokens.
 * Used for ambiguity between / as a divisor and start of regexp.
 *)
let _last_non_whitespace_like_token =
  ref (None: Parser_js.token option)

let reset () =
  _mode_stack := [default_state];
   _last_non_whitespace_like_token := None;
  ()

let rec current_mode () =
  match !_mode_stack with
  | top :: _ -> top
  | [] ->
      Log.warn (fun m -> m "mode_stack is empty, defaulting to INITIAL");
      reset();
      current_mode ()

let push_mode mode = Stack_.push mode _mode_stack
let pop_mode () = ignore(Stack_.pop _mode_stack)
let set_mode mode = begin pop_mode(); push_mode mode; end

(* Here is an example of state transition. Given a js file like:
 *
 *   return <x>foo<y>bar</y></x>;
 *
 * we start with the stack in [ST_IN_CODE]. The transitions are then:
 *
 * 'return' -> [IN_CODE]
 * '<x'     -> [IN_XHP_TAG "x"; IN_CODE], via push_mode()
 * '>'      -> [IN_XHP_TEXT "x"; IN_CODE], via set_mode()
 * 'foo'    -> [IN_XHP_TEXT "x"; IN_CODE]
 * '<y'     -> [IN_XHP_TAG "y";IN_XHP_TEXT "x"; IN_CODE], via push_mode()
 * '>'      -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x";IN_CODE], via set_mode()
 * 'bar'    -> [IN_XHP_TEXT "y"; IN_XHP_TEXT "x"; IN_CODE]
 * '</y>'   -> [IN_XHP_TEXT "x"; IN_CODE], via pop_mode()
 * '</x>'   -> [IN_CODE], via pop_mode()
 * ';'      -> [IN_CODE]
 *
 *)

}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let NEWLINE = ("\r"|"\n"|"\r\n")
let HEXA = ['0'-'9''a'-'f''A'-'F']

(* JSX allows also '.' *)
let XHPLABEL =	['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_''-'  '.']*
let XHPTAG_ORIG = XHPLABEL (":" XHPLABEL)*
let XHPATTR = XHPLABEL (":" XHPLABEL)*
(* sgrep-ext: less: should use Flag.sgrep_guard to forbid $ in normal mode *)
let XHPTAG = '$'? XHPTAG_ORIG

let InputCharacter = [^ '\r' '\n']

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule initial = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | "/*" {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      Buffer.add_string buf "/*";
      st_comment buf lexbuf;
      TComment(info |> Tok.rewrap_str (Buffer.contents buf))
    }

  (* don't keep the trailing \n; it will be in another token *)
  | "//" InputCharacter* { TComment (tokinfo lexbuf) }
  (* should be accepted only at the beginning of the file *)
  | "#!" InputCharacter* { TComment (tokinfo lexbuf) }

  (* classic space characters (\u0020 and \u0009) *)
  | [' ' '\t']+
  (* non-classic space characters
   * reference: https://www.ecma-international.org/ecma-262/5.1/#sec-7.2 *)
  | "\x0b" (* vertical tab *) | "\x0c" (* form feed *)
  (* unicode space characters.
   * Note that OCaml supports now unicode characters as \u{00a0} in strings
   * but this does not seem to work in ocamllex, hence the hardcoding of
   * the actual UTF8 bytes below (use scripts/unicode.ml and hexl-mode in
   * Emacs to get those byte values).
   * The right solution would be to switch to a unicode-aware lexer generator,
   * like ulex or sedlex.
   * todo: https://en.wikipedia.org/wiki/Whitespace_character#Unicode
   *)
  | "\xc2\xa0" (* non-breaking-space \u{00A0} *)
  | "\xef\xbb\xbf" (* byte-order-mark \u{FEFF} *)
    { TCommentSpace(tokinfo lexbuf) }

  | NEWLINE      { TCommentNewline(tokinfo lexbuf) }

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  | "{" {
    push_mode ST_IN_CODE;
    T_LCURLY (tokinfo lexbuf);
  }
  | "}" {
    pop_mode ();
    T_RCURLY (tokinfo lexbuf);
  }

  | "(" { T_LPAREN (tokinfo lexbuf) } | ")" { T_RPAREN (tokinfo lexbuf) }

  | "[" { T_LBRACKET (tokinfo lexbuf) } | "]" { T_RBRACKET (tokinfo lexbuf) }
  | "." { T_PERIOD (tokinfo lexbuf) }
  | ";" { T_SEMICOLON (tokinfo lexbuf) }
  | "," { T_COMMA (tokinfo lexbuf) }
  | ":" { T_COLON (tokinfo lexbuf) }
  | "?" { T_PLING (tokinfo lexbuf) }
  | "?." { T_QUESTDOT (tokinfo lexbuf) }
  | "&&" { T_AND (tokinfo lexbuf) } | "||" { T_OR (tokinfo lexbuf) }
  | "===" { T_STRICT_EQUAL (tokinfo lexbuf) }
  | "!==" { T_STRICT_NOT_EQUAL (tokinfo lexbuf) }
  | "<=" { T_LESS_THAN_EQUAL (tokinfo lexbuf) }
  | ">=" { T_GREATER_THAN_EQUAL (tokinfo lexbuf) }
  | "==" { T_EQUAL (tokinfo lexbuf) }
  | "!=" { T_NOT_EQUAL (tokinfo lexbuf) }
  | "++" { T_INCR (tokinfo lexbuf) } | "--" { T_DECR (tokinfo lexbuf) }
  | "<<=" { T_LSHIFT_ASSIGN (tokinfo lexbuf) }
  | "<<" { T_LSHIFT (tokinfo lexbuf) }
  | ">>=" { T_RSHIFT_ASSIGN (tokinfo lexbuf) }
  | ">>>=" { T_RSHIFT3_ASSIGN (tokinfo lexbuf) }
  | ">>>" { T_RSHIFT3 (tokinfo lexbuf) }
  | ">>" { T_RSHIFT (tokinfo lexbuf) }
  | "+=" { T_PLUS_ASSIGN (tokinfo lexbuf) }
  | "-=" { T_MINUS_ASSIGN (tokinfo lexbuf) }

  | "*=" { T_MULT_ASSIGN (tokinfo lexbuf) }
  | "%=" { T_MOD_ASSIGN (tokinfo lexbuf) }
  | "&=" { T_BIT_AND_ASSIGN (tokinfo lexbuf) }
  | "|=" { T_BIT_OR_ASSIGN (tokinfo lexbuf) }
  | "^=" { T_BIT_XOR_ASSIGN (tokinfo lexbuf) }
  (* see also xhp code for handling "< XHPTAG" below *)
  | "<" { T_LESS_THAN (tokinfo lexbuf) }
  | ">" { T_GREATER_THAN (tokinfo lexbuf) }
  | "+" { T_PLUS (tokinfo lexbuf) }
  | "-" { T_MINUS (tokinfo lexbuf) }
  | "*" { T_MULT (tokinfo lexbuf) }
  (* for '/' see below the regexp handling *)
  | "%" { T_MOD (tokinfo lexbuf) }
  | "|" { T_BIT_OR (tokinfo lexbuf) }
  | "&" { T_BIT_AND (tokinfo lexbuf) }
  | "^" { T_BIT_XOR (tokinfo lexbuf) }
  | "!" { T_NOT (tokinfo lexbuf) }
  | "~" { T_BIT_NOT (tokinfo lexbuf) }
  | "=" { T_ASSIGN (tokinfo lexbuf) }
  (* es7: *)
  | "**" { T_EXPONENT (tokinfo lexbuf) }

  (* arrows (aka short lambdas) *)
  | "=>" { T_ARROW (tokinfo lexbuf) }
  (* variable number of parameters (and sgrep-ext)
   * less: enforce directly attached to ident? *)
  | "..." { T_DOTS (tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }

  (* https://tc39.es/proposal-decorators/ *)
  | "@"  { T_AT (tokinfo lexbuf)  }

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident *)
  (* ----------------------------------------------------------------------- *)
  (* sgrep-ext: no need for extension actually. A Javascript identifier
   * can have a leading '$', which means sgrep for JS will have some
   * limitations.
   *)
  | ['a'-'z''A'-'Z' '$' '_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      match Common2.optionise (fun () ->
        Hashtbl.find keyword_table s (* need case insensitive ? *))
      with
      | Some f -> f info
      | None -> T_ID (s, info)
    }
  (* sgrep-ext: *)
  | '$' "..." ['A'-'Z''_']['A'-'Z''_''0'-'9']*
     { Flag.sgrep_guard (T_ID (tok lexbuf, tokinfo lexbuf)) }

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | "0" ['X''x'] HEXA+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_INT (int_of_string_opt s, info)
    }
  (* es6? *)
  | "0" ['B''b'] ['0'-'1']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_INT (int_of_string_opt s, info)
    }
  (* es6? *)
  | "0" ['O''o'] ['0'-'7']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_INT (int_of_string_opt s, info)
    }

  | '0' (['0'-'7']+ as n) {
      let s = "0o" ^ n in
      let info = tokinfo lexbuf in
      T_INT (int_of_string_opt s, info)
    }

  | ['0'-'9']*'.'?['0'-'9']+['e''E']['-''+']?['0'-'9']+ (* {1,3} *) {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_FLOAT (float_of_string_opt s, info)
    }

  | ['0'-'9']+'.'? |
    ['0'-'9']*'.'['0'-'9']+ {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in
      T_FLOAT (float_of_string_opt s, info)
    }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)
  | ("'"|'"') as quote {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in

      string_quote quote buf lexbuf;

      let s = Buffer.contents buf in
      let buf2 = Buffer.create 127 in
      Buffer.add_char buf2 quote;
      Buffer.add_string buf2 s;
      Buffer.add_char buf2 quote;

      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info |> Tok.rewrap_str (Buffer.contents buf2))
    }

  (* ----------------------------------------------------------------------- *)
  (* Backquote (interpolated) strings  *)
  (* ----------------------------------------------------------------------- *)

  | "`" {
      push_mode ST_IN_BACKQUOTE;
      T_BACKQUOTE(tokinfo lexbuf)
  }

  (* ----------------------------------------------------------------------- *)
  (* Regexp *)
  (* ----------------------------------------------------------------------- *)
  (* take care of ambiguity with start of comment //, and with
   * '/' as a divisor operator
   *
   * it can not be '/' [^ '/']* '/' because then
   * comments will not be recognized as lex tries
   * to find the longest match.
   *
   * It can not be
   * '/' [^'*''/'] ([^'/''\n'])* '/' ['A'-'Z''a'-'z']*
   * because a / (b/c)  will be recognized as a regexp.
   *)
  | "/" | "/=" {
      let s = tok lexbuf in
      let info = tokinfo lexbuf in

      match !_last_non_whitespace_like_token with
      | Some (
            T_INT _ | T_FLOAT _ | T_STRING _ | T_REGEX _
          | T_FALSE _ | T_TRUE _ | T_NULL _
          | T_THIS _
          | T_INCR _ | T_DECR _
          | T_RBRACKET _ | T_RPAREN _
          | T_ID _
          (* typescript: ugly! keywords that are idents *)
          | T_NUMBER_TYPE _ | T_BOOLEAN_TYPE _ | T_ANY_TYPE _ | T_STRING_TYPE _
        ) ->
          (match s with
          | "/" -> T_DIV info
          | "/=" -> T_DIV_ASSIGN (tokinfo lexbuf)
          | _ -> raise Impossible
          )
      (* a regexp *)
      | _ ->
          let buf = Buffer.create 127 in
          let buf_modifier = Buffer.create 127 in
          (match s with
          | "/" -> ()
          | "/=" -> Buffer.add_string buf "="
          | _ -> raise Impossible
          );
          regexp buf buf_modifier lexbuf;
          let str = Buffer.contents buf in
          let str_modifier = Buffer.contents buf_modifier in

          let fullstr = "/" ^ str ^ "/" ^ str_modifier in
          let info = info |> Tok.rewrap_str fullstr in
          let (lt, info) = Tok.split_tok_at_bytepos 1 info in
          let (t, info) = Tok.split_tok_at_bytepos (String.length str) info in
          let (rt, info) = Tok.split_tok_at_bytepos 1 info in
          let modifier =
            if str_modifier = ""
            then None
            else Some (str_modifier, info)
          in
          T_REGEX ((lt, (str, t), rt), modifier)
    }

  (* ----------------------------------------------------------------------- *)
  (* XHP/JSX *)
  (* ----------------------------------------------------------------------- *)

  (* xhp: we need to disambiguate the different use of '<' to know whether
   * we are in a position where an XHP construct can be started. Knowing
   * what was the previous token seems enough; no need to hack the
   * grammar to have a global shared by the lexer and parser.
   *
   * We could maybe even return a TSMALLER in both cases and still
   * not generate any conflict in the grammar, but it feels cleaner to
   * generate a different token, because we will really change the lexing
   * mode when we will see a '>' which makes the parser enter in the
   * ST_IN_XHP_TEXT state where it's ok to write "I don't like you"
   * in which the quote does not need to be ended.
   *)
  | "<" (XHPTAG as tag) {
    match !_last_non_whitespace_like_token with
    | Some (
        T_LPAREN _
      | T_SEMICOLON _ (* TODO: somes ambiguities with generics then! *)
      | T_COMMA _
      | T_LCURLY _ | T_RCURLY _
      | T_RETURN _
      | T_ASSIGN _
      | T_ARROW _
      | T_PLING _ | T_COLON _
      | T_LBRACKET _
      | T_AND _ | T_OR _ | T_PLUS _
    ) when !Flag_parsing_js.jsx ->
      push_mode (ST_IN_XHP_TAG tag);
      T_XHP_OPEN_TAG(tag, tokinfo lexbuf)
    (* sgrep-ext: *)
    | None when !Flag.sgrep_mode ->
      push_mode (ST_IN_XHP_TAG tag);
      T_XHP_OPEN_TAG(tag, tokinfo lexbuf)
    | _ ->
      Parsing_helpers.yyback (String.length tag) lexbuf;
      T_LESS_THAN(tokinfo lexbuf)
  }

   (* support short fragment syntax of react.
    * see https://reactjs.org/docs/fragments.html#short-syntax
    * mostly copy paste from above
    *)
   | "<>" {
    match !_last_non_whitespace_like_token with
    | Some (
        T_LPAREN _
      | T_SEMICOLON _ (* TODO: somes ambiguities with generics then! *)
      | T_COMMA _
      | T_LCURLY _ | T_RCURLY _
      | T_RETURN _
      | T_ASSIGN _
      | T_ARROW _
      | T_PLING _ | T_COLON _
      | T_LBRACKET _
      | T_AND _ | T_OR _ | T_PLUS _
    ) when !Flag_parsing_js.jsx
      ->
      let tag = "" in
      (* we go directly in ST_IN_XHP_TEXT *)
      push_mode (ST_IN_XHP_TEXT tag);
      T_XHP_SHORT_FRAGMENT(tokinfo lexbuf)

    (* sgrep-ext: *)
    | None when !Flag.sgrep_mode ->
      let tag = "" in
      (* we go directly in ST_IN_XHP_TEXT *)
      push_mode (ST_IN_XHP_TEXT tag);
      T_XHP_SHORT_FRAGMENT(tokinfo lexbuf)

    | _ ->
      Parsing_helpers.yyback 1 lexbuf;
      T_LESS_THAN(tokinfo lexbuf)
    }


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

and string_escape quote buf = parse
  | '\\'{ Buffer.add_string buf "\\\\" }
  | 'x' (HEXA as a) (HEXA as b)
    { let code = hexa_to_int a * 16 + hexa_to_int b in
      if code > 127
      then
        let c1 = code lsr 6 + 0xC0
        and c2 = code land 0x3f + 0x80 in
        Buffer.add_char buf (Char.chr c1);
        Buffer.add_char buf (Char.chr c2)
      else Buffer.add_char buf (Char.chr code) }
  | 'u' HEXA HEXA HEXA HEXA {
      Buffer.add_char buf '\\';
      Buffer.add_string buf (Lexing.lexeme lexbuf) }
  | (_ as c)
    { Buffer.add_char buf '\\'; Buffer.add_char buf c }
      (* if c = quote *)
      (* then Buffer.add_char buf quote *)
      (* else Buffer.add_char buf c } *)


and string_quote q buf = parse
  | ("'"|'"') as q' {
    if q =$= q'
    then ()
    else (Buffer.add_char buf q'; string_quote q buf lexbuf) }
  | '\\' {
      string_escape q buf lexbuf;
      string_quote q buf lexbuf
    }
  | (_ as x) { Buffer.add_char buf x; string_quote q buf lexbuf }
  | eof      { error "WIERD end of file in quoted string" lexbuf }

(*****************************************************************************)
(* Rule backquote *)
(*****************************************************************************)

and backquote = parse
  | "`" {
    pop_mode();
    T_BACKQUOTE(tokinfo lexbuf)
  }
  | "${" {
    push_mode ST_IN_CODE;
    T_DOLLARCURLY(tokinfo lexbuf)
  }
  | [^'`' '$' '\\']+ {
      T_ENCAPSED_STRING(tok lexbuf, tokinfo lexbuf)
    }
  (* semgrep-ext: this is for metavariable in semgrep *)
  | "$"[^'`' '$' '\\' '{']* {
      T_ENCAPSED_STRING(tok lexbuf, tokinfo lexbuf)
  }
  | '\\' {
      let buf = Buffer.create 127 in
      let info = tokinfo lexbuf in
      string_escape '`' buf lexbuf;
      T_ENCAPSED_STRING(Buffer.contents buf, info)
    }

  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
  | _  {
      error ("unrecognised symbol, in backquote string:"^tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }


(*****************************************************************************)
(* Rule regexp *)
(*****************************************************************************)
and regexp buf buf_modifier = parse
  | '/' { regexp_maybe_ident buf_modifier lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    (* check char ? *)
                    Buffer.add_char buf x;
                    regexp buf buf_modifier lexbuf }
  | '[' { Buffer.add_char buf '['; regexp_class buf buf_modifier lexbuf }
  | (_ as x)       { Buffer.add_char buf x; regexp buf buf_modifier lexbuf }
  | eof { error "WIERD end of file in regexp" lexbuf; ()}


and regexp_class buf buf_modifier = parse
  | ']' { Buffer.add_char buf ']';
             regexp buf buf_modifier lexbuf }
  | '\\' (_ as x) { Buffer.add_char buf '\\';
                    Buffer.add_char buf x;
                    regexp_class buf buf_modifier lexbuf }
  | (_ as x) { Buffer.add_char buf x; regexp_class buf buf_modifier lexbuf }

and regexp_maybe_ident buf = parse
  | ['A'-'Z''a'-'z']* { Buffer.add_string buf (tok lexbuf) }

(*****************************************************************************)
(* Rule comment *)
(*****************************************************************************)

and st_comment buf = parse
  | "*/"    { Buffer.add_string buf (tok lexbuf) }
  (* noteopti: *)
  | [^'*']+ { Buffer.add_string buf (tok lexbuf); st_comment buf lexbuf }
  | "*"     { Buffer.add_string buf (tok lexbuf); st_comment buf lexbuf }
  | eof     { error "end of file in comment" lexbuf }
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in comment:"^s) lexbuf;
      Buffer.add_string buf s;
      st_comment buf lexbuf
    }

(*****************************************************************************)
(* Rules for XHP *)
(*****************************************************************************)
(* XHP lexing states and rules *)

and st_in_xhp_tag current_tag = parse

  (* The original XHP parser have some special handlings of
   * whitespace and enforce to use certain whitespace at
   * certain places. Not sure I need to enforce this too.
   * Simpler to ignore whitespaces.
   *
   * todo? factorize with st_in_scripting rule?
   *)
  | [' ' '\t']+ { TCommentSpace(tokinfo lexbuf) }
  | NEWLINE { TCommentNewline(tokinfo lexbuf) }
  | "/*" {
        let info = tokinfo lexbuf in
        let buf = Buffer.create 127 in
        Buffer.add_string buf "/*";
        st_comment buf lexbuf;
        TComment(info |> Tok.rewrap_str (Buffer.contents buf))
     }
  | "/**/" { TComment(tokinfo lexbuf) }

  | "//" InputCharacter* { TComment(tokinfo lexbuf) }

  (* attribute management *)
  | XHPATTR { T_XHP_ATTR(tok lexbuf, tokinfo lexbuf) }
  | "="     { T_ASSIGN(tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "$" XHPATTR
     { Flag_parsing.sgrep_guard (T_XHP_ATTR(tok lexbuf, tokinfo lexbuf)) }
  | "..."
     { Flag_parsing.sgrep_guard (T_DOTS(tokinfo lexbuf)) }

  | ("'"|'"') as quote {
      let info = tokinfo lexbuf in
      let buf = Buffer.create 127 in
      string_quote quote buf lexbuf;
      let s = Buffer.contents buf in
      let buf2 = Buffer.create 127 in
      Buffer.add_char buf2 quote;
      Buffer.add_string buf2 s;
      Buffer.add_char buf2 quote;
      (* s does not contain the enclosing "'" but the info does *)
      T_STRING (s, info |> Tok.rewrap_str (Buffer.contents buf2))
    }

  | "{" {
      push_mode ST_IN_CODE;
      T_LCURLY(tokinfo lexbuf)
    }

  (* a singleton tag *)
  | "/>" {
      pop_mode ();
      T_XHP_SLASH_GT (tokinfo lexbuf)
    }

  (* When we see a ">", it means it's just the end of
   * the opening tag. Transit to IN_XHP_TEXT.
   *)
  | ">" {
      set_mode (ST_IN_XHP_TEXT current_tag);
      T_XHP_GT (tokinfo lexbuf)
    }

  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
  | _  {
        error("unrecognised symbol, in XHP tag:"^tok lexbuf) lexbuf;
        TUnknown (tokinfo lexbuf)
    }

(* ----------------------------------------------------------------------- *)
and st_in_xhp_text current_tag = parse

  (* a nested xhp construct *)
  | "<" (XHPTAG as tag) {

      push_mode (ST_IN_XHP_TAG tag);
      T_XHP_OPEN_TAG(tag, tokinfo lexbuf)
    }

  | "<" "/" (XHPTAG as tag) ">" {
      if (tag <> current_tag)
      then error (spf "XHP: wrong closing tag for, %s != %s" tag current_tag)
            lexbuf;
      pop_mode ();
      T_XHP_CLOSE_TAG(Some tag, tokinfo lexbuf)

    }
  (* shortcut for closing tag ? *)
  | "<" "/" ">" {
      (* no check :( *)
      pop_mode ();
      T_XHP_CLOSE_TAG(None, tokinfo lexbuf)
    }
  | "<!--" {
      let info = tokinfo lexbuf in
      let com = st_xhp_comment lexbuf in
      (* less: make a special token T_XHP_COMMENT? *)
      TComment(info |> tok_add_s com)
  }

  (* PHP interpolation. How the user can produce a { ? &;something ? *)
  | "{" {
      push_mode ST_IN_CODE;
      T_LCURLY(tokinfo lexbuf)
    }

  (* opti: *)
  | [^'<' '{']+ { T_XHP_TEXT (tok lexbuf, tokinfo lexbuf) }

  | eof { EOF (tokinfo lexbuf |> Tok.rewrap_str "") }
  | _  {
      error ("unrecognised symbol, in XHP text:" ^ tok lexbuf) lexbuf;
      TUnknown (tokinfo lexbuf)
    }

and st_xhp_comment = parse
  | "-->" { tok lexbuf }
  | [^'-']+ { let s = tok lexbuf in s ^ st_xhp_comment lexbuf }
  | "-"     { let s = tok lexbuf in s ^ st_xhp_comment lexbuf }
  | eof { error "end of file in xhp comment" lexbuf; "-->"}
  | _  {
      let s = tok lexbuf in
      error ("unrecognised symbol in xhp comment:"^s) lexbuf;
      s ^ st_xhp_comment lexbuf
    }
