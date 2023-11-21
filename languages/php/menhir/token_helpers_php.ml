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
open Parser_php
module PI = Lib_ast_fuzzy

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)

let is_eof = function
  | EOF _ -> true
  | _ -> false

let is_comment = function
  | T_COMMENT _
  | T_DOC_COMMENT _
  | TSpaces _
  | TNewline _ ->
      true
  | TCommentPP _ -> true
  | _ -> false

let is_just_comment = function
  | T_COMMENT _ -> true
  | _ -> false

let token_kind_of_tok t =
  match t with
  | TOBRACE _ -> PI.LBrace
  | TCBRACE _ -> PI.RBrace
  | TOPAR _
  | T_LAMBDA_OPAR _ ->
      PI.LPar
  | TCPAR _
  | T_LAMBDA_CPAR _ ->
      PI.RPar
  | T_COMMENT _
  | T_DOC_COMMENT _
  | TCommentPP _ ->
      PI.Esthet PI.Comment
  | TSpaces _ -> PI.Esthet PI.Space
  | TNewline _ -> PI.Esthet PI.Newline
  (* just after a ?>, the newline are not anymore TNewline but that *)
  | T_INLINE_HTML ("\n", _) -> PI.Esthet PI.Newline
  | _ -> PI.Other

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

(* Ugly repetitive code but ocamlyacc force us to do it that way.
 * Indeed the ocamlyacc token  cant be a pair of a sum type, it must be
 * directly a sum type. Fortunately most of the code was generated via an
 * emacs macro working on the type definition of token in parser_php.mli
 *)

let visitor_info_of_tok f = function
  | TOATTR ii -> TOATTR (f ii)
  | LDots ii -> LDots (f ii)
  | RDots ii -> RDots (f ii)
  | T_FROM ii -> T_FROM (f ii)
  | TUnknown ii -> TUnknown (f ii)
  | TSpaces ii -> TSpaces (f ii)
  | TNewline ii -> TNewline (f ii)
  | TCommentPP ii -> TCommentPP (f ii)
  | T_BOOL (s, ii) -> T_BOOL (s, f ii)
  | T_LNUMBER pi -> T_LNUMBER (Parsed_int.map_tok f pi)
  | T_DNUMBER (s, ii) -> T_DNUMBER (s, f ii)
  | T_IDENT (s, ii) -> T_IDENT (s, f ii)
  | T_METAVAR (s, ii) -> T_METAVAR (s, f ii)
  | T_STRING_VARNAME (s, ii) -> T_STRING_VARNAME (s, f ii)
  | T_VARIABLE (s, ii) -> T_VARIABLE (s, f ii)
  | T_NUM_STRING (s, ii) -> T_NUM_STRING (s, f ii)
  | T_INLINE_HTML (s, ii) -> T_INLINE_HTML (s, f ii)
  | T_ENCAPSED_AND_WHITESPACE (s, ii) -> T_ENCAPSED_AND_WHITESPACE (s, f ii)
  | T_CONSTANT_ENCAPSED_STRING (s, ii) -> T_CONSTANT_ENCAPSED_STRING (s, f ii)
  | T_ECHO ii -> T_ECHO (f ii)
  | T_PRINT ii -> T_PRINT (f ii)
  | T_DO ii -> T_DO (f ii)
  | T_WHILE ii -> T_WHILE (f ii)
  | T_ENDWHILE ii -> T_ENDWHILE (f ii)
  | T_FOR ii -> T_FOR (f ii)
  | T_ENDFOR ii -> T_ENDFOR (f ii)
  | T_FOREACH ii -> T_FOREACH (f ii)
  | T_ENDFOREACH ii -> T_ENDFOREACH (f ii)
  | T_SWITCH ii -> T_SWITCH (f ii)
  | T_ENDSWITCH ii -> T_ENDSWITCH (f ii)
  | T_CASE ii -> T_CASE (f ii)
  | T_DEFAULT ii -> T_DEFAULT (f ii)
  | T_BREAK ii -> T_BREAK (f ii)
  | T_CONTINUE ii -> T_CONTINUE (f ii)
  | T_RETURN ii -> T_RETURN (f ii)
  | T_GOTO ii -> T_GOTO (f ii)
  | T_TRY ii -> T_TRY (f ii)
  | T_CATCH ii -> T_CATCH (f ii)
  | T_FINALLY ii -> T_FINALLY (f ii)
  | T_THROW ii -> T_THROW (f ii)
  | T_DECLARE ii -> T_DECLARE (f ii)
  | T_ENDDECLARE ii -> T_ENDDECLARE (f ii)
  | T_USE ii -> T_USE (f ii)
  | T_GLOBAL ii -> T_GLOBAL (f ii)
  | T_AS ii -> T_AS (f ii)
  | T_SUPER ii -> T_SUPER (f ii)
  | T_FUNCTION ii -> T_FUNCTION (f ii)
  | T_CONST ii -> T_CONST (f ii)
  | T_STATIC ii -> T_STATIC (f ii)
  | T_ABSTRACT ii -> T_ABSTRACT (f ii)
  | T_FINAL ii -> T_FINAL (f ii)
  | T_ASYNC ii -> T_ASYNC (f ii)
  | T_PRIVATE ii -> T_PRIVATE (f ii)
  | T_PROTECTED ii -> T_PROTECTED (f ii)
  | T_PUBLIC ii -> T_PUBLIC (f ii)
  | T_VAR ii -> T_VAR (f ii)
  | T_UNSET ii -> T_UNSET (f ii)
  | T_ISSET ii -> T_ISSET (f ii)
  | T_EMPTY ii -> T_EMPTY (f ii)
  | T_CLASS ii -> T_CLASS (f ii)
  | T_ENUM ii -> T_ENUM (f ii)
  | T_INTERFACE ii -> T_INTERFACE (f ii)
  | T_EXTENDS ii -> T_EXTENDS (f ii)
  | T_IMPLEMENTS ii -> T_IMPLEMENTS (f ii)
  | T_OBJECT_OPERATOR ii -> T_OBJECT_OPERATOR (f ii)
  | T_ARROW ii -> T_ARROW (f ii)
  | T_DOUBLE_ARROW ii -> T_DOUBLE_ARROW (f ii)
  | T_LIST ii -> T_LIST (f ii)
  | T_ARRAY ii -> T_ARRAY (f ii)
  | T_CLASS_C ii -> T_CLASS_C (f ii)
  | T_METHOD_C ii -> T_METHOD_C (f ii)
  | T_FUNC_C ii -> T_FUNC_C (f ii)
  | T_LINE ii -> T_LINE (f ii)
  | T_FILE ii -> T_FILE (f ii)
  | T_DIR ii -> T_DIR (f ii)
  | T_COMMENT ii -> T_COMMENT (f ii)
  | T_DOC_COMMENT ii -> T_DOC_COMMENT (f ii)
  | T_OPEN_TAG ii -> T_OPEN_TAG (f ii)
  | T_OPEN_TAG_WITH_ECHO ii -> T_OPEN_TAG_WITH_ECHO (f ii)
  | T_CLOSE_TAG_OF_ECHO ii -> T_CLOSE_TAG_OF_ECHO (f ii)
  | T_CLOSE_TAG ii -> T_CLOSE_TAG (f ii)
  | T_START_HEREDOC ii -> T_START_HEREDOC (f ii)
  | T_END_HEREDOC ii -> T_END_HEREDOC (f ii)
  | T_DOLLAR_OPEN_CURLY_BRACES ii -> T_DOLLAR_OPEN_CURLY_BRACES (f ii)
  | T_CURLY_OPEN ii -> T_CURLY_OPEN (f ii)
  | TCOLCOL ii -> TCOLCOL (f ii)
  | T_EXIT ii -> T_EXIT (f ii)
  | T_IF ii -> T_IF (f ii)
  | TCOLON ii -> TCOLON (f ii)
  | TCOMMA ii -> TCOMMA (f ii)
  | TDOT ii -> TDOT (f ii)
  | TBANG ii -> TBANG (f ii)
  | TTILDE ii -> TTILDE (f ii)
  | TQUESTION ii -> TQUESTION (f ii)
  | TOBRA ii -> TOBRA (f ii)
  | TPLUS ii -> TPLUS (f ii)
  | TMINUS ii -> TMINUS (f ii)
  | TPOW ii -> TPOW (f ii)
  | TMUL ii -> TMUL (f ii)
  | TDIV ii -> TDIV (f ii)
  | TMOD ii -> TMOD (f ii)
  | TAND ii -> TAND (f ii)
  | TOR ii -> TOR (f ii)
  | TEQ ii -> TEQ (f ii)
  | TSMALLER ii -> TSMALLER (f ii)
  | TGREATER ii -> TGREATER (f ii)
  | T_PLUS_EQUAL ii -> T_PLUS_EQUAL (f ii)
  | T_MINUS_EQUAL ii -> T_MINUS_EQUAL (f ii)
  | T_MUL_EQUAL ii -> T_MUL_EQUAL (f ii)
  | T_DIV_EQUAL ii -> T_DIV_EQUAL (f ii)
  | T_CONCAT_EQUAL ii -> T_CONCAT_EQUAL (f ii)
  | T_MOD_EQUAL ii -> T_MOD_EQUAL (f ii)
  | T_AND_EQUAL ii -> T_AND_EQUAL (f ii)
  | T_OR_EQUAL ii -> T_OR_EQUAL (f ii)
  | T_XOR_EQUAL ii -> T_XOR_EQUAL (f ii)
  | T_SL_EQUAL ii -> T_SL_EQUAL (f ii)
  | T_SR_EQUAL ii -> T_SR_EQUAL (f ii)
  | T_INC ii -> T_INC (f ii)
  | T_DEC ii -> T_DEC (f ii)
  | T_BOOLEAN_OR ii -> T_BOOLEAN_OR (f ii)
  | T_BOOLEAN_AND ii -> T_BOOLEAN_AND (f ii)
  | T_BOOLEAN_PIPE ii -> T_BOOLEAN_PIPE (f ii)
  | T_LOGICAL_OR ii -> T_LOGICAL_OR (f ii)
  | T_LOGICAL_AND ii -> T_LOGICAL_AND (f ii)
  | T_LOGICAL_XOR ii -> T_LOGICAL_XOR (f ii)
  | T_SL ii -> T_SL (f ii)
  | T_SR ii -> T_SR (f ii)
  | T_IS_SMALLER_OR_EQUAL ii -> T_IS_SMALLER_OR_EQUAL (f ii)
  | T_IS_GREATER_OR_EQUAL ii -> T_IS_GREATER_OR_EQUAL (f ii)
  | T_BOOL_CAST ii -> T_BOOL_CAST (f ii)
  | T_INT_CAST ii -> T_INT_CAST (f ii)
  | T_DOUBLE_CAST ii -> T_DOUBLE_CAST (f ii)
  | T_STRING_CAST ii -> T_STRING_CAST (f ii)
  | T_ARRAY_CAST ii -> T_ARRAY_CAST (f ii)
  | T_OBJECT_CAST ii -> T_OBJECT_CAST (f ii)
  | T_UNSET_CAST ii -> T_UNSET_CAST (f ii)
  | T_IS_IDENTICAL ii -> T_IS_IDENTICAL (f ii)
  | T_IS_NOT_IDENTICAL ii -> T_IS_NOT_IDENTICAL (f ii)
  | T_IS_EQUAL ii -> T_IS_EQUAL (f ii)
  | T_IS_NOT_EQUAL ii -> T_IS_NOT_EQUAL (f ii)
  | TXOR ii -> TXOR (f ii)
  | T__AT ii -> T__AT (f ii)
  | T_NEW ii -> T_NEW (f ii)
  | T_CLONE ii -> T_CLONE (f ii)
  | T_INSTANCEOF ii -> T_INSTANCEOF (f ii)
  | T_INCLUDE ii -> T_INCLUDE (f ii)
  | T_INCLUDE_ONCE ii -> T_INCLUDE_ONCE (f ii)
  | T_REQUIRE ii -> T_REQUIRE (f ii)
  | T_REQUIRE_ONCE ii -> T_REQUIRE_ONCE (f ii)
  | T_EVAL ii -> T_EVAL (f ii)
  | T_ELSE ii -> T_ELSE (f ii)
  | T_ELSEIF ii -> T_ELSEIF (f ii)
  | T_ENDIF ii -> T_ENDIF (f ii)
  | TOPAR ii -> TOPAR (f ii)
  | TCPAR ii -> TCPAR (f ii)
  | T_LAMBDA_OPAR ii -> T_LAMBDA_OPAR (f ii)
  | T_LAMBDA_CPAR ii -> T_LAMBDA_CPAR (f ii)
  | TOBRACE ii -> TOBRACE (f ii)
  | TCBRACE ii -> TCBRACE (f ii)
  | TCBRA ii -> TCBRA (f ii)
  | TBACKQUOTE ii -> TBACKQUOTE (f ii)
  | TSEMICOLON ii -> TSEMICOLON (f ii)
  | TDOLLAR ii -> TDOLLAR (f ii)
  | TDOLLARDOLLAR ii -> TDOLLARDOLLAR (f ii)
  | TGUIL ii -> TGUIL (f ii)
  | T_ELLIPSIS ii -> T_ELLIPSIS (f ii)
  | T_YIELD ii -> T_YIELD (f ii)
  | T_AWAIT ii -> T_AWAIT (f ii)
  | T_SELF ii -> T_SELF (f ii)
  | T_PARENT ii -> T_PARENT (f ii)
  | T_TRAIT ii -> T_TRAIT (f ii)
  | T_INSTEADOF ii -> T_INSTEADOF (f ii)
  | T_TRAIT_C ii -> T_TRAIT_C (f ii)
  | T_TYPE ii -> T_TYPE (f ii)
  | T_NAMESPACE ii -> T_NAMESPACE (f ii)
  | TANTISLASH ii -> TANTISLASH (f ii)
  | T_NAMESPACE_C ii -> T_NAMESPACE_C (f ii)
  | EOF ii -> EOF (f ii)
  | T_ROCKET ii -> T_ROCKET (f ii)

let info_of_tok tok =
  let res = ref None in
  visitor_info_of_tok
    (fun ii ->
      res := Some ii;
      ii)
    tok
  |> ignore;
  match !res with
  | Some x -> x
  | None -> Tok.unsafe_fake_tok "NOTOK"

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

let line_of_tok tok =
  let info = info_of_tok tok in
  Tok.line_of_tok info
