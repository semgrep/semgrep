(* Yoann Padioleau, Matthew McQuaid
 *
 * Copyright (C) 2021-2022 Semgrep Inc.
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
open Common
open Either_
module T = Token_scala
module TH = Token_helpers_scala
module Flag = Flag_parsing
open Token_scala
open AST_scala
module AST = AST_scala
module Log = Log_parser_scala.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A recursive-descent Scala parser.
 *
 * This is mostly an OCaml port of the Scala2 parser found in
 * https://github.com/scala/scala/src/compiler/scala/tools/nsc/ast/parser/Parsers.scala
 *
 * alt:
 *  - use Parsers.scala of dotty? but the source is longer, and most of
 *    the code out there is still Scala2 code, so it's better for semgrep
 *    to focus on Scala 2 first.
 *  - use the one in scalameta? The code looks cleaner, but it handles
 *    more constructs because it handles also Scala3 (so it needs to manage
 *    indent/outdent). It might have been a better basis though, because on
 *    a second look (thx to Nathan) the code is far cleaner than Parsers.scala
 *    in the Scala 2 compiler source.
 *
 * Note that parsing Scala is difficult. See the crazy: tag in this file.
 * In some cases, parsing looks very contextual and crazy, because the
 * code looks at the AST built in previous steps to decide what to do
 * (e.g., the code around PatBind), but it's just that the parser
 * is using an LL(1) parsing style, so the parser sometimes accept
 * a more general expression first and later restrict what was possible.
 * In that case I use the not_crazy: tag when I think this could be solved
 * using a LALR(1) parser.
 *
 *
 * See also the newline: tag to see all the places where newlines are handled
 * in a special way.
 *
 * TODO:
 *  - see the AST: tag for what Parsers.scala was doing to build an AST.
 *    I use ast: when I return something from AST_scala.ml to cover the
 *    construct. I don't include the ast: for Nil or EmptyTree where I
 *    usually instead return a None or a [].
 * less:
 *  - see the CHECK: tag for what Parsers.scala was statically checking
 *  - see the STILL for what was TODO but may not be needed in the end as
 *    it seems we can parse most code without handling those
 *
 * Not handled (or partially handled) on purpose for now:
 *  - XML (deprecated constructs anyway)
 *  - symbolic literals (deprecated constructs anyway)
 *  - lots of semantic AST rewriting
 *     * stuff around placeholder '_' and implicit parameters
 *       CHECK: "unbound placeholder paramter"
 *       CHECK: "unbound wildcard type"
 *  - error recovery (skipping parens, braces, etc.)
 *  - advanced error diagnosis: lots of variants (deprecationWarning,
 *    syntaxError, expecting X but got Y, etc)
 *  - position/offset management (just use position info in the token, easier)
 *  - hooks for Scaladoc or IDEs
 *)

(* See also Flag_parsing.debug_parser and Flag_parsing.debug_lexer *)
let debug_newline = ref false

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type env = {
  (* imitating the Scala implementation *)
  mutable token : T.token;
  (* a stack of tokens which indicates whether line-ends can be statement
   *  separators also used for keeping track of nesting levels.
   *  We keep track of the closing symbol of a region. This can be
   *  RPAREN    if region starts with '('
   *  RBRACKET  if region starts with '['
   *  RBRACE    if region starts with '{'
   *  ARROW     if region starts with 'case'
   *  STRINGLIT if region is a string interpolation expression starting
   *   with '${' (the STRINGLIT appears twice in succession on the stack iff
   *              the expression is a multiline string literal).
   *)
  mutable sepRegions : T.token list;
  (* not in Scala implementation *)
  mutable rest : T.token list;
  mutable passed : T.token list;
  (* newline: last newline token *)
  mutable last_nl : Tok.t option;
  (* for logging *)
  mutable depth : int;
  (* is the current token indented? if so, at what line and width?
     this should be used in conjunction with nextToken/fetchToken
  *)
  mutable is_indented : (int * int) option;
}

let mk_env toks =
  match toks with
  | [] ->
      (* we should at least get an EOF token from the lexer *)
      raise Impossible
  | x :: xs ->
      {
        token = x;
        (* this assumes we will call first nextToken on it *)
        rest = x :: xs;
        passed = [];
        last_nl = None;
        depth = 0;
        (* We are implicitly in an indentation zone, as with the official Scala parser:
           https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L274
        *)
        sepRegions = [ T.DEDENT (0, 0) ];
        is_indented = None;
      }

(* We sometimes need to lookahead tokens, and call fetchToken and revert;
 * To do that we copy the environment to run fetchToken on a fresh copy. See
 * https://stackoverflow.com/questions/47688111/copy-construction-in-ocaml
 *)
let copy_env env = { env with token = env.token }

(* Trick to use = (called =~= below) to compare tokens *)
let ab = Tok.abstract_tok
let fb = Tok.fake_bracket
let noSelfType = None
let noMods = []

(* crazy? context-sensitive parsing? *)
type location = Local | InBlock | InTemplate
[@@deriving show { with_path = false }]

(* to correctly handle infix operators (in expressions, patterns, and types)*)
type mode = FirstOp | LeftOp | RightOp [@@deriving show { with_path = false }]

type indent_status =
  | Dedent of (* line *) int * (* width *) int
  | Indent of (* same *) int * int

(*****************************************************************************)
(* Logging/Dumpers  *)
(*****************************************************************************)
let n_dash n = Common2.repeat "--" n |> String.concat ""

let with_logging funcname f in_ =
  if !Flag.debug_parser then (
    let save = in_.depth in
    in_.depth <- in_.depth + 1;
    let depth = n_dash in_.depth in
    Log.debug (fun m -> m "%s>%s: %s" depth funcname (T.show in_.token));
    let res = f () in
    (* less: pass in_ ? *)
    Log.debug (fun m -> m "%s<%s: %s" depth funcname (T.show in_.token));
    in_.depth <- save;
    res)
  else f ()

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
let error x in_ =
  let tok = in_.token in
  let info = TH.info_of_tok tok in
  if !Flag.debug_parser then
    Log.err (fun m -> m "tok =%s, x = %s" (T.show tok) x);
  raise (Parsing_error.Syntax_error info)

let warning s = if !Flag.debug_parser then Log.warn (fun m -> m "%s" s)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
(* less: assert t1 is not a token with value (s, info) *)
let ( =~= ) t1 t2 = TH.abstract_info_tok t1 =*= TH.abstract_info_tok t2

(* to imitate Parsers.scala *)
let ( ++= ) aref xs = aref := xs @ !aref
let ( += ) aref x = aref := x :: !aref

let id_of_e_opt = function
  | Name (Id id, []) -> Some id
  (* TODO? can this happen? is it because we wrongly parse the self type? *)
  | Name (This (None, ii), []) -> Some (AST.this, ii)
  | ExprUnderscore ii -> Some (AST.wildcard, ii)
  | _ -> None

let param_of_e_opt e =
  match (e, id_of_e_opt e) with
  | _, Some id -> Some (id, None)
  | TypedExpr (e, _, t), None -> (
      match id_of_e_opt e with
      | Some id -> Some (id, Some t)
      | None -> None)
  | _ -> None

let convertToParam tk e =
  match param_of_e_opt e with
  | Some (id, None) -> ParamClassic (AST.basic_param id)
  | Some (id, Some t) ->
      ParamClassic { (AST.basic_param id) with p_type = Some (PT t) }
  | _ ->
      (* CHECK: "not a legal formal parameter" *)
      warning (spf "convertToParam: not handled %s" (AST.show_expr e));
      raise (Parsing_error.Syntax_error tk)

let convertToParams tk e =
  match e with
  | Tuple (lb, xs, rb) -> (lb, (xs |> List_.map (convertToParam tk), None), rb)
  | _ -> fb tk ([ convertToParam tk e ], None)

let makeMatchFromExpr e =
  match e with
  | BlockExpr (lb, BECases cases, rb) -> CatchCases (lb, cases, rb)
  | _ -> CatchExpr e

(*****************************************************************************)
(* Token helpers  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* looking ahead part 1 *)
(* ------------------------------------------------------------------------- *)

(* was called in.next.token *)
let rec in_next_token xs =
  match xs with
  | [] -> None
  | x :: xs -> (
      match x with
      | Space _
      | Comment _ ->
          in_next_token xs
      | _ -> Some x)

(* ------------------------------------------------------------------------- *)
(* indentation helpers *)
(* ------------------------------------------------------------------------- *)

(* If we do not do this, it's possible that we will be unable to start an
   indentation region within LBRACES, because we will look back and see no
   indentation region.

   For instance:

   { 5 match
       case _ => 2
   }

   ^ needs to start an indentation region before the `case`!
*)
let rec getLastIndentationRegion sepRegions =
  match sepRegions with
  | DEDENT (prev_line, width) :: _ -> (prev_line, width)
  | _ :: rest -> getLastIndentationRegion rest
  | [] -> raise Common.Impossible

let startsNewLine in_ =
  let rec loop = function
    | Nl _ :: _
    | NEWLINE _ :: _ ->
        true
    | Space _ :: rest
    | Comment _ :: rest
    | DEDENT _ :: rest ->
        loop rest
    | _ -> false
  in
  loop in_.passed

let getPreviousToken in_ =
  let rec loop passed =
    match passed with
    | DEDENT _ :: rest
    | NEWLINE _ :: rest
    | NEWLINES _ :: rest
    | Nl _ :: rest
    | Space _ :: rest
    | Comment _ :: rest ->
        loop rest
    | other :: _ -> Some other
    | [] -> None
  in
  loop in_.passed

(* Should we emit an INDENT or DEDENT before the current token? *)
let getIndentStatus ?(is_opportunistic = false) in_ =
  let tok = in_.token in
  let prevTok = getPreviousToken in_ in

  let prev_is_indent_token =
    match prevTok with
    | None -> false
    | Some tok -> TH.isIndentationToken tok
  in

  let info = TH.info_of_tok tok in
  let line = Tok.line_of_tok info in
  let col = Tok.col_of_tok info in

  let is_indented =
    (* Here, we need to look at the last indentation region that ever existed,
       because we want to still be able to start indentation regions inside of
       other ones.
    *)
    let _prev_indent_line, prev_indent_width =
      getLastIndentationRegion in_.sepRegions
    in
    col
    > prev_indent_width (* we are indented w.r.t. the old indentation region *)
    && (is_opportunistic || prev_is_indent_token)
       (* and the previous token can start an indent *)
    && (* and this token starts a new line *)
    startsNewLine in_
  in
  let is_dedented =
    (* Here, we look only at the current region, because we shouldn't be emitting
       dedents for a region if it's been interrupted by braces or parens.
    *)
    match in_.sepRegions with
    | DEDENT (_, prev_width) :: _ ->
        col < prev_width (* we are left w.r.t. the current indentation region *)
    | _ -> false
  in

  match tok with
  (* Don't emit before an EOF. *)
  | EOF _ -> None
  | __else__ ->
      if is_indented then Some (Indent (line, col))
      else if is_dedented then Some (Dedent (line, col))
      else None

(* Sometimes, we can emit an indent, irrespective of the specific token that we
   just passed. This is in certain contextual parsing positions, such as after
   the condition to an `if`, where we can't necessarily determine it based
   off tokens alone.

   This function allows that event to trigger.
*)
let opportunisticIndent in_ =
  match getIndentStatus ~is_opportunistic:true in_ with
  | Some (Indent (line, col)) -> in_.is_indented <- Some (line, col)
  | _ -> ()

(* ------------------------------------------------------------------------- *)
(* newline: Newline management part1  *)
(* ------------------------------------------------------------------------- *)
(* pad: newlines are skipped in fetchToken but reinserted in insertNL
 * in certain complex conditions.
 *)

(** Adapt sepRegions according to last token.
  * pad: called just at the beginning of nextToken so
  * in_.token = lastToken
*)
let adjustSepRegions lastToken in_ =
  let newRegions =
    match (lastToken, in_.sepRegions) with
    | LPAREN info, xs -> RPAREN info :: xs
    | LBRACKET info, xs -> RBRACKET info :: xs
    | LBRACE info, xs -> RBRACE info :: xs
    (* pad: the original code does something different for RBRACE,
     * I think for error recovery, and it does not raise an
     * error for mismatch.
     *)
    | (RPAREN _ | RBRACKET _ | RBRACE _), [] ->
        (* stricter: original code just does nothing *)
        error "unmatched closing token" in_
    | ((RPAREN _ | RBRACKET _ | RBRACE _) as x), y :: ys ->
        if x =~= y then ys
        else
          (* stricter: original code just does nothing *)
          error "unmatched closing token" in_
    | _, xs -> xs
  in
  in_.sepRegions <- newRegions;
  ()

let inSepRegion tok f in_ =
  let cur = in_.sepRegions in
  in_.sepRegions <- tok :: cur;
  let res = f () in
  in_.sepRegions <- cur;
  res

(* newline: pad: I've added the ~newlines param to differentiante adding
 * a NEWLINE or NEWLINES *)
let insertNL ?(newlines = false) in_ =
  if !debug_newline then (
    Log.debug (fun m -> m "%s: %s" "insertNL" (T.show in_.token));
    Log.debug (fun m ->
        m "inserting back a newline:%s" (Dumper.dump in_.last_nl)));
  match in_.last_nl with
  | None -> error "IMPOSSIBLE? no last newline to insert back" in_
  | Some x ->
      in_.rest <- in_.token :: in_.rest;
      in_.token <- (if newlines then NEWLINES x else NEWLINE x);
      ()

(** Is current token first one after a newline? *)
let afterLineEnd in_ =
  let rec loop passed =
    match passed with
    | x :: xs -> (
        match x with
        | Nl _
        | NEWLINE _
        | NEWLINES _ ->
            true
        | Space _
        | Comment _
        | DEDENT _ ->
            loop xs
        | _ ->
            if !debug_newline then
              Log.debug (fun m ->
                  m "%s: false because %s" "afterLineEnd" (T.show x));
            false)
    | [] -> false
  in
  loop in_.passed |> fun b ->
  if !debug_newline then
    Log.debug (fun m ->
        m "%s: %s, result = %b" "afterLineEnd" (T.show in_.token) b);
  b

(* ------------------------------------------------------------------------- *)
(* Lexing tricks *)
(* ------------------------------------------------------------------------- *)
let postProcessToken _in_ =
  warning "postProcessToken";
  ()

(* ------------------------------------------------------------------------- *)
(* fetchToken *)
(* ------------------------------------------------------------------------- *)

let fetchToken in_ =
  in_.is_indented <- None;
  let rec loop aux =
    match in_.rest with
    | [] -> error "IMPOSSIBLE? fetchToken: no more tokens" in_
    | x :: xs -> (
        if !Flag.debug_lexer then
          Log.debug (fun m -> m "fetchToken: %s" (T.show x));

        in_.rest <- xs;

        match x with
        | Space _
        | Comment _ ->
            loop (x :: aux)
        (* pad: the newline is skipped here, but reinserted conditionally in
         * insertNL() *)
        | Nl info ->
            in_.last_nl <- Some info;
            loop (x :: aux)
        (* those tokens are inserted post tokenization *)
        | NEWLINE _
        | NEWLINES _ ->
            raise Impossible
        | _ ->
            in_.passed <- aux @ in_.passed;
            in_.token <- x)
  in
  loop [ in_.token ]

(* ------------------------------------------------------------------------- *)
(* nextToken *)
(* ------------------------------------------------------------------------- *)

(** Consume and discard the next token. *)
let nextToken in_ =
  let lastToken = in_.token in
  adjustSepRegions lastToken in_;

  (* STILL?: if inStringInterpolation fetchStringPart else *)
  fetchToken in_;
  (* Insert NEWLINE or NEWLINES if
   * - we are after a newline
   * - we are within a { ... } or on toplevel (wrt sepRegions)
   * - the current token can start a statement and the one before can end it
   * insert NEWLINES if we are past a blank line, NEWLINE otherwise
   *)
  (* newline: *)
  (if
     afterLineEnd in_ && TH.inLastOfStat lastToken && TH.inFirstOfStat in_.token
     && (match in_.sepRegions with
        | []
        | RBRACE _ :: _
        (* See "note: sepFor" below. *)
        | Kfor _ :: _
        | DEDENT _ :: _ ->
            true
        | _ -> false)
     && (* STILL?: not applyBracePatch *)
     true
   then
     match () with
     (* STILL?: | _ when pastBlankLine in_ -> insertNL ~newlines:true in_ *)
     (* STILL?: | _ when TH.isLeadingInfixOperator *)
     (* CHECK: scala3: "Line starts with an operator that in future" *)
     | _ -> insertNL in_
   else
     (* Determine if this next token should emit an INDENT or DEDENT *)
     (* According to the Dotty compiler, emitting a NEWLINE is exclusive with
        emitting an INDENT or DEDENT.
        https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L596
     *)
     let indent_status = getIndentStatus in_ in
     match indent_status with
     | Some (Indent (line, width)) ->
         (* Set our indented flag to true.
             This bool will give us the option to enter an indentation
             region, we might not necessarily have to.
         *)
         in_.is_indented <- Some (line, width)
     | Some (Dedent (line, width)) ->
         (* Push a DEDENT token onto our token stream.
             This is so we can consume it in `statSeq`

             This results in significantly simpler code, in the case
             of nested DEDENTs (due to the ending of multiple indent regions)
         *)
         in_.rest <- in_.token :: in_.rest;
         in_.token <- DEDENT (line, width)
     | None -> ());
  postProcessToken in_;
  ()

let nextTokenAllow _next in_ =
  warning "nextTokenAllow: STILL?";
  nextToken in_

let skipToken in_ = nextToken in_
let init in_ = nextToken in_

(* Consume one token of the specified type, or signal an error if it is
 * not there *)
let accept t in_ =
  if not (in_.token =~= t) then error (spf "was expecting: %s" (T.show t)) in_;
  match t with
  | EOF _ -> ()
  | _ -> nextToken in_

(* ------------------------------------------------------------------------- *)
(* looking ahead part 2 *)
(* ------------------------------------------------------------------------- *)

let lookingAhead body in_ =
  (* CHECK: allowLeadingInfixOperators *)
  let in_' = copy_env in_ in
  nextToken in_';
  let res = body in_' in
  res

let lookingAhead2 body1 body2 in_ =
  (* CHECK: allowLeadingInfixOperators *)
  let in_' = copy_env in_ in
  nextToken in_';
  let res1 = body1 in_' in
  match in_'.token with
  | EOF _ -> false
  | _ ->
      nextToken in_';
      let res2 = body2 in_' in
      res1 && res2

let rec skipMatching in_ =
  let opening = in_.token in
  let closing =
    match in_.token with
    | LPAREN _ -> RPAREN ab
    | LBRACKET _ -> RBRACKET ab
    | _ -> failwith "precondition failure"
  in
  skipToken in_;
  while not (in_.token =~= EOF ab || in_.token =~= closing) do
    if in_.token =~= opening then skipMatching in_ else skipToken in_
  done;
  skipToken in_

(* ------------------------------------------------------------------------- *)
(* indentation *)
(* ------------------------------------------------------------------------- *)

(* Have we passed an indent?
   If so, the `is_indented` flag should be set by our `nextToken` logic.
*)
let passedIndent in_ = in_.is_indented

(* It's not always the case that a colon means we must start a
   colon and then indented block.

   This function just helps us discern if we're really going to be
   starting an indented block.
*)
let isIndentAfterColon in_ =
  match in_.token with
  | COLON _ -> lookingAhead (fun in_ -> Option.is_some (passedIndent in_)) in_
  | _ -> false

(* Adjust sepRegions to enter a new indent region, if we've
   passed by one.
*)
let enterIndentRegion in_ =
  let indent_opt = passedIndent in_ in
  match indent_opt with
  | None -> ()
  | Some (line, width) ->
      (* We don't want to recurse forever in entering
         new indent regions.
         If we consume this indent region here, there's no reason
         to need to use it again.
      *)
      in_.is_indented <- None;
      in_.sepRegions <- DEDENT (line, width) :: in_.sepRegions

(* Adjust sepREgions to end the current indent region, if we're
   at the end.
*)
let closeIndentRegion in_ =
  match (in_.token, in_.sepRegions) with
  | EOF _, _ -> ()
  (* This check that the DEDENTs match each other was already
     done, or we wouldn't have stopped on this token in the
     first place.
  *)
  | DEDENT _, DEDENT _ :: rest ->
      in_.sepRegions <- rest;
      skipToken in_
  | _ -> failwith "expected closing dedent (probably not emitted correctly)"

(* If this next token is indented, it's quite likely we're inside of an implicit
   indented blockExpr, which can start off an `expr`.

   For instance, something like
   def foo() =
     val x = 3

   It's not guaranteed, however, such as if we saw something like

   def foo() =
     3

   where `{ 3 }` is not a valid BlockExpr.

   So we will only assume it's an implicit blockExpr if it's both indented,
   and not the start to an expression.
*)
let isIndentedBlockExprStart in_ =
  Option.is_some (passedIndent in_) && not (TH.isExprIntro in_.token)

(*****************************************************************************)
(* Special parsing  *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* case class: Check that we are parsing a case that is not part of case class or case object *)
(* ------------------------------------------------------------------------- *)

let nextTokNotClassOrObject =
  lookingAhead (fun in_ ->
      match in_.token with
      | Kclass _
      | Kobject _ ->
          false
      | _ -> true)

(* ------------------------------------------------------------------------- *)
(* newline: Newline management part2  *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  semi = nl {nl} | `;`
 *  nl  = `\n` // where allowed
 *  }}}
*)
let acceptStatSep in_ =
  in_
  |> with_logging "acceptStatSep" (fun () ->
         match in_.token with
         | NEWLINE _
         | NEWLINES _ ->
             nextToken in_
         | _ -> accept (SEMI ab) in_)

let acceptStatSepOpt in_ =
  if not (TH.isStatSeqEnd in_.token) then acceptStatSep in_

let newLineOpt in_ =
  match in_.token with
  | NEWLINE _ -> nextToken in_
  | _ -> ()

let newLinesOpt in_ =
  match in_.token with
  | NEWLINE _
  | NEWLINES _ ->
      nextToken in_
  | _ -> ()

let newLineOptWhenFollowedBy token in_ =
  match (in_.token, in_next_token in_.rest) with
  | NEWLINE _, Some x when x =~= token -> newLineOpt in_
  | _ -> ()

let newLineOptWhenFollowing ftok in_ =
  match (in_.token, in_next_token in_.rest) with
  | NEWLINE _, Some x when ftok x -> newLineOpt in_
  | _ -> ()

(* ------------------------------------------------------------------------- *)
(* Trailing commas  *)
(* ------------------------------------------------------------------------- *)
(* pad: trailing commas are _detected_ in separatedToken to not cause
 * separatedToken to call part another time, but they are _skipped_ in
 * inGroupers.
 *)

(** used by parser to distinguish pattern P(_*, p) from trailing comma.
 *  EOF is accepted for REPL, which can't look ahead past the current line.
*)
let isTrailingComma right in_ =
  in_.token =~= COMMA ab
  && lookingAhead
       (fun in_ ->
         afterLineEnd in_ && in_.token =~= right (* REPL: || token =~= EOF *))
       in_

(* advance past COMMA NEWLINE RBRACE (to whichever token is the matching
 * close bracket)
 *)
(* pad: this was returning a boolean in the original code *)
let skipTrailingComma right in_ =
  match in_.token with
  | COMMA _ when isTrailingComma right in_ ->
      (* SIP-27 Trailing Comma (multi-line only) support
       * If a comma is followed by a new line & then a closing paren,
       * bracket or brace
       * then it is a trailing comma and is ignored
       *)
      (* pad: why not skipToken? don't want side effects in nextToken? *)
      fetchToken in_
  | _ -> ()

(* ------------------------------------------------------------------------- *)
(* Context sensitive parsing  *)
(* ------------------------------------------------------------------------- *)
(* The implementation for parsing inside of patterns at points where
 * sequences are disallowed.
 *)
let noSeq f in_ =
  warning "noSeq: STILL?";
  f in_

(* The implementation for parsing inside of patterns at points where
 * sequences are allowed.
 *)
let seqOK f in_ =
  warning "seqOK: STILL?";
  f in_

(* The implementation of the context sensitive methods for parsing
 * outside of patterns.
 *)
let outPattern f in_ =
  warning "outPattern: STILL?";
  f in_

(*****************************************************************************)
(* Grammar helpers  *)
(*****************************************************************************)

let inGroupers left right body in_ =
  let lp = TH.info_of_tok in_.token in
  accept left in_;
  let res = body in_ in
  skipTrailingComma right in_;
  let rp = TH.info_of_tok in_.token in
  accept right in_;
  (lp, res, rp)

let inBraces f in_ =
  in_
  |> with_logging "inBraces" (fun () ->
         inGroupers (LBRACE ab) (RBRACE ab) f in_)

let inParens f in_ =
  in_
  |> with_logging "inParens" (fun () ->
         inGroupers (LPAREN ab) (RPAREN ab) f in_)

let inBrackets f in_ =
  in_
  |> with_logging "inBrackets" (fun () ->
         inGroupers (LBRACKET ab) (RBRACKET ab) f in_)

let inIndented f in_ =
  in_
  |> with_logging "inIndented" (fun () ->
         enterIndentRegion in_;
         let res = fb (Tok.unsafe_fake_tok "") (f in_) in
         closeIndentRegion in_;
         res)

(* less: do actual error recovery? *)
let inBracesOrNil = inBraces

let inBracesOrIndented f in_ =
  match in_.token with
  | LBRACE _ -> inBraces f in_
  | _ -> inIndented f in_

let inBracesOrColonIndented f in_ =
  match in_.token with
  | LBRACE _ -> inBraces f in_
  | _ ->
      accept (COLON ab) in_;
      inIndented f in_

(** {{{ { `sep` part } }}}. *)
let separatedToken sep part in_ =
  (* CHECK: "separator cannot be a comma" *)
  in_
  |> with_logging
       (spf "separatedToken(%s)" (T.show sep))
       (fun () ->
         let ts = ref [] in
         while in_.token =~= sep do
           let ii = TH.info_of_tok in_.token in
           nextToken in_;
           let x = part ii in_ in
           ts += x
         done;
         List.rev !ts)

(** {{{ part { `sep` part } }}}. *)
let tokenSeparated separator part in_ =
  in_
  |> with_logging
       (spf "tokenSeparated(%s)" (T.show separator))
       (fun () ->
         let ts = ref [] in
         let x = part in_ in
         ts += x;
         let done_ = ref (not (in_.token =~= separator)) in
         while not !done_ do
           let skippable =
             separator =~= COMMA ab
             &&
             match in_.sepRegions with
             | [] -> false
             | head :: _ ->
                 (* usually an RPAREN, but also RBRACE in import! *)
                 isTrailingComma head in_
           in
           if not skippable then (
             nextToken in_;
             let x = part in_ in
             ts += x);
           done_ := (not (in_.token =~= separator)) || skippable
         done;
         List.rev !ts)

(* AST: Creates an actual Parens node (only used during parsing.) *)
let makeParens body in_ =
  inParens
    (fun in_ ->
      match in_.token with
      | RPAREN _ -> []
      | _ -> body in_)
    in_

(* Strip the artificial `Parens` node to create a tuple term Tree. *)
let stripParens x =
  (* AST: if Parens(ts) -> makeSafeTupleTerm(ts) *)
  x

(** {{{ tokenSeparated }}}, with the separator fixed to commas. *)
let commaSeparated part in_ = tokenSeparated (COMMA ab) part in_

(*****************************************************************************)
(* Parsing names  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Idents *)
(* ------------------------------------------------------------------------- *)

let ident_opt in_ : ident option =
  match TH.isIdent in_.token with
  | Some (s, info) ->
      nextToken in_;
      Some (s, info)
  | None -> None

(* AST: Assumed to be TermNames *)
let ident in_ : ident =
  match ident_opt in_ with
  | Some x -> x
  | None -> error "expecting ident" in_

let rawIdent in_ : ident =
  (* ast: in.name *)
  ident in_

let identOrMacro in_ : ident =
  if TH.isMacro in_.token then ident in_ else rawIdent in_

let wildcardOrIdent in_ : ident =
  match in_.token with
  | USCORE ii ->
      nextToken in_;
      (* ast: nme.WILDCARD *)
      (AST.wildcard, ii)
  | _ -> ident in_

(* For when it's known already to be a type name. *)
let identForType in_ : ident =
  let x = ident in_ in
  (* ast: x.toTypeName *)
  x

(* ------------------------------------------------------------------------- *)
(* Selectors *)
(* ------------------------------------------------------------------------- *)

let selector in_ : ident =
  let id = ident in_ in
  (* ast: Select(t, id) *)
  id

let rec selectors ~typeOK in_ : dotted_ident =
  match in_.token with
  | Ktype ii when typeOK ->
      nextToken in_;
      (* AST: SingletonTypeTree(t) *)
      [ ("type", ii) ]
  | _ ->
      let t1 = selector in_ in
      if in_.token =~= DOT ab then (
        skipToken in_;
        t1 :: selectors ~typeOK in_)
      else [ t1 ]

(* ------------------------------------------------------------------------- *)
(* Paths *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *   QualId ::= Id {`.` Id}
 *   }}}
*)
let qualId in_ : dotted_ident =
  (* ast: Ident(id) *)
  let id = ident in_ in
  match in_.token with
  | DOT _ ->
      skipToken in_;
      id :: selectors ~typeOK:false in_
  | _ -> [ id ]

(* Calls `qualId()` and manages some package state. *)
let pkgQualId in_ : dotted_ident =
  (* ast: if ... then inScalePackage = true *)
  let pkg = qualId in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  (* ast: adjust currentPackage *)
  pkg

(** {{{
 *   MixinQualifier ::= `[` Id `]`
 *   }}}
*)
let mixinQualifierOpt in_ : ident bracket option =
  if in_.token =~= LBRACKET ab then Some (inBrackets identForType in_)
  else None (* ast: tpnme.EMPTY *)

(** {{{
 *  Path       ::= StableId
 *              |  [Ident `.`] this
 *  AnnotType ::= Path [`.` type]
 *  }}}
*)
let path ~thisOK ~typeOK in_ : path =
  in_
  |> with_logging (spf "path(thisOK:%b, typeOK:%b)" thisOK typeOK) (fun () ->
         match in_.token with
         | Kthis ii ->
             nextToken in_;
             (* ast: t := This(tpnme.Empty) *)
             let t = This (None, ii) in
             if (not thisOK) || in_.token =~= DOT ab then (
               accept (DOT ab) in_;
               (t, selectors ~typeOK in_))
             else (t, [])
         | Ksuper ii ->
             nextToken in_;
             let mixins = mixinQualifierOpt in_ in
             (* ast: t := Super(This(tpnme.EMPTY), x *)
             accept (DOT ab) in_;
             let x = selector in_ in
             let t = Super (None, ii, mixins, x) in
             if in_.token =~= DOT ab then (
               skipToken in_;
               (t, selectors ~typeOK in_))
             else (t, [])
         | _ ->
             let name = ident in_ in
             (* ast: t := Ident(name) and special stuff in BACKQUOTED_IDENT *)
             if in_.token =~= DOT ab then (
               skipToken in_;
               match in_.token with
               | Kthis ii ->
                   nextToken in_;
                   (* ast: t = This(name.toTypeName) *)
                   let t = This (Some name, ii) in
                   if (not thisOK) || in_.token =~= DOT ab then (
                     accept (DOT ab) in_;
                     (t, selectors ~typeOK in_))
                   else (t, [])
               | Ksuper ii ->
                   (* pad: factorize with above Ksuper case *)
                   nextToken in_;
                   let mixins = mixinQualifierOpt in_ in
                   (* ast: Super(This(name.toTypeName), x) *)
                   accept (DOT ab) in_;
                   let x = selector in_ in
                   let t = Super (Some name, ii, mixins, x) in
                   if in_.token =~= DOT ab then (
                     skipToken in_;
                     (t, selectors ~typeOK in_))
                   else (t, [])
               | _ -> (Id name, selectors ~typeOK in_))
             else (Id name, []))

(** {{{
 *  StableId ::= Id
 *            |  Path `.` Id
 *            |  [id `.`] super [`[` id `]`]`.` id
 *  }}}
*)
let stableId in_ : stable_id = path ~thisOK:false ~typeOK:false in_

(*****************************************************************************)
(* Forward references  *)
(*****************************************************************************)
(* The Scala constructs are mutually recursive, like in most languages.
 * We should use a long list of let rec xxx ... and yyy  ... but
 * but it helps a bit to structure the parser to reduce those set of
 * mutual dependencies by using forward references.
 * alt: have all functions mutually recursive
 *)
let interpolatedString_ =
  ref (fun ~inPattern _ ->
      ignore inPattern;
      failwith "forward ref not set")

let exprTypeArgs_ = ref (fun _ -> failwith "forward ref not set")
let annotTypeRest_ = ref (fun _ -> failwith "forward ref not set")
let template_ = ref (fun _ -> failwith "forward ref not set")
let defOrDcl_ = ref (fun _ _ -> failwith "forward ref not set")
let tmplDef_ = ref (fun _ -> failwith "forward ref not set")
let blockStatSeq_ = ref (fun _ -> failwith "forward ref not set")

let indentedExprOrBlockStatSeqUntil_ =
  ref (fun _ ~until:_ -> failwith "forward ref not set")

let packageOrPackageObject_ = ref (fun _ _ -> failwith "forward ref not set")
let typeCaseClauses_ = ref (fun _ -> failwith "forward ref not set")

let paramClauseInner_ =
  ref (fun ~caseParam:_ ~endTok:_ _ _ -> failwith "forward ref not set")

let paramType_ =
  ref (fun ?repeatedParameterOK _ ->
      ignore repeatedParameterOK;
      failwith "forward ref not set")

(*****************************************************************************)
(* Literal  *)
(*****************************************************************************)

(** {{{
 *  SimpleExpr    ::= literal
 *                  | symbol
 *                  | null
 *  }}}
*)
let literal ?isNegated ?(inPattern = false) in_ : literal =
  in_
  |> with_logging
       (spf "literal(isNegated:%s, inPattern:%b)" (Dumper.dump isNegated)
          inPattern)
       (fun () ->
         let finish value_ =
           (* ast: newLiteral(value) *)
           nextToken in_;
           value_
         in
         let negate ~f x =
           match isNegated with
           | None -> x
           | Some iminus -> f iminus x
         in
         (* less: check that negate only on Int or Float *)
         match in_.token with
         | T_INTERPOLATED_START (s, ii) ->
             (* AST: if not inPattern then withPlaceholders(...) *)
             let xs, endt = !interpolatedString_ ~inPattern in_ in
             Interpolated ((s, ii), xs, endt)
         (* scala3: unsupported, deprecated in 2.13.0 *)
         (* ast: Apply(scalaDot(Symbol, List(finish(in.strVal)) *)
         | SymbolLiteral (tcolon, id) -> finish (Symbol (tcolon, id))
         | CharacterLiteral (x, ii) ->
             (* ast: incharVal *)
             finish (Char (x, ii))
         | IntegerLiteral pi ->
             (* ast: in.intVal(isNegated) *)
             finish
               (Int
                  (negate
                     ~f:(fun iminus pi ->
                       Parsed_int.map_tok
                         (fun tok -> Tok.combine_toks iminus [ tok ])
                         pi
                       |> Parsed_int.neg)
                     pi))
         | FloatingPointLiteral (x, ii) ->
             (* ast: in.floatVal(isNegated)*)
             finish
               (Float
                  (negate
                     ~f:(fun iminus (x, ii) ->
                       let ii = Tok.combine_toks iminus [ ii ] in
                       match x with
                       | None -> (x, ii)
                       | Some i -> (Some (-.i), ii))
                     (x, ii)))
         | StringLiteral (x, ii) ->
             (* ast: in.strVal.intern() *)
             finish (String (x, ii))
         | BooleanLiteral (x, ii) ->
             (* ast: bool *)
             finish (Bool (x, ii))
         | Knull ii ->
             (* ast: null *)
             finish (Null ii)
         | _ -> error "illegal literal" in_)

(*****************************************************************************)
(* Infix Expr/Types/pattern management  *)
(*****************************************************************************)

let pushOpInfo _topTODO in_ : ident * type_ list bracket option =
  let x = ident in_ in
  let targs =
    if in_.token =~= LBRACKET ab then Some (!exprTypeArgs_ in_) else None
  in
  (* AST: OpInfo(top, name, targs) *)
  (x, targs)

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* in PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

(* A TypedFunParam looks like "id ':' Type"
   but it might actually be just a series of comma-separated types
   so let's use two lookahead to see whether it's just an IDENT and
   then COLON.
*)
let is_typed_fun_param_after_lparen in_ =
  lookingAhead
    (fun in_ ->
      if TH.isIdentBool in_.token then (
        nextToken in_;
        in_.token =~= COLON ab)
      else false)
    in_

(** {{{
 *  Type ::= InfixType `=>` Type
 *         | `(` [`=>` Type] `)` `=>` Type
 *         | InfixType [ExistentialClause]
 *         | InfixType `match` <<< TypeCaseClauses >>>
 *         | FunType
 *  ExistentialClause ::= forSome `{` ExistentialDcl {semi ExistentialDcl} `}`
 *  ExistentialDcl    ::= type TypeDcl | val ValDcl
 *  }}}
*)
let rec typ in_ : type_ =
  in_
  |> with_logging "typ" (fun () ->
         (* CHECK: placeholderTypeBoundary *)
         let t =
           match in_.token with
           | LBRACKET _ -> funType in_
           | LPAREN _ when is_typed_fun_param_after_lparen in_ -> funType in_
           | LPAREN _ -> tupleInfixType in_
           | _ -> infixType FirstOp in_
         in
         match in_.token with
         | ARROW ii ->
             skipToken in_;
             let t2 = typ in_ in
             (* ast: makeFunctionTypeTree t t2 *)
             TyFunction1 (t, ii, t2)
         | KforSome ii ->
             skipToken in_;
             makeExistentialTypeTree t ii in_
         | Kmatch ii ->
             skipToken in_;
             let _, type_case_clauses, _ = inBraces !typeCaseClauses_ in_ in
             TyMatch (t, ii, type_case_clauses)
         | _ -> t)

(** {{{
 *  FunType ::= FunTypeArgs (`=>` | `?=>') Type
 *            | HKTypeParamClause `=>` Type
 *
 *  FunTypeArgs   ::=  InfixType
 *                  |  ‘(’ [ FunArgTypes ] ‘)’
 *                  |  FunParamClause
 *
 * FunParamClause ::= ‘(’ TypedFunParam {‘,’ TypedFunParam } ‘)’
 * TypedFunParam  ::= id ‘:’ Type
 * }}}
*)

and funType in_ : type_ =
  let owner = ("", Tok.unsafe_fake_tok "") in
  match in_.token with
  | LBRACKET _ ->
      (* I still don't know why this is necessary. *)
      let caseParam = ref [] in
      let _, (params, _), _ =
        inBrackets
          (fun in_ ->
            !paramClauseInner_ ~caseParam ~endTok:(RBRACKET ab) owner in_)
          in_
      in
      let ii = TH.info_of_tok in_.token in
      skipToken in_;
      let ty = typ in_ in
      TyPoly (params, ii, ty)
  | _ -> (
      let type_args = funTypeArgs in_ in
      let ii = TH.info_of_tok in_.token in
      (* one of two kinds of arrows *)
      skipToken in_;
      let ty = typ in_ in
      match type_args with
      | Left3 ty_args -> TyDependent (ty_args, ii, ty)
      | Middle3 tys -> TyFunction2 (tys, ii, ty)
      | Right3 l_ty -> TyFunction1 (l_ty, ii, ty))

and funTypeArgs in_ :
    ((ident * type_) list, type_ list bracket, type_) Either_.either3 =
  match in_.token with
  | LPAREN _ ->
      if is_typed_fun_param_after_lparen in_ then
        inParens (commaSeparated typedFunParam) in_ |> Tok.unbracket |> fun l ->
        Left3 l
      else inParens types in_ |> fun x -> Middle3 x
  | _ -> Right3 (infixType FirstOp in_)

and typedFunParam in_ : ident * type_ =
  let id = ident in_ in
  let _ii = TH.info_of_tok in_.token in
  accept (COLON ab) in_;
  let ty = typ in_ in
  (id, ty)

(** {{{
 *  InfixType ::= CompoundType {id [nl] CompoundType}
 *  }}}
*)
(* pad: similar to infixExpr, but seems very rarely used in Scala projects.
*)
and infixType mode in_ : type_ =
  in_
  |> with_logging
       (spf "infixType(%s)" (show_mode mode))
       (fun () ->
         (* CHECK: placeholderTypeBoundary *)
         let x = compoundType in_ in
         infixTypeRest x mode in_)

and infixTypeRest t _modeTODO in_ : type_ =
  in_
  |> with_logging "infixTypeRest" (fun () ->
         (* Detect postfix star for repeated args.
          * Only RPAREN can follow, but accept COMMA and EQUALS for error's sake.
          * Take RBRACE as a paren typo.
          *)
         let checkRepeatedParam in_ =
           if TH.isRawStar in_.token then
             lookingAhead
               (fun in_ ->
                 match in_.token with
                 | RPAREN ii (* the good one *)
                 | COMMA ii
                 | EQUALS ii (* error recovery *)
                 | RBRACE ii (* probably typo *) ->
                     Some ii
                 | _ -> None)
               in_
           else None
         in
         let asInfix in_ : type_ =
           (* AST: let leftAssoc = nme.isLeftAssoc(in.name) *)
           let leftAssoc = false in
           warning "InfixTypeRest: leftAssoc??";
           (* AST: if (mode != InfixMode.FirstOp) checkAssoc ... *)
           let tycon = identForType in_ in
           newLineOptWhenFollowing TH.isTypeIntroToken in_;
           (* AST: let mkOp t1 = AppliedTypeTree(tycon, List(t, t1)) *)
           if leftAssoc then
             let t2 = compoundType in_ in
             let x = TyInfix (t, tycon, t2) in
             (* ast: mkOp(x) *)
             infixTypeRest x LeftOp in_
           else
             let t2 = infixType RightOp in_ in
             (* ast: mkOp(x) *)
             TyInfix (t, tycon, t2)
         in
         if TH.isIdentBool in_.token then
           match checkRepeatedParam in_ with
           | None -> asInfix in_
           | Some ii -> TyRepeated (t, ii)
         else t)

(* () must be () => R; (types) could be tuple or (types) => R *)
and tupleInfixType in_ : type_ =
  in_
  |> with_logging "tupleInfixType" (fun () ->
         if not (in_.token =~= LPAREN ab) then
           error "first token must be a left parenthesis" in_;
         let ts =
           inParens
             (fun in_ ->
               match in_.token with
               | RPAREN _ -> []
               | _ -> functionTypes in_)
             in_
         in
         match in_.token with
         | ARROW ii ->
             skipToken in_;
             let t = typ in_ in
             (* ast: makeSafeFunctionType(ts, t) *)
             TyFunction2 (ts, ii, t)
         (* CHECK: if is.isEmpty "Illegal literal type (), use Unit instead" *)
         | _ ->
             (* CHECK: ts foreach checkNotByNameOrVarargs *)
             (* ast: makeSafeTupleType(ts) *)
             let tuple = TyTuple ts in
             let str = simpleTypeRest tuple in_ in
             let atr = !annotTypeRest_ str in_ in
             let ctr = compoundTypeRest (Some atr) in_ in
             infixTypeRest ctr FirstOp in_)

(** {{{
 *  SimpleType       ::=  SimpleType TypeArgs
 *                     |  SimpleType `#` Id
 *                     |  StableId
 *                     |  Path `.` type
 *                     |  Literal
 *                     |  `?` TypeBounds
 *                     |  `(` Types `)`
 *                     |  WildcardType
 *  }}}
*)
and simpleType in_ : type_ =
  in_
  |> with_logging "simpleType" (fun () ->
         match in_.token with
         | t when TH.isLiteral t && not (t =~= Knull ab) ->
             let x = literal in_ in
             (* ast: SingletonTypeTree(x) *)
             (* scala3: restrict to simple_literal *)
             TyLiteral x
         | MINUS ii when lookingAhead (fun in_ -> TH.isNumericLit in_.token) in_
           ->
             nextToken in_;
             let x = literal ~isNegated:ii in_ in
             (* ast: SingletonTypeTree(x) *)
             TyLiteral x
         (* See https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html *)
         | OP ("?", _) ->
             let ii = TH.info_of_tok in_.token in
             skipToken in_;
             let bounds = typeBounds in_ in
             TyAnon (ii, bounds)
         | _ ->
             let start =
               match in_.token with
               | LPAREN _ ->
                   (* CHECK: "Illegal literal type (), use Unit instead" *)
                   let xs = inParens types in_ in
                   (* ast: makeSafeTupleType *)
                   TyTuple xs
               | t when TH.isWildcardType t ->
                   let ii = TH.info_of_tok in_.token in
                   skipToken in_;
                   let bounds = wildcardType in_ in
                   TyWildcard (ii, bounds)
               | _ ->
                   let x = path ~thisOK:false ~typeOK:true in_ in
                   (* ast: convertToTypeId if not SingletonTypeTree *)
                   TyName x
             in
             simpleTypeRest start in_)

and simpleTypeRest t in_ : type_ =
  match in_.token with
  | HASH _ ->
      let x = typeProjection t in_ in
      simpleTypeRest x in_
  | LBRACKET _ ->
      let xs = typeArgs in_ in
      (* ast: AppliedTypeTree(t, xs) *)
      let x = TyApplied (t, xs) in
      simpleTypeRest x in_
  | _ -> t

(** {{{
 *  TypeArgs    ::= `[` ArgType {`,` ArgType} `]`
 *  }}}
*)
and typeArgs in_ : type_ list bracket = inBrackets types in_

(** {{{
 *  Types ::= Type {`,` Type}
 *  }}}
*)
and types in_ = commaSeparated argType in_

and functionTypes in_ = commaSeparated functionArgType in_

(** {{{
 *  CompoundType ::= AnnotType {with AnnotType} [Refinement]
 *                |  Refinement
 *  }}}
*)
and compoundType in_ : type_ =
  (* pad: can't call typ() here, which allows function type 'int => float'
   * because this is used in a pattern context as in case p: ... =>
   * where the arrow mark the start of the caseBlock, not a type.
   *)
  let topt =
    match in_.token with
    | LBRACE _ ->
        (* AST: scalaAnyRefConstr *)
        None
    | _ -> Some (annotType in_)
  in
  compoundTypeRest topt in_

and compoundTypeRest topt in_ =
  let ts = ref [] in
  (* less: could use separatedToken (Kwith ab) annotType instead of while *)
  while in_.token =~= Kwith ab do
    let iwith = TH.info_of_tok in_.token in
    nextToken in_;
    let x = annotType in_ in
    ts += (iwith, x)
  done;
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  let refinements =
    if in_.token =~= LBRACE ab then Some (refinement in_) else None
  in
  (* CHECK: "Detected apparent refinement of Unit" *)
  (* AST: CompoundTypeTree(Template(tps, noSelfType, refinements) *)
  let topt =
    match (topt, List.rev !ts) with
    (* stricter: *)
    | None, _ :: _ -> error "missing type before 'with'" in_
    | x, [] -> x
    | Some t, x :: xs ->
        Some
          (List.fold_left
             (fun acc (iwith, t2) -> TyWith (acc, iwith, t2))
             t (x :: xs))
  in
  match (topt, refinements) with
  | None, None -> raise Impossible
  | Some t, None -> t
  | topt, Some refined -> TyRefined (topt, refined)

(** {{{
 *  AnnotType        ::=  SimpleType {Annotation}
 *  }}}
*)
(* was in PatternContextSensitive trait *)
and annotType in_ =
  (* CHECK: placeholderTypeBoundary *)
  let x = simpleType in_ in
  !annotTypeRest_ x in_

(* ------------------------------------------------------------------------- *)
(* Still in PatternContextSensitive, but more obscure type constructs *)
(* ------------------------------------------------------------------------- *)
and makeExistentialTypeTree t iforsome in_ =
  let xs = refinement in_ in
  (* CHECK: "not a legal existential clause", just TypeDef and ValDef *)
  TyExistential (t, iforsome, xs)

(* pad: https://stackoverflow.com/questions/6676048/why-does-one-select-scala-type-members-with-a-hash-instead-of-a-dot *)
and typeProjection t in_ : type_ =
  let ii = TH.info_of_tok in_.token in
  (* # *)
  skipToken in_;
  let name = identForType in_ in
  (* ast: SelectFromTypeTree(t, name) *)
  TyProj (t, ii, name)

(** {{{
 *  Refinement ::= [nl] `{` RefineStat {semi RefineStat} `}`
 *  }}}
*)
and refinement in_ : refine_stat list bracket = inBraces refineStatSeq in_

(** {{{
 *  RefineStatSeq    ::= RefineStat {semi RefineStat}
 *  RefineStat       ::= Dcl
 *                     | type TypeDef
 *                     |
 *  }}}
*)
and refineStatSeq in_ : refine_stat list =
  (* CHECK: checkNoEscapingPlaceholders *)
  (* less: pad: reuse statSeq? *)
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) do
    let xopt = refineStat in_ in
    stats ++= Option.to_list xopt;
    if not (in_.token =~= RBRACE ab) then acceptStatSep in_
  done;
  List.rev !stats

and refineStat in_ : refine_stat option =
  match in_.token with
  | t when TH.isDclIntro t -> Some (!defOrDcl_ noMods in_)
  | t when not (TH.isStatSep t) -> error "illegal start of declaration" in_
  | _ -> None

(* ------------------------------------------------------------------------- *)
(* Abstract in PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  ArgType       ::=  Type
 *  }}}
*)
and argType in_ =
  warning "argType: STILL? typ() and wildcard or typ()";
  typ in_

(* We want to allow types like (=> Int) => Int, so we parse a paramType, which can also just be a normal type, as before *)
and functionArgType in_ =
  warning "functionArgType STILL? argType or paramType";
  match !paramType_ ~repeatedParameterOK:false in_ with
  | PTByNameApplication (tok, t, _star_opt) ->
      (* RepeatedType probably not allowed here either *)
      TyByName (tok, t)
  | PT t -> t
  | PTRepeatedApplication _ -> error "RepeatedType not allowed here" in_

(* ------------------------------------------------------------------------- *)
(* Outside PatternContextSensitive but mutually recursive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  WildcardType ::= `_` TypeBounds
 *  }}}
*)
and wildcardType in_ =
  (* AST: freshTypeName("_$"), Ident(...) *)
  (* AST: makeSyntheticTypeParam(pname, bounds) *)
  (* CHECK: placeholderTypes = ... *)
  typeBounds in_

(** {{{
 *  TypeBounds ::= [`>:` Type] [`<:` Type]
 *  }}}
*)
and typeBounds in_ : type_bounds =
  (* CHECK: checkNoEscapingPlaceholders *)
  let lo = bound (SUPERTYPE ab) in_ in
  let hi = bound (SUBTYPE ab) in_ in
  (* ast: TypeBoundsTree(lo, hi) *)
  { supertype = lo; subtype = hi }

and bound tok in_ : (tok * type_) option =
  if in_.token =~= tok then (
    let ii = TH.info_of_tok tok in
    nextToken in_;
    Some (ii, typ in_))
  else None

(* ------------------------------------------------------------------------- *)
(* Outside PatternContextSensitive *)
(* ------------------------------------------------------------------------- *)

(* These are default entry points into the pattern context sensitive methods:
 *  they are all initiated from non-pattern context.
 *)
let typ x : type_ = outPattern typ x
let startInfixType x : type_ = outPattern (infixType FirstOp) x
let startAnnotType in_ : type_ = outPattern annotType in_
let exprSimpleType x : type_ = outPattern simpleType x

let typeOrInfixType location in_ : type_ =
  in_
  |> with_logging "typeOrInfixType" (fun () ->
         if location =*= Local then typ in_ else startInfixType in_)

(** {{{
 *  TypedOpt ::= [`:` Type]
 *  }}}
*)
let typedOpt in_ =
  match in_.token with
  | COLON _ ->
      nextToken in_;
      let t = typ in_ in
      Some t
  | _ -> None
(* ast: TypeTree *)

(*****************************************************************************)
(* Parsing patterns  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Inside SeqContextSensitive *)
(* ------------------------------------------------------------------------- *)
(* STILL?: functionArgType, argType different here *)

(** {{{
 *  Pattern  ::=  Pattern1 { `|` Pattern1 }
 *  SeqPattern ::= SeqPattern1 { `|` SeqPattern1 }
 *  }}}
*)
let rec pattern in_ : pattern =
  in_
  |> with_logging "pattern" (fun () ->
         let rec loop () =
           let p1 = pattern1 in_ in
           if TH.isRawBar in_.token then (
             let ii = TH.info_of_tok in_.token in
             nextToken in_;
             let p2 = loop () in
             PatDisj (p1, ii, p2))
           else p1
         in
         let res = loop () in
         (* ast: makeAlternative if many elements *)
         res)

(** {{{
 *  Pattern1    ::= boundvarid `:` TypePat
 *                |  `_` `:` TypePat
 *                |  Pattern2
 *  SeqPattern1 ::= boundvarid `:` TypePat
 *                |  `_` `:` TypePat
 *                |  [SeqPattern2]
 *  }}}
*)
and pattern1 in_ : pattern =
  let p = pattern2 in_ in
  (* crazy? depending whether Ident(name) && if nme.isVariableName(name)? *)
  match in_.token with
  | COLON ii -> (
      (* AST: p.removeAttachment[BackquotedIdentifierAttachment.type] *)
      skipToken in_;
      let t = compoundType in_ in
      (* ast: Typed(p, x) *)
      match p with
      | PatVarid varid -> PatTypedVarid (varid, ii, t)
      (* stricter? *)
      | _ -> error "typed patterns work only with a varid" in_)
  (* CHECK: "Pattern variables must start with a lower-case letter." *)
  | _ -> p

(** {{{
 *  Pattern2    ::=  id  `@` Pattern3
 *                |  `_` `@` Pattern3
 *                |   Pattern3
 *  }}}
*)
and pattern2 in_ : pattern =
  let x = pattern3 in_ in
  match in_.token with
  | AT ii -> (
      nextToken in_;
      let y = pattern3 in_ in
      (* not_crazy: ast: case p @ Ident(name) *)
      match x with
      (* ast: Bind(name, body), if WILDCARD then ... *)
      | PatVarid varid -> PatBind (varid, ii, y)
      (* stricter? *)
      | _ -> error "bound patterns work only with a varid" in_)
  | _ -> x

(** {{{
 *  Pattern3    ::= SimplePattern
 *                |  SimplePattern {Id [nl] SimplePattern}
 *  }}}
*)
(* pad: similar to infixExpr/infixType, rename infixPattern? *)
and pattern3 in_ : pattern =
  in_
  |> with_logging "pattern3" (fun () ->
         (* CHECK: badPattern3 *)
         let top = simplePattern in_ in
         (* AST: let base = opstack *)
         let checkWildStar in_ =
           warning "checkWildStar, incomplete";
           (* crazy: if top = USCORE && sequenceOK && peekingAhead ... *)
           match in_.token with
           | STAR ii ->
               nextToken in_;
               Some ii
           | _ -> None
         in
         let rec loop top in_ =
           in_
           |> with_logging "pattern3: loop" (fun () ->
                  (* AST: let next = reducePatternStack(base, top) *)
                  let next = () in
                  if TH.isIdentBool in_.token && not (TH.isRawBar in_.token)
                  then
                    let id, _targsTODO = pushOpInfo next in_ in
                    (* no postfixPattern, so always go for right part of infix op *)
                    let t2 = simplePattern in_ in
                    let x = PatInfix (top, id, t2) in
                    loop x in_
                  else
                    in_
                    |> with_logging "pattern3: loop noIsIdent stop" (fun () ->
                           top))
         in
         match checkWildStar in_ with
         | None ->
             let x = loop top in_ in
             stripParens x
         | Some istar -> (
             (* ast: x *)
             match top with
             | PatVarid ("_", iuscore) -> PatUnderscoreStar (iuscore, istar)
             (* stricter: *)
             | _ -> error "star patterns works only with wildcard" in_))

(** {{{
 *  Patterns ::= Pattern { `,` Pattern }
 *  SeqPatterns ::= SeqPattern { `,` SeqPattern }
 *  }}}
*)
and patterns in_ = commaSeparated pattern in_

(* ------------------------------------------------------------------------- *)
(* Outside SeqContextSensitive *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  SimplePattern    ::= varid
 *                    |  `_`
 *                    |  literal
 *                    |  XmlPattern
 *                    |  StableId  /[TypeArgs]/ [`(` [Patterns] `)`]
 *                    |  StableId  [`(` [Patterns] `)`]
 *                    |  StableId  [`(` [Patterns] `,` [varid `@`] `_` `*` `)`]
 *                    |  `(` [Patterns] `)`
 *                    |  Quoted
 *  }}}
 *
 * XXX: Hook for IDE
*)
and simplePattern in_ : pattern =
  in_
  |> with_logging "simplePattern" (fun () ->
         match in_.token with
         (* pad: the code was written in a very different way by doing that
          * below, given isIdentBool will say yes for MINUS
          *)
         (* matthew: [val - = expr] and [case - => expr] and [- <- expr] are valid Scala, so we check that - is not the end of the pattern *)
         | MINUS ii
           when lookingAhead
                  (fun in_ ->
                    match in_.token with
                    | EQUALS _
                    | ARROW _
                    | LARROW _ ->
                        false
                    | _ -> true)
                  in_ ->
             nextToken in_;
             let x = literal ~isNegated:ii ~inPattern:true in_ in
             PatLiteral x
         | x when TH.isIdentBool x || x =~= Kthis ab -> (
             let t = stableId in_ in
             (* less: if t = Ident("-") literal isNegated:true inPattern:true *)
             let typeAppliedTree =
               match in_.token with
               | LBRACKET _ ->
                   let xs = typeArgs in_ in
                   (* ast: AppliedTypeTree(convertToTypeId(t), xs) *)
                   Some xs
               | _ -> None (* ast: t *)
             in
             let args =
               match in_.token with
               | LPAREN _ ->
                   (* ast: Apply(typeAppliedTree, t) *)
                   Some (argumentPatterns in_)
               | _ ->
                   (* ast: typeAppliedTree *)
                   None
             in
             match (t, typeAppliedTree, args) with
             | (Id (s, ii), []), None, None when AST.is_variable_name s ->
                 PatVarid (s, ii)
             | t, None, None -> PatName t
             | t, x1, x2 -> PatApply (t, x1, x2))
         | USCORE ii ->
             nextToken in_;
             (* ast: Ident(nme.WILDCARD) *)
             PatVarid (AST.wildcard, ii)
         | LPAREN _ ->
             let xs = makeParens (noSeq patterns) in_ in
             PatTuple xs
         | x when TH.isLiteral x ->
             let x = literal ~inPattern:true in_ in
             PatLiteral x
         | QUOTE quote ->
             let x = quoted quote in_ in
             PatQuoted x
         | Ellipsis t ->
             nextToken in_;
             PatEllipsis t
         (* scala3: deprecated XMLSTART *)
         | _ -> error "illegal start of simple pattern" in_)

and argumentPatterns in_ : pattern list bracket =
  in_
  |> with_logging "argumentPatterns" (fun () ->
         inParens
           (fun in_ -> if in_.token =~= RPAREN ab then [] else seqPatterns in_)
           in_)

(** {{{
 *  Quoted     ::=  ‘'’ ‘{’ Block ‘}’
 *               |  ‘'’ ‘[’ Type ‘]’
 *  }}}
*)
and quoted quote_tok in_ =
  in_
  |> with_logging (spf "quoted") (fun () ->
         nextToken in_;
         match in_.token with
         | LBRACE _ ->
             let x = inBraces !blockStatSeq_ in_ in
             QuotedBlock (quote_tok, x)
         | LBRACKET _ ->
             let x = inBrackets typ in_ in
             QuotedType (quote_tok, x)
         | _ -> error "error in quoted expr: brace or bracket expected" in_)

(* ------------------------------------------------------------------------- *)
(* Context sensitive choices *)
(* ------------------------------------------------------------------------- *)

and seqPatterns in_ = seqOK patterns in_

let pattern in_ : pattern = noSeq pattern in_

(*****************************************************************************)
(* Parsing expressions  *)
(*****************************************************************************)

(** {{{
 *  Expr       ::= (Bindings | [`implicit`] Id | `_`)  `=>` Expr
 *               | Expr1
 *  ResultExpr ::= (Bindings | Id `:` CompoundType) `=>` Block
 *               | Expr1
 *  Expr1      ::= if `(` Expr `)` {nl} Expr [[semi] else Expr]
 *               | ‘if’  Expr ‘then’ Expr [[semi] ‘else’ Expr]
 *               | try (`{` Block `}` | Expr) [catch (`{` CaseClauses `}` | ExprCaseClause)] [finally Expr]
 *               | while `(` Expr `)` {nl} Expr
 *               | `while` Expr `do` Expr
 *               | do Expr [semi] while `(` Expr `)`
 *               | for (`(` Enumerators `)` | `{` Enumerators `}` | Enumerators) {nl} [yield] Expr
 *               | throw Expr
 *               | return [Expr]
 *               | [SimpleExpr `.`] Id `=` Expr
 *               | SimpleExpr1 ArgumentExprs `=` Expr
 *               | PostfixExpr Ascription
 *               | PostfixExpr match <<< CaseClauses >>>
 *  Bindings   ::= `(` [Binding {`,` Binding}] `)`
 *  Binding    ::= (Id | `_`) [`:` Type]
 *  Ascription ::= `:` CompoundType
 *               | `:` Annotation {Annotation}
 *               | `:` `_` `*`
 *  }}}
*)

(* NOTE: indented-block-expr
   Moreover, if this is the start to a block, then every case in the `expr`
   recursive descent must fall through to the simpleExpr blockExpr case,
   otherwise we will end up assuming we're in a different case when we
   already determined we're not.
*)
let rec expr ?(location = Local) (in_ : env) : expr =
  let is_block_expr = isIndentedBlockExprStart in_ in
  expr0 ~is_block_expr location in_

and expr0 ?(is_block_expr = false) (location : location) (in_ : env) : expr =
  in_
  |> with_logging
       (spf "expr0(location = %s)" (show_location location))
       (fun () ->
         match in_.token with
         | _ when is_block_expr ->
             (* see NOTE: indented-block-expr *)
             parseOther ~is_block_expr location in_
         | Kif _ -> S (parseIf in_)
         | Ktry _ -> S (parseTry in_)
         | Kwhile _ -> S (parseWhile in_)
         | Kdo _ -> S (parseDo in_)
         | Kfor _ -> S (parseFor in_)
         | Kreturn _ -> S (parseReturn in_)
         | Kthrow _ -> S (parseThrow in_)
         | Kimplicit ii ->
             skipToken in_;
             implicitClosure (Implicit, ii) location in_
         | _ -> parseOther location in_)

and parseOther ?(is_block_expr = false) location (in_ : env) : expr =
  in_
  |> with_logging
       (spf "parseOther(location = %s)" (show_location location))
       (fun () ->
         let t = ref (postfixExpr ~is_block_expr in_) in
         (match in_.token with
         | EQUALS ii ->
             skipToken in_;
             (* crazy? parsing that depends on built AST!!
              * AST: if Ident | Select | Apply *)
             let e = expr in_ in
             (* ast: mkAssign(t, e) *)
             t := Assign (!t, ii, e)
         (* pad: we may actually be inside a binding, and not an expression here,
          * for example in 'foo((x: Path) => (...))' we can have parsed
          * x and seeing the colon.
          *)
         | COLON icolon -> (
             t := stripParens !t;
             skipToken in_;
             match in_.token with
             | USCORE _ -> (
                 skipToken in_;
                 match in_.token with
                 | STAR _ (* STILL?: was isIdent && name = "*" *) ->
                     nextToken in_
                 (* AST: t = Typed(t, Ident(WILDCARD_STAR)) *)
                 | _ -> error "* expected" in_)
             | x when TH.isAnnotation x ->
                 let _tsTODO = annotations ~skipNewLines:false in_ in
                 (* AST: fold around t makeAnnotated *)
                 ()
             | _ ->
                 let tpt = typeOrInfixType location in_ in
                 (* AST: if isWildcard(t) ... *)
                 (* ast: Typed(t, tpt) *)
                 t := TypedExpr (!t, icolon, tpt))
         | Kmatch ii ->
             skipToken in_;
             let xs = inBracesOrIndented caseClauses in_ in
             (* ast: t:= Match(stripParens(t)) *)
             t := Match (stripParens !t, ii, xs)
         | _ -> ());
         (* AST: disambiguate between self types "x: Int =>" and orphan function
          * literals "(x: Int) => ???"
          * "(this: Int) =>" is parsed as an erroneous function literal but emits
          * special guidance on what's probably intended.
          *)
         (* crazy: another parsing depending on the AST! crazy *)
         let lhsIsTypedParamList _xTODO =
           (* CHECK: "self-type annotation may not be in parentheses" *)
           warning "lhsIsTypedParamList";
           false
         in
         (match in_.token with
         | ARROW ii when location <> InTemplate || lhsIsTypedParamList !t ->
             skipToken in_;
             let fbody =
               if location <> InBlock then FExpr (ii, expr in_)
               else FBlock (fb ii (BEBlock (block in_)))
             in
             let params = convertToParams ii !t in
             let def =
               {
                 fkind = (LambdaArrow, ii);
                 fparams = [ params ];
                 frettype = None;
                 fbody = Some fbody;
               }
             in
             t := Lambda def
         | _ -> ());
         stripParens !t)

(** {{{
 *  PostfixExpr   ::= InfixExpr [Id [nl]]
 *  InfixExpr     ::= PrefixExpr
 *                  | InfixExpr Id [nl] InfixExpr
 *  }}}
*)
and postfixExpr ?(is_block_expr = false) in_ : expr =
  in_
  |> with_logging "postfixExpr" (fun () ->
         (* TODO: AST: opstack, reduce *)
         let rec loop top in_ =
           in_
           |> with_logging "postfixExpr:loop" (fun () ->
                  (* In Scala 3, we can have if statements that look like
                     if <expr> then <expr> else <expr>

                     Except, in Scala 3, `then` is also a soft keyword.

                     This means that without this check, we would parse
                       <expr> then
                     as a postfix application of `then` to <expr>.
                  *)
                  if
                    (not (TH.isIdentBool in_.token))
                    || in_.token =~= ID_LOWER ("then", ab)
                  then
                    in_
                    |> with_logging "postfixExpr:loop: noIdentBool, stop"
                         (fun () -> top)
                  else
                    (* isIdentBool is true; remember that operators are identifiers and
                     * that Scala allows any identifier in infix position
                     *)
                    (* AST: pushOpInfo(reduceExprStack(base, top)) *)
                    let id, _targsTODO = pushOpInfo top in_ in
                    newLineOptWhenFollowing TH.isExprIntro in_;
                    if TH.isExprIntro in_.token then
                      let res = prefixExpr in_ in
                      match res with
                      (*| None -> (* AST: reduceExprStack(base, top) *) None
                        | Some *)
                      | next ->
                          let next = Infix (top, id, next) in
                          loop next in_
                    else
                      in_
                      |> with_logging "postfixExpr:loop: noExprIntro, stop"
                           (fun () ->
                             (* AST: finishPostfixOp(base, popOpInfo()) *)
                             Postfix (top, id)))
         in
         let res = prefixExpr ~is_block_expr in_ in
         (* AST: reduceExprStack (res) *)
         loop res in_)

(** {{{
 *  PrefixExpr   ::= [`-` | `+` | `~` | `!`] SimpleExpr
 *  }}}
*)
and prefixExpr ?(is_block_expr = false) in_ : expr =
  match in_.token with
  | _ when is_block_expr ->
      (* see NOTE: indented-block-expr *)
      simpleExpr ~is_block_expr in_
  | t when TH.isUnaryOp t ->
      if lookingAhead (fun in_ -> TH.isExprIntro in_.token) in_ then
        let uname = rawIdent in_ in
        (* AST: toUnaryName ... *)
        match (t, in_.token) with
        | MINUS ii, x when TH.isNumericLit x (* uname == nme.UNARY_- ... *) ->
            (* start at the -, not the number *)
            let x = literal ~isNegated:ii in_ in
            let x' = L x in
            simpleExprRest ~canApply:true x' in_
        | _ ->
            let x = simpleExpr in_ in
            (* ast: Select(stripParens(x), uname) *)
            Prefix (uname, stripParens x)
      else simpleExpr in_
  | _ -> simpleExpr in_

(** {{{
 *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
 *                  |  BlockExpr
 *                  |  SimpleExpr1 [`_`]
 *  SimpleExpr1   ::= literal
 *                  |  xLiteral
 *                  |  Path
 *                  |  Quoted
 *                  |  `(` [Exprs] `)`
 *                  |  SimpleExpr `.` Id
 *                  |  SimpleExpr TypeArgs
 *                  |  SimpleExpr1 ArgumentExprs
 *  }}}
*)
and simpleExpr ?(is_block_expr = false) in_ : expr =
  in_
  |> with_logging "simpleExpr" (fun () ->
         let canApply = ref true in
         let t =
           match in_.token with
           | _ when is_block_expr ->
               canApply := false;
               BlockExpr (blockExpr in_)
           | x when TH.isLiteral x ->
               let x = literal in_ in
               L x
           | x
             when TH.isIdentBool x
                  && lookingAhead2
                       (fun in_ ->
                         match in_.token with
                         | DOT _ -> true
                         | _ -> false)
                       (fun in_ ->
                         match in_.token with
                         | Ellipsis _ -> true
                         | _ -> false)
                       in_ ->
               let name = ident in_ in
               Name (Id name, [])
           (* scala3: deprecated XMLSTART *)
           | x when TH.isIdentBool x ->
               let x = path ~thisOK:true ~typeOK:false in_ in
               Name x
           | Kthis _
           | Ksuper _ ->
               let x = path ~thisOK:true ~typeOK:false in_ in
               Name x
           | USCORE ii ->
               nextToken in_;
               (* ast: freshPlaceholder *)
               ExprUnderscore ii
           (* pad: this may actually be a binding, not a tuple of
            * expressions when part of a short lambda (arrow).
            *)
           | LPAREN _ -> (
               let ((_, xs, _) as tuple) =
                 makeParens (commaSeparated expr) in_
               in
               match xs with
               | [ x ] -> x
               | _ -> Tuple tuple)
           | LBRACE _ ->
               canApply := false;
               let x = blockExpr in_ in
               BlockExpr x
           | QUOTE quote ->
               let x = quoted quote in_ in
               Quoted x
           | Knew ii ->
               canApply := false;
               skipToken in_;
               let cparents, cbody = !template_ in_ in
               (* ast: gen.mkNew(parents, self, stats) *)
               let def =
                 { ckind = (Singleton, ii); cparams = []; cparents; cbody }
               in
               New (ii, def)
           (* semgrep-ext: *)
           | T.Ellipsis ii ->
               skipToken in_;
               AST.Ellipsis ii
           | LDots ld ->
               skipToken in_;
               let e = expr in_ in
               let rd = TH.info_of_tok in_.token in
               accept (RDots ab) in_;
               DeepEllipsis (ld, e, rd)
           | _ -> error "illegal start of simple expression" in_
         in
         simpleExprRest ~canApply:!canApply t in_)

and simpleExprRest ~canApply t in_ : expr =
  in_
  |> with_logging (spf "simpleExprRest(canApply:%b)" canApply) (fun () ->
         (* crazy: again parsing depending on AST *)
         if canApply then newLineOptWhenFollowedBy (LBRACE ab) in_;

         let paren_or_brace () =
           let xs = argumentExprs in_ in
           let app =
             (* AST: look for anonymous function application like (f _)(x) and
              *      translate to (f _).apply(x), bug #460
              * AST: let sel = ... Select(stripParens(t, nme.apply) in
              *      Apply(sel, x)
              *)
             Apply (t, [ xs ])
           in
           simpleExprRest ~canApply:true app in_
         in
         match in_.token with
         | DOT ii -> (
             nextToken in_;
             match in_.token with
             | Ellipsis ii ->
                 nextToken in_;
                 let x = stripParens (DotAccessEllipsis (t, ii)) in
                 simpleExprRest ~canApply:true x in_
             | _ ->
                 let id = selector (*t*) in_ in
                 let x = stripParens (DotAccess (t, ii, id)) in
                 simpleExprRest ~canApply:true x in_)
         | LBRACKET _ ->
             let t1 = stripParens t in
             (* crazy: parsing depending on built AST: Ident(_) | Select(_, _) | Apply(_, _) | Literal(_) *)
             let app = ref t1 in
             while in_.token =~= LBRACKET ab do
               let xs = exprTypeArgs in_ in
               (* ast: app := TypeApply(app, xs) *)
               app := InstanciatedExpr (!app, xs)
             done;
             simpleExprRest ~canApply:true !app in_
         | LPAREN _ -> paren_or_brace ()
         | LBRACE _ when canApply -> paren_or_brace ()
         | USCORE _ ->
             skipToken in_;
             (* AST: MethodValue(stripParens(t)) *)
             t
         | _ ->
             in_
             |> with_logging "simpleExprRest: nothing to consume" (fun () -> t))

(* and literal in_ = ...
 *  is now at the top because it's also used by SimplePattern
 *)

and exprTypeArgs in_ = outPattern typeArgs in_

(* ------------------------------------------------------------------------- *)
(* Interpolated strings *)
(* ------------------------------------------------------------------------- *)

and interpolatedString ~inPattern in_ =
  ignore inPattern;
  (* useful? *)
  (* ast: let interpolater = in.name.encoded,  *)
  (* ast: let partsBuf = ref [] in let exprsBuf = ref [] in *)
  let xs = ref [] in
  let ii = TH.info_of_tok in_.token in
  nextToken in_;
  (* While parsing interpolated string components, we don't want
     to emit NEWLINEs or DEDENTs.

     This helps us in cases like:

     object Foo:
       val x = s"""
     hi there"""
  *)
  inSepRegion (T_INTERPOLATED_END ii)
    (fun () ->
      (* T_INTERPOLATED_START(s,info) *)
      while TH.is_stringpart in_.token do
        (* pad: in original code, but the interpolated string can start with
         * a non string literal like $f, so I've commented this code.
         * let x = literal in_ in
         * if inPattern
         * then todo "interpolatedString: inPattern (dropAnyBraces(pattern))" in_
         * else
         *)
        match in_.token with
        (* pad: the original code  uses IDENTIFIER but Lexer_scala.mll
         * introduces a new token for $xxx.
         * STILL? the original code also allow 'this', but ID_DOLLAR should cover
         * that?
         *)
        | ID_DOLLAR _ ->
            let x = ident in_ in
            (* ast: Ident(x) *)
            xs += EncapsDollarIdent x
        (* actually a ${, but using LBRACE allows to reuse blockExpr *)
        | LBRACE _ ->
            let x = expr in_ in
            (* ast: x *)
            xs += EncapsExpr x
        (* pad: not in original code, but the way Lexer_scala.mll is written
         * we can have multiple consecutive StringLiteral *)
        | StringLiteral x ->
            nextToken in_;
            xs += EncapsStr x
        | _ ->
            error "error in interpolated string: identifier or block expected"
              in_
      done)
    in_;
  (* pad: not in original code *)
  let ii = TH.info_of_tok in_.token in
  accept (T_INTERPOLATED_END ab) in_;
  (* ast: InterpolatedString(...) *)
  (List.rev !xs, ii)

(* ------------------------------------------------------------------------- *)
(* Arguments *)
(* ------------------------------------------------------------------------- *)
(* A succession of argument lists. *)
and multipleArgumentExprs in_ : arguments list =
  in_
  |> with_logging "multipleArgumentExprs" (fun () ->
         match in_.token with
         | LPAREN _ ->
             let x = argumentExprs in_ in
             let xs = multipleArgumentExprs in_ in
             x :: xs
         | _ -> [])

(** {{{
 *  ArgumentExprs     ::= ParArgumentExprs
 *                      | [nl] BlockExpr
 *  ParArgumentExprs  ::=  ‘(’ [Exprs] ‘)’
 *                      |  ‘(’ ‘using’ Exprs ‘)’
 *                      |  ‘(’ [Exprs ‘,’] PostfixExpr ‘*’ ‘)’ (* TODO *)
 *  }}}
*)

(* We will solve the splatted last argument case in scala_to_generic.
 *)
and parArgumentExprs in_ : arguments =
  (* less: could use makeParens *)
  let lb, (exprs, is_using), rb =
    inParens
      (fun in_ ->
        match in_.token with
        | RPAREN _ -> ([], false)
        | ID_LOWER ("using", _) ->
            accept (ID_LOWER ("using", ab)) in_;
            (commaSeparated expr in_, true)
        | _ ->
            (* AST: if isIdent then assignmentToMaybeNamedArg *)
            (commaSeparated expr in_, false))
      in_
  in
  if is_using then ArgUsing (lb, exprs, rb) else Args (lb, exprs, rb)

and argumentExprs in_ : arguments =
  in_
  |> with_logging "argumentExprs" (fun () ->
         match in_.token with
         | LBRACE _ -> ArgBlock (blockExpr in_)
         | LPAREN _ -> parArgumentExprs in_
         (* TODO: is using this token a good idea? Would unsafe_fake_bracket be better or worse? *)
         | _ -> Args (fb (TH.info_of_tok in_.token) []))

(* ------------------------------------------------------------------------- *)
(* Case clauses *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Guard ::= if PostfixExpr
 *  }}}
*)
and guard in_ : guard option =
  in_
  |> with_logging "guard" (fun () ->
         match in_.token with
         | Kif ii ->
             nextToken in_;
             let x = postfixExpr in_ in
             Some (ii, stripParens x)
         | _ -> None)

and caseRhs rhs_fn in_ : tok * block =
  let ii = TH.info_of_tok in_.token in
  accept (ARROW ab) in_;
  let x = rhs_fn in_ in
  (ii, x)

and caseClause rhs_fn icase in_ : (pattern, block) case_clause =
  let p, g =
    inSepRegion (ARROW icase)
      (fun () ->
        let p = pattern in_ in
        let g = guard in_ in
        (p, g))
      in_
  in
  let iarrow, rhs = caseRhs rhs_fn in_ in
  (* ast: makeCaseDef *)
  CC
    {
      casetoks = (icase, iarrow);
      case_left = p;
      caseguard = g;
      case_right = rhs;
    }

(** {{{
 *  ExprCaseClause  ::= case Pattern [Guard] `=>` Expr
 *  }}}
*)
and exprCaseClause icase in_ =
  caseClause
    (fun in_ ->
      let expr = expr in_ in
      [ E expr ])
    icase in_

(** {{{
 *  CaseClauses ::= CaseClause {CaseClause}
 *  CaseClause  ::= case Pattern [Guard] `=>` Block
 *  }}}
*)
and caseClauses in_ : case_clauses =
  (* CHECK: empty cases *)
  (* old: was
   *   caseSeparated caseClause in_
   * with
   *   let caseSeparated part in_ =
   *     separatedToken (Kcase ab) part in_
   * but for semgrep we also need to handle ellipsis
   *)
  (* similar to separatedToken body *)
  let ts = ref [] in
  while in_.token =~= Kcase ab || in_.token =~= Ellipsis ab do
    match in_.token with
    | Kcase ii ->
        nextToken in_;
        let x = caseClause block ii in_ in
        ts += x
    | Ellipsis ii ->
        nextToken in_;
        ts += CaseEllipsis ii
    | _ -> raise Impossible
  done;
  List.rev !ts

(** {{{
 *  TypeCaseClauses ::= TypeCaseClause {TypeCaseClause}
 *  TypeCaseClause  ::= `case` (InfixType | `_`) `=>` Type [semi]
 *  }}}
*)
and typeCaseClause icase in_ : ((tok, type_) Either.t, type_) case_clause =
  let l_ty =
    inSepRegion (ARROW icase)
      (fun () ->
        match in_.token with
        | USCORE ii ->
            skipToken in_;
            Left ii
        | _ -> Right (startInfixType in_))
      in_
  in
  let iarrow = TH.info_of_tok in_.token in
  accept (ARROW ab) in_;
  let r_ty = typ in_ in
  if TH.isStatSep in_.token then nextToken in_;
  (* ast: makeCaseDef *)
  CC
    {
      casetoks = (icase, iarrow);
      case_left = l_ty;
      caseguard = None;
      case_right = r_ty;
    }

and typeCaseClauses in_ : type_case_clauses =
  (* CHECK: empty cases *)
  (* old: was
   *   caseSeparated caseClause in_s
   * with
   *   let caseSeparated part in_ =
   *     separatedToken (Kcase ab) part in_
   * but for semgrep we also need to handle ellipsis
   *)
  (* similar to separatedToken body *)
  let ts = ref [] in
  while in_.token =~= Kcase ab || in_.token =~= Ellipsis ab do
    match in_.token with
    | Kcase ii ->
        nextToken in_;
        let x = typeCaseClause ii in_ in
        ts += x
    | Ellipsis ii ->
        nextToken in_;
        ts += CaseEllipsis ii
    | _ -> raise Impossible
  done;
  List.rev !ts

(* ------------------------------------------------------------------------- *)
(* Misc *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Expr ::= implicit Id `=>` Expr
 *  }}}
*)
and implicitClosure implicitmod location in_ =
  let id = ident in_ in
  (* ast: expr0 = ... *)
  let topt =
    if in_.token =~= COLON ab then (
      nextToken in_;
      (* ast: Typed(Ident(id, t)) *)
      Some (PT (typeOrInfixType location in_)))
    else None (* ast: Ident(id) *)
  in
  (* ast: convertToParam expr0; copyValDef(param0, mods|Flags.IMPLICIT)  *)
  let param =
    ParamClassic
      {
        p_name = id;
        p_attrs = [ M implicitmod ];
        p_type = topt;
        p_default = None;
      }
  in
  let iarrow = TH.info_of_tok in_.token in
  accept (ARROW ab) in_;
  let body =
    if location <> InBlock then FExpr (iarrow, expr in_)
    else
      let xs = block in_ in
      FBlock (fb iarrow (BEBlock xs))
  in

  (* ast: Function(List(param), x) *)
  let def =
    {
      fkind = (LambdaArrow, iarrow);
      fparams = [ fb iarrow ([ param ], None) ];
      frettype = None;
      fbody = Some body;
    }
  in
  Lambda def

(*****************************************************************************)
(* Parsing statements (which are expressions in Scala) *)
(*****************************************************************************)
and parseIf in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let cond =
    match in_.token with
    | LPAREN _ ->
        let cond = parensCondExpr in_ in
        newLinesOpt in_;
        cond
    | _ ->
        let e = expr in_ in
        (* So we can parse things like
           if true
           then 2
           else 3
        *)
        newLinesOpt in_;
        accept (ID_LOWER ("then", ab)) in_;
        newLinesOpt in_;
        fb (Tok.unsafe_fake_tok "") e
  in
  opportunisticIndent in_;
  let thenp = !indentedExprOrBlockStatSeqUntil_ ~until:(Some (Kelse ab)) in_ in
  let elsep =
    match in_.token with
    | Kelse ii ->
        nextToken in_;
        let e = expr in_ in
        Some (ii, e)
    | _ ->
        (* ast: literalUnit *)
        None
  in
  (* ast: If(cond, thenp, elsep) *)
  If (ii, cond, thenp, elsep)

and parensCondExpr in_ : expr bracket =
  let lp = TH.info_of_tok in_.token in
  accept (LPAREN ab) in_;
  let r = expr in_ in
  let rp = TH.info_of_tok in_.token in
  accept (RPAREN ab) in_;
  (* AST: if isWildcard(r) *)
  (lp, r, rp)

and parseWhile in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let cond =
    match in_.token with
    | LPAREN _ ->
        let cond = parensCondExpr in_ in
        newLinesOpt in_;
        cond
    | _ ->
        let e = expr in_ in
        newLinesOpt in_;
        accept (Kdo ab) in_;
        fb (Tok.unsafe_fake_tok "") e
  in
  opportunisticIndent in_;
  let body = expr in_ in
  (* ast: makeWhile(cond, body) *)
  While (ii, cond, body)

and parseDo in_ : stmt =
  let ido = TH.info_of_tok in_.token in
  skipToken in_;
  (* AST: lname = freshTermName(nme.DO_WHILE_PREFIX) *)
  let body = expr in_ in
  if TH.isStatSep in_.token then nextToken in_;
  let iwhile = TH.info_of_tok in_.token in
  accept (Kwhile ab) in_;
  let cond = parensCondExpr in_ in
  (* ast: makeDoWhile(lname.toTermName, body, cond) *)
  DoWhile (ido, body, iwhile, cond)

(* Is the following sequence the generators of a for-expression enclosed in (...)? *)
(* As implemented in Dotty:
   https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L857
*)
and followingIsEnclosedGenerators in_ =
  let parens = ref 1 in
  lookingAhead
    (fun in_ ->
      while !parens <> 0 && not (in_.token =~= EOF ab) do
        if in_.token =~= LPAREN ab then incr parens
        else if in_.token =~= RPAREN ab then decr parens;
        nextToken in_
      done;
      if in_.token =~= LARROW ab then false
        (* it's a pattern *)
        (* nosemgrep *)
      else if TH.isIdentBool in_.token then true
        (* it's not a pattern since token cannot be an infix operator *)
      else
        (* followedByToken(LARROW) // `<-` comes before possible statement starts
           In the Scala compiler, this is what it says ^

           Except this function is actually like, quite complicated, and relies on
           other functions we haven't yet ported.

           So let's just not do that for now.
        *)
        true)
    in_

and parseFor in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let enums =
    (* note: sepFor
       This is so that we can emit newlines specifically within a `for`.
       Don't blame me, blame Dotty:
       https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Scanners.scala#L505
    *)
    inSepRegion (Kfor ii)
      (fun () ->
        match in_.token with
        | LPAREN _ when followingIsEnclosedGenerators in_ ->
            inParens enumerators in_
        | LBRACE _ -> inBraces enumerators in_
        | _ ->
            (* Deliberately not `inBracesOrIndented`, to combine the last two cases!
               If we enter the indentation region, the end of a for expression like:

               for
                 _ <- 5
               yield

               will cause a DEDENT to be emitted before the `yield`. This messes us
               up when determining when to end, because if we see the future DEDENT and break,
               then we will still be on the NEWLINE.

               It's easiest to just not emit the DEDENT tokens at all, which is what happens
               if we do not enter the indentation region.

               See
               https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L2730
            *)
            fb (Tok.unsafe_fake_tok "") (enumerators in_))
      in_
  in
  newLinesOpt in_;
  opportunisticIndent in_;
  let body =
    match in_.token with
    | Kyield ii ->
        nextToken in_;
        let e = expr in_ in
        (* ast: gen.mkFor(enums, gen.Yield(expr())) *)
        Yield (ii, e)
    | _ ->
        let e = expr in_ in
        (* ast: gen.mkFor(enums, expr()) *)
        NoYield e
  in
  For (ii, enums, body)

and parseReturn in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let x =
    if TH.isExprIntro in_.token then Some (expr in_)
    else None (* ast: literalUnit *)
  in
  (* ast: Return(x) *)
  Return (ii, x)

(* use this to determine if we are in
   catch { ... }
         ^
   where we are currently after the `catch`
*)
and isEllipsisCase in_ =
  in_.token =~= LBRACE ab
  && lookingAhead
       (fun in_ ->
         in_.token =~= Ellipsis ab
         && lookingAhead (fun in_ -> in_.token =~= RBRACE ab) in_)
       in_

(** {{{
 *  ExprCaseClause ::= ‘case’ Pattern [Guard] ‘=>’ Expr
 *  }}}
*)

and parseTry in_ : stmt =
  let itry = TH.info_of_tok in_.token in
  skipToken in_;
  let body = expr in_ in
  let handler =
    match in_.token with
    | Kcatch ii -> (
        nextToken in_;
        match in_.token with
        | LBRACE _ when isEllipsisCase in_ ->
            let l = TH.info_of_tok in_.token in
            accept (LBRACE ab) in_;
            let ii = TH.info_of_tok in_.token in
            accept (Ellipsis ab) in_;
            let r = TH.info_of_tok in_.token in
            accept (RBRACE ab) in_;
            Flag_parsing.sgrep_guard
              (Some (ii, CatchCases (l, [ CaseEllipsis ii ], r)))
        | Kcase ab ->
            skipToken in_;
            let cc = exprCaseClause ab in_ in
            (* Technically, the body of this case should be an `expr`, and
               not a `block`.

               But that's how all case clauses behave, they can have a singular
               expression at the end, since `expr` is subsumed under `block`.

               So we will just treat them uniformly.
            *)
            Some (ii, CatchCases (ab, [ cc ], ab))
        | _ ->
            let e = expr in_ in
            let cases = makeMatchFromExpr e in
            Some (ii, cases))
    | _ -> None
  in
  let finalizer =
    match in_.token with
    | Kfinally ii ->
        nextToken in_;
        Some (ii, expr in_)
    | _ -> None
  in
  (* ast: Try(body, handler, finalizer) *)
  Try (itry, body, handler, finalizer)

and parseThrow in_ : stmt =
  let ii = TH.info_of_tok in_.token in
  skipToken in_;
  let e = expr in_ in
  (* ast: Throw(e) *)
  Throw (ii, e)

and statement (location : location) (in_ : env) : expr = expr ~location in_

(** {{{
 *  Block ::= BlockStatSeq
 *  }}}
 *  @note  Return tree does not carry position.
*)

and block in_ : block =
  in_
  |> with_logging "block" (fun () -> (* ast: makeBlock(xs) *)
                                     !blockStatSeq_ in_)

(** {{{
  *  BlockExpr ::= <<< (CaseClauses | Block) >>>
  *  }}}
*)
and blockExpr in_ : block_expr =
  inBracesOrIndented
    (fun in_ ->
      match in_.token with
      | Kcase _ when nextTokNotClassOrObject in_ ->
          let xs = caseClauses in_ in
          (* AST: Match(EmptyTree, xs *)
          BECases xs
      | _ ->
          let xs = block in_ in
          BEBlock xs)
    in_

(* ------------------------------------------------------------------------- *)
(* Enumerator/generator *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Enumerators ::= Generator {semi Enumerator}
 *  Enumerator  ::=  Generator
 *                |  Guard
 *                |  Pattern1 `=` Expr
 *  }}}
*)

and isEnumeratorsEnd in_ =
  match in_.token with
  | Kdo _
  | Kyield _
  | RBRACE _ ->
      true
  | _ -> false

and enumerators in_ : enumerators =
  in_
  |> with_logging "enumerators" (fun () ->
         let enums = ref [] in
         let x = enumerator ~isFirst:true in_ in
         enums += x;
         while
           TH.isStatSep in_.token && not (lookingAhead isEnumeratorsEnd in_)
         do
           nextToken in_;
           let x = enumerator ~isFirst:false in_ in
           enums += x
         done;
         List.rev !enums)

(* pad: this was duplicated in enumerator and generator in the original code*)
and guard_loop in_ : guard list =
  let is_if =
    in_.token =~= Kif ab
    || lookingAhead
         (fun in_' ->
           if in_'.token =~= Kif ab then (
             nextToken in_;
             true)
           else false)
         in_
  in
  if not is_if then []
  else
    let g = guard in_ in
    let xs = guard_loop in_ in
    (* ast: makeFilter (g)::xs *)
    Option.to_list g @ xs

and enumerator ~isFirst ?(allowNestedIf = true) in_ : enumerator =
  in_
  |> with_logging "enumerator" (fun () ->
         match in_.token with
         | Ellipsis tok ->
             nextToken in_;
             GEllipsis tok
         | _ ->
             let g = generator ~eqOK:(not isFirst) ~allowNestedIf in_ in
             G g)

(** {{{
 *  Generator ::= Pattern1 (`<-` | `=`) Expr [Guard]
 *  }}}
*)
and generator ~eqOK ~allowNestedIf in_ : generator =
  in_
  |> with_logging "generator" (fun () ->
         let hasVal = in_.token =~= Kval ab in
         if hasVal then nextToken in_;
         let pat = noSeq pattern1 in_ in
         let hasEq = in_.token =~= EQUALS ab in
         (* CHECK: scala3: "`val` keyword in for comprehension is" *)
         let ieq = TH.info_of_tok in_.token in
         if hasEq && eqOK then nextToken in_ else accept (LARROW ab) in_;
         let rhs = expr in_ in
         let tail = if allowNestedIf then guard_loop in_ else [] in
         (* ast: gen.mkGenerator(genPos, pat, hasEq, rhs) :: tail *)
         { genpat = pat; gentok = ieq; genbody = rhs; genguards = tail })

(*****************************************************************************)
(* Parsing annotations  *)
(*****************************************************************************)

and readAnnots part in_ = separatedToken (AT ab) part in_

(** {{{
 *  Annotations       ::= {`@` SimpleType {ArgumentExprs}}
 *  ConstrAnnotations ::= {`@` SimpleType ArgumentExprs}
 *  }}}
*)
and annotationExpr in_ =
  let t = exprSimpleType in_ in
  let args =
    if in_.token =~= LPAREN ab then
      multipleArgumentExprs in_ (* ast: New (t, xs) *)
    else [] (* ast: New(t, Nil) *)
  in
  (t, args)

and annotations ~skipNewLines in_ : annotation list =
  in_
  |> with_logging (spf "annotations(skipNewLines:%b)" skipNewLines) (fun () ->
         readAnnots
           (fun iat in_ ->
             let t, args = annotationExpr in_ in
             if skipNewLines then newLineOpt in_;
             (iat, t, args))
           in_)

let annotTypeRest t in_ : type_ =
  let xs = annotations ~skipNewLines:false in_ in
  (* ast: fold around t makeAnnotated *)
  match xs with
  | [] -> t
  | xs -> TyAnnotated (t, xs)

(*****************************************************************************)
(* Parsing directives  *)
(*****************************************************************************)

(* For imports, we will parse a less restrictive version of the Scala grammar.
   Essentially, we notice that some sequence of dot-separated elements
   make up the primary path, namely "this", "super", and an identifier.
   Once that primary path is done, we may see a `as` or `=>`, and then
   another identifier.

   Otherwise, the primary path may also be terminated with a `*`, `given`,
   `_`, or brace-delimited list of import specifications.
*)

(** {{{
 *  NamedSelector     ::=  id [‘as’ (id | ‘_’)]
 *  WildCardSelector  ::=  ‘*' | ‘given’ [InfixType]
 *  }}}
*)
let wildCardSelector in_ =
  match in_.token with
  | USCORE ii ->
      skipToken in_;
      Left ("_", ii)
  | STAR ii ->
      skipToken in_;
      Left ("*", ii)
  | _ ->
      let ii = TH.info_of_tok in_.token in
      accept (ID_LOWER ("given", ab)) in_;
      (* only a keyword in Scala 3*)
      (* TODO: this type is only optional, is this too eager? *)
      if TH.isTypeIntroToken in_.token then
        let ty = startInfixType in_ in
        Right (ii, Some ty)
      else Right (ii, None)

(** {{{
 *  ImportSelector ::= Id [`=>` Id | `=>` `_`]  (scala 2)
                     | NamedSelector            (scala 3)
                     | WildCardSelector         (scala 3)
 *  }}}
*)
let importSelector in_ : import_selector =
  match in_.token with
  | STAR _
  | ID_LOWER ("given", _) ->
      WildCardSelector (wildCardSelector in_)
  | _ ->
      (* namedSelector case:
       *)
      let name = wildcardOrIdent in_ in
      let rename =
        match in_.token with
        | ARROW _
        | ID_LOWER ("as", _) ->
            nextToken in_;
            (* CHECK: "Wildcard import cannot be renamed" *)
            let alias = wildcardOrIdent in_ in
            Some alias
        (* AST: if name = nme.WILDCARD && !bbq => null *)
        | _ -> None
      in
      (* ast: ImportSelector(name, start, rename, renameOffset) *)
      NamedSelector (ImportId name, rename)

(** {{{
 *  ImportSelectors ::= `{` {ImportSelector `,`} (ImportSelector | `_`) `}`
 *  }}}
*)
let importSelectors in_ : import_selector list bracket =
  (* CHECK: "Wildcard import must be in last position" *)
  inBracesOrNil (commaSeparated importSelector) in_

(** {{{
 *  ImportExpr ::= StableId `.` (NamedSelector | WildCardSelector | `_` | ImportSelectors)
                 | StableId `as` id  (scala 3)
                 | (* semgrep-ext: allow this for things like `import $X` *)
                   metavariable
 *  }}}
*)

(* Since we just accumulate the entire list of the import path, we may have
   to run it back one step if we reach the end. That's because parsing
   a.b.c
   means that our path will be [`a`, `b`, `c`], but we really want to
   turn it into importing the name `c` from the path `a.b`.

   So this function just splits the last element, taking the "tl".
*)
let tl_path path =
  (* presumably this means the end of the stable id,
      with nothing after it *)
  match List.rev path with
  | last :: rest -> (List.rev rest, last)
  | [] -> failwith "shouldn't happen"

(* A single "path element". In reality, `super` and `this` are more
   restricted in where they can appear, but for our purposes we will
   be lenient.
*)
let read_path_elem in_ : import_path_elem =
  match in_.token with
  | Ksuper ii ->
      skipToken in_;
      ImportSuper ii
  | Kthis ii ->
      skipToken in_;
      ImportThis ii
  | _ ->
      let id = ident in_ in
      ImportId id

(* This walks the import path, reading things like a.b.c until it
   finds something different.
*)
let rec collect_import_path (path : import_path) in_ =
  match in_.token with
  (* All the things that can terminate an imported StableId.
  *)
  | ARROW _
  | ID_LOWER ("as", _) -> (
      nextToken in_;
      match in_.token with
      | USCORE _ ->
          (* This is something like `import a as _`
             Kind of a weird thing to write. I assume it's just binding it to a weird name.
          *)
          let ii = TH.info_of_tok in_.token in
          accept (USCORE ab) in_;
          let path, id = tl_path path in
          ImportExprSpec (path, ImportNamed (id, Some ("_", ii)))
      | _ ->
          let path, id = tl_path path in
          (* either "as" or `=>` *)
          let id' = ident in_ in
          ImportExprSpec (path, ImportNamed (id, Some id')))
  | DOT _ -> (
      accept (DOT ab) in_;
      match in_.token with
      (* import foo.bar._; (scala 2) *)
      | USCORE _
      (* import foo.bar.*; (scala 3) *)
      | STAR _
      (* import foo.bar.given; (scala 3) *)
      | ID_LOWER ("given", _) ->
          ImportExprSpec (path, ImportWildcard (wildCardSelector in_))
      (* import foo.bar.{ x, y, z } *)
      | LBRACE _ ->
          let internal = importSelectors in_ in
          ImportExprSpec (path, ImportSelectors internal)
      (* Otherwise, the import path just continues. *)
      | _ ->
          let new_path_elem = read_path_elem in_ in
          collect_import_path (path @ [ new_path_elem ]) in_)
  (* Here, we probably reached the end of the import expression.
   *)
  | _ ->
      let stable_id, id = tl_path path in
      ImportExprSpec (stable_id, ImportNamed (id, None))

(* To parse an import expression, first we find a single "element".
   Then, we collect a dot-separated list of them, until we have
   reason to read something else.
*)
and importExpr in_ : import_expr =
  in_
  |> with_logging "importExpr" (fun () ->
         (* Walks down import `foo.bar.baz.{ ... }` until it ends at
          * an underscore, a left brace, or an undotted identifier.
          *)
         let new_path_elem = read_path_elem in_ in
         collect_import_path [ new_path_elem ] in_)

(** {{{
 *  Import  ::= import ImportExpr {`,` ImportExpr}
 *  }}}
*)
let importClause in_ : import =
  let ii = TH.info_of_tok in_.token in
  accept (Kimport ab) in_;
  let xs = commaSeparated importExpr in_ in
  (ii, xs)

(** {{{
 *  Export  ::= export ImportExpr {`,` ImportExpr}
 *  }}}
*)
let exportClause in_ : import =
  let ii = TH.info_of_tok in_.token in
  accept (ID_LOWER ("export", ab)) in_;
  let xs = commaSeparated importExpr in_ in
  (ii, xs)

(*****************************************************************************)
(* Parsing modifiers  *)
(*****************************************************************************)

(* NOTE: soft modifiers
   This is necessary because of the existence of "soft keywords".
   These are modifiers like "inline" or "open", which can sometimes behave like
   modifiers, but only if certain conditions are met, and otherwise behave like
   identifiers.
   So in order to identify if it is a modifier, we need an extra lookahead.
*)
let is_modifier in_ =
  let next_is_soft_modifier_follower =
    lookingAhead (fun in_ -> TH.isSoftModifierFollower in_.token) in_
  in
  TH.isModifier in_.token
  ||
  match in_.token with
  | ID_LOWER ("inline", _)
  | ID_LOWER ("opaque", _)
  | ID_LOWER ("open", _) ->
      next_is_soft_modifier_follower
  | __else__ -> false

(* coupling: TH.isLocalModifier *)
let modifier_of_isLocalModifier_opt in_ =
  match in_.token with
  | Kabstract ii -> Some (Abstract, ii)
  | Kfinal ii -> Some (Final, ii)
  | Ksealed ii -> Some (Sealed, ii)
  | Kimplicit ii -> Some (Implicit, ii)
  | Klazy ii -> Some (Lazy, ii)
  | ID_LOWER ("inline", ii) when is_modifier in_ -> Some (Inline, ii)
  | ID_LOWER ("open", ii) when is_modifier in_ -> Some (Open, ii)
  | ID_LOWER ("opaque", ii) when is_modifier in_ -> Some (Opaque, ii)
  | _ -> None

(** {{{
 *  AccessQualifier ::= `[` (Id | this) `]`
 *  }}}
*)
let accessQualifierOpt in_ : ident_or_this bracket option =
  match in_.token with
  | LBRACKET lb ->
      nextToken in_;
      (* CHECK: "duplicate private/protected qualifier" *)
      let id =
        match in_.token with
        | Kthis ii ->
            nextToken in_;
            (* Flags.Local?? *)
            (AST.this, ii)
        | _ ->
            let x = identForType in_ in
            (* ast: Modifiers(mods.flags, x) *)
            x
      in
      let rb = TH.info_of_tok in_.token in
      accept (RBRACKET ab) in_;
      Some (lb, id, rb)
  | _ -> None

(** {{{
 *  AccessModifier ::= (private | protected) [AccessQualifier]
 *  }}}
*)
let accessModifierOpt in_ : modifier option =
  (* ast: normalizeModifiers *)
  match in_.token with
  | Kprivate ii ->
      nextToken in_;
      (* ast: flagToken(in_.token) *)
      let x = accessQualifierOpt in_ in
      Some (Private x, ii)
  | Kprotected ii ->
      nextToken in_;
      (* ast: flagToken(in_.token) *)
      let x = accessQualifierOpt in_ in
      Some (Protected x, ii)
  | _ -> None (* ast: noMods *)

(** {{{
 *  LocalModifiers ::= {LocalModifier}
 *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
 *  }}}
*)
let localModifiers in_ : modifier list =
  let rec loop mods in_ =
    (* old: if TH.isLocalModifier in_.token
     * then let mods = addMod mods in_.token in_ in loop mods in_
     * else mods
     *)
    match modifier_of_isLocalModifier_opt in_ with
    | Some x ->
        nextToken in_;
        loop (x :: mods) in_
    | None -> List.rev mods
  in
  loop noMods in_

(* ast: normalizeModifiers() *)
(* CHECK: "repeated modifier" *)
(* old: let addMod mods t in_ = nextToken in_; t::mods *)

(** {{{
 *  Modifiers ::= {Modifier}
 *  Modifier  ::= LocalModifier
 *              |  AccessModifier
 *              |  override
 *  }}}
*)
let modifiers in_ =
  (* ast: normalizeModifiers() *)
  let rec loop (mods : modifier list) =
    match in_.token with
    | Kprivate _
    | Kprotected _ ->
        let mopt = accessModifierOpt in_ in
        loop (Option.to_list mopt @ mods)
    (* old: let mods = addMod mods in_.token in_ in
     * let mods_bis = accessQualifierOpt in_ in
     * loop mods
     *)
    | Koverride ii ->
        nextToken in_;
        loop ((Override, ii) :: mods)
    | Kabstract _
    | Kfinal _
    | Ksealed _
    | Kimplicit _
    | Klazy _ ->
        (* old: let mods = addMod mods in_.token in_ in loop mods *)
        loop (List.rev (localModifiers in_) @ mods)
    (* see NOTE: soft modifiers
     *)
    | ID_LOWER _ when is_modifier in_ ->
        loop (List.rev (localModifiers in_) @ mods)
    | NEWLINE _ ->
        nextToken in_;
        loop mods
    | _ -> List.rev mods
  in
  loop noMods

(*****************************************************************************)
(* Parsing Methods/Functions  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Parameter *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  ParamType ::= Type | `=>` Type ['*'] | Type `*`
 *  }}}
*)
let paramType ?(repeatedParameterOK = true) in_ : param_type =
  ignore repeatedParameterOK;
  in_
  |> with_logging "paramType" (fun () ->
         match in_.token with
         | ARROW ii ->
             nextToken in_;
             let t = typ in_ in
             let star_opt =
               match in_.token with
               | tok when TH.isRawStar tok ->
                   let ii = TH.info_of_tok in_.token in
                   skipToken in_;
                   Some ii
               | _ -> None
             in
             (* ast: byNameApplication *)
             PTByNameApplication (ii, t, star_opt)
         | _ ->
             let t = typ in_ in
             if TH.isRawStar in_.token then (
               let ii = TH.info_of_tok in_.token in
               nextToken in_;
               (* CHECK: if (!repeatedParameterOK)
                * "repeated parameters are only allowed in method signatures" *)
               (* ast: repeatedApplication t *)
               PTRepeatedApplication (t, ii))
             else PT t (* ast: t *))

(** {{{
 *  ParamClauses      ::= {ParamClause} [[nl] `(` implicit Params `)`]
 *  ParamClause       ::= [nl] `(` [Params] `)`
                        | UsingParamClause
 *  UsingParamClause  ::= [nl] ‘(’ ‘using’ (DefParams | FunArgTypes) ‘)’
 *  Params            ::= Param {`,` Param}
 *  Param             ::= {Annotation} [`inline`] Id [`:` ParamType] [`=` Expr]
 *  ClassParamClauses ::= {ClassParamClause} [[nl] `(` implicit ClassParams `)`]
 *  ClassParamClause  ::= [nl] `(` [ClassParams] `)`
                        | [nl] ‘(’ ‘using’ (ClsParams | FunArgTypes) ‘)’
 *  ClassParams       ::= ClassParam {`,` ClassParam}
 *  ClassParam        ::= {Annotation}  [{Modifier} (`val` | `var`)] Id [`:` ParamType] [`=` Expr]
 *  }}}
*)
let param _owner implicitmod _caseParam in_ : binding =
  in_
  |> with_logging "param" (fun () ->
         let annots = annotations ~skipNewLines:false in_ in
         let mods =
           (* crazy?: if owner.isTypeName *)
           let xs = modifiers in_ in
           let inline =
             match in_.token with
             | ID_LOWER ("inline", ii) ->
                 nextToken in_;
                 [ (Inline, ii) ]
             | _ -> []
           in
           (* ast: mods = xs |= Flags.PARAMACCESSOR *)
           (* CHECK: "lazy modifier not allowed here. " *)
           implicitmod @ inline @ xs
           @
           match in_.token with
           | Kval ii ->
               nextToken in_;
               [ (Val, ii) ]
           | Kvar ii ->
               nextToken in_;
               (* ast: if (v == VAR) mods |= Flags.MUTABLE *)
               [ (Var, ii) ]
           | _ ->
               (* CHECK: if (mods.flags != Flags.PARAMACCESSOR) accept(VAL) *)
               (* ast: if (!caseParam) mods |= Flags.PrivateLocal *)
               []
           (* ast: if (caseParam) mods |= Flags.CASEACCESSOR *)
         in
         (* semgrep-ext: *)
         match in_.token with
         | Ellipsis ii ->
             nextToken in_;
             ParamEllipsis ii
         | _ ->
             let name = ident in_ in
             let tpt =
               match in_.token with
               | COLON _ ->
                   accept (COLON ab) in_;
                   if in_.token =~= ARROW ab then
                     ( (* CHECK: if owner.isTypeName && !mods.isLocalToThis
                          * "var/val parameters may not be call-by-name" *)
                       (* ast: bynamemod = Flags.BYNAMEPARAM *) );
                   Some (paramType in_)
               | _ -> None
             in
             let default =
               match in_.token with
               | EQUALS _ ->
                   nextToken in_;
                   (* ast: mods |= FLAGS.DEFAULTPARAM *)
                   let x = expr in_ in
                   Some x
               | _ ->
                   (* ast: emptyTree *)
                   None
             in
             (* ast: ValDef((mods|implicitmod|bynamemod) with annots, name.toTermName, tpt, default) *)
             let p_attrs = AST.mods_with_annots mods annots in
             ParamClassic
               { p_name = name; p_attrs; p_type = tpt; p_default = default })

(* parses the part of the using param clause, inside the parens *)
let usingParamClauseInner ~caseParam owner implicitmod in_ =
  match in_.token with
  | ID_LOWER ("using", ii) ->
      (* here you might have more params, or a type
          but these are not easy to distinguish from each other :(
      *)
      nextToken in_;
      let is_type =
        TH.isTypeIntroToken in_.token
        && lookingAhead (fun in_ -> not (in_.token =~= COLON ab)) in_
      in
      if is_type then
        let tys = types in_ in
        (List_.map (fun ty -> ParamType ty) tys, Some ii)
      else
        (* TODO: not right to reuse? *)
        (* no implciitmod?*)
        (commaSeparated (param owner implicitmod !caseParam) in_, Some ii)
  | _ -> error "expected using param clause" in_

let paramClauseInner ~caseParam ~endTok owner in_ : binding list * tok option =
  if in_.token =~= endTok then ([], None)
  else
    let implicitmod =
      match in_.token with
      | Kimplicit ii ->
          nextToken in_;
          (* AST: Flags.IMPLICIT *)
          [ (Implicit, ii) ]
      | _ -> []
    in
    match in_.token with
    | ID_LOWER ("using", _) ->
        usingParamClauseInner ~caseParam owner implicitmod in_
    | __else__ -> (commaSeparated (param owner implicitmod !caseParam) in_, None)

(* CHECK: "no by-name parameter type allowed here" *)
(* CHECK: "no * parameter type allowed here" *)
(* AST: convert tree to parameter *)
(* CHECK: Tuples cannot be directly destructured in method ... *)
(* CHECK: "identifier expected" *)
let paramClauses ~ofCaseClass owner _contextBoundBuf in_ : bindings list =
  let vds = ref [] in
  let caseParam = ref ofCaseClass in

  newLineOptWhenFollowedBy (LPAREN ab) in_;
  while in_.token =~= LPAREN ab do
    let x =
      inParens (paramClauseInner ~caseParam ~endTok:(RPAREN ab) owner) in_
    in
    vds += x;
    caseParam := false;
    newLineOptWhenFollowedBy (LPAREN ab) in_
  done;
  if ofCaseClass then
    (* AST: name(), elliptical(), *)
    (* CHECK: "case classes must have a parameter list" *)
    (* CHECK: "case classes must have a non-implicit parameter list" *)
    ();
  (* CHECK: "an implicit parameter section must be last" *)
  (* CHECK: "multiple implicit parameter sections are not allowed" *)
  (* CHECK: "parameter sections are effectively implicit" *)
  List.rev !vds
(* AST: if owner is CONSTRUCTOR && ... *)
(* CHECK: "no type parameters allowed here" *)
(* CHECK: "auxiliary constructor needs non-implicit parameter list" *)
(* AST: addEvidentParams (owner, result, contextBounds) *)

(* ------------------------------------------------------------------------- *)
(* Type Parameter *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TypeParamClauseOpt    ::= [TypeParamClause]
 *  TypeParamClause       ::= `[` VariantTypeParam {`,` VariantTypeParam} `]`]
 *  VariantTypeParam      ::= {Annotation} [`+` | `-`] TypeParam
 *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
 *  FunTypeParamClause    ::= `[` TypeParam {`,` TypeParam} `]`]
 *  ^ also known as "DefTypeParamClause" in Scala 3
 *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {`<%` Type} {`:` Type}
 *  }}}
*)
let rec typeParamClauseOpt _owner _contextBoundBuf in_ : type_parameters option
    =
  in_
  |> with_logging "typeParamClauseOpt" (fun () ->
         let typeParam in_ =
           in_
           |> with_logging "typeParam" (fun () ->
                  (* ast: ms | Flags.PARAM *)
                  let tpannots = annotations ~skipNewLines:true in_ in
                  let tpvariance =
                    match in_.token with
                    (* STILL? is supposed to be only if isTypeName owner *)
                    | PLUS ii ->
                        nextToken in_;
                        (* ast: mods |= Flags.COVARIANT *)
                        Some (Covariant, ii)
                    | MINUS ii ->
                        nextToken in_;
                        (* ast: mods |= Flags.CONTRAVARIANT *)
                        Some (Contravariant, ii)
                    | _ -> None
                  in
                  let tpname = wildcardOrIdent in_ in
                  (* AST: toTypeName *)
                  let tpparams = typeParamClauseOpt tpname None in_ in
                  let tpbounds = typeBounds in_ in
                  (* AST: TypeDef(mods, pname, tparams, xs) *)
                  let viewbounds = ref [] in
                  let colons = ref [] in
                  (*if contextBoundBuf <> None
                    then
                  *)
                  while in_.token =~= VIEWBOUND ab do
                    (* CHECK: scala3: "view bounds are unsupported" *)
                    skipToken in_;
                    let t = typ in_ in
                    (* AST: contextBoundBuf +=
                     *  makeFunctionTypeTree(List(Ident(pname)), t)
                     *)
                    viewbounds += t
                  done;
                  while in_.token =~= COLON ab do
                    skipToken in_;
                    let t = typ in_ in
                    (* AST: contextBoundBuf += AppliedTypeTree(t, List(Ident(pname)))*)
                    colons += t
                  done;
                  {
                    tpannots;
                    tpname;
                    tpvariance;
                    tpparams;
                    tpbounds;
                    tpviewbounds = List.rev !viewbounds;
                    tpcolons = List.rev !colons;
                  })
         in
         newLineOptWhenFollowedBy (LBRACKET ab) in_;
         match in_.token with
         | LBRACKET _ ->
             let x = inBrackets (fun in_ -> commaSeparated typeParam in_) in_ in
             Some x
         | _ -> None)

(* ------------------------------------------------------------------------- *)
(* Constructor body (as in 'def this(...) = { <body> }') *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
 *  }}}
*)
let selfInvocation _vparamss in_ : expr =
  let ithis = TH.info_of_tok in_.token in
  accept (Kthis ab) in_;
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  let xs = argumentExprs in_ in
  let xxs = ref [ xs ] in
  (* AST: t = Apply(Ident(nme.CONSTRUCTOR), xs) *)
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  while
    match in_.token with
    | LPAREN _
    | LBRACE _ ->
        true
    | _ -> false
  do
    let xs = argumentExprs in_ in
    (* AST: t = Apply(t, xs) *)
    newLineOptWhenFollowedBy (LBRACE ab) in_;
    xxs += xs
  done;
  (* AST: if classContextBounds is empty then t else
   *  Apply(t, vparamss.last.map(vp => Ident(vp.name)))
   *)
  let this = This (None, ithis) in
  let name = Name (this, []) in
  Apply (name, List.rev !xxs)

(** {{{
 *  ConstrBlock    ::=  `{` SelfInvocation {semi BlockStat} `}`
 *  }}}
*)
let constrBlock vparamss in_ : block bracket =
  let lb = TH.info_of_tok in_.token in
  skipToken in_;
  let x = selfInvocation vparamss in_ in
  let xs =
    if TH.isStatSep in_.token then (
      nextToken in_;
      !blockStatSeq_ in_)
    else []
  in
  let stats = E x :: xs in
  let rb = TH.info_of_tok in_.token in
  accept (RBRACE ab) in_;
  (* ast: Block(stats, literalUnit) *)
  (lb, stats, rb)

(** {{{
 *  ConstrExpr      ::=  SelfInvocation
 *                    |  ConstrBlock
 *  }}}
*)
let constrExpr vparamss in_ : expr =
  if in_.token =~= LBRACE ab then
    let x = constrBlock vparamss in_ in
    S (Block x)
  else
    let x = selfInvocation vparamss in_ in
    (* AST: Block(x :: Nil, literalUnit) *)
    x

(* ------------------------------------------------------------------------- *)
(* Def *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  FunDef ::= FunSig [`:` Type] `=` [`macro`] Expr
 *          |  FunSig [nl] `{` Block `}`
 *          |  `this` ParamClause ParamClauses
 *                 (`=` ConstrExpr | [nl] ConstrBlock)
 *  FunDcl ::= FunSig [`:` Type]
 *  FunSig ::= id [FunTypeParamClause] ParamClauses
 *  }}}
*)
let funDefRest fkind _attrs name in_ :
    function_definition * type_parameters option =
  in_
  |> with_logging "funDefRest" (fun () ->
         (* let newmods = ref attrs in *)
         (* contextBoundBuf is for context bounded type parameters of the form
          * [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
          * i.e. (B[T] or T => B)
          *)
         let contextBoundBuf = ref [] in
         let tparams = typeParamClauseOpt name (Some contextBoundBuf) in_ in
         let vparamss =
           paramClauses ~ofCaseClass:false name contextBoundBuf in_
         in
         newLineOptWhenFollowedBy (LBRACE ab) in_;
         let restype = (* AST: fromWithinReturnType *) typedOpt in_ in

         let rhs =
           match in_.token with
           | t when TH.isStatSep t || t =~= RBRACE ab ->
               (* CHECK: if restype = None then deprecated missing type, use : Unit *)
               (* AST: EmptyTree, newmods |= DEFERRED *)
               None
           | LBRACE _ when restype =*= None ->
               (* CHECK: missing type *)
               let x = blockExpr in_ in
               Some (FBlock x)
           | EQUALS ii ->
               nextTokenAllow TH.nme_MACROkw in_;
               if TH.isMacro in_.token then
                 nextToken in_ (* AST: newmmods |= MACRO *);
               let x = expr in_ in
               Some (FExpr (ii, x))
           | _ -> None
         in
         (* ast: DefDef(newmods, name.toTermName, tparams,vparamss,restype, rhs) *)
         (* CHECK: "unary prefix operator definition with empty parameter list.."*)
         ( { fkind; fparams = vparamss; frettype = restype; fbody = rhs },
           tparams ))

let funDefOrDcl attrs in_ : definition =
  in_
  |> with_logging "funDefOrDcl" (fun () ->
         let idef = TH.info_of_tok in_.token in
         nextToken in_;
         match in_.token with
         | Kthis ii ->
             skipToken in_;
             let classcontextBoundBuf = ref [] in
             (* AST: STILL? *)
             let name = (AST.this, ii (* ast: nme.CONSTRUCTOR *)) in
             (* pad: quite similar to funDefRest *)
             let vparamss =
               paramClauses ~ofCaseClass:false name classcontextBoundBuf in_
             in
             newLineOptWhenFollowedBy (LBRACE ab) in_;
             let (rhs : fbody) =
               match in_.token with
               | LBRACE _ ->
                   (* CHECK: "procedure syntax is deprecated for constructors" *)
                   let lb, xs, rb = constrBlock vparamss in_ in
                   FBlock (lb, BEBlock xs, rb)
               | EQUALS ii ->
                   accept (EQUALS ab) in_;
                   let x = constrExpr vparamss in_ in
                   FExpr (ii, x)
               | _ ->
                   (* generate error message *)
                   raise Impossible
             in
             (* ast: DefDef(mods, nme.CONSTRUCTOR, [], vparamss, TypeTree(), rhs)*)
             let ent = { name; attrs; tparams = None } in
             let fdef =
               {
                 fkind = (Def, idef);
                 fparams = vparamss;
                 frettype = None;
                 fbody = Some rhs;
               }
             in
             DefEnt (ent, FuncDef fdef)
         | _ ->
             let name = identOrMacro in_ in
             let fdef, tparams = funDefRest (Def, idef) attrs name in_ in
             let ent = { name; attrs; tparams } in
             DefEnt (ent, FuncDef fdef))

(*****************************************************************************)
(* Parsing types definitions or declarations  *)
(*****************************************************************************)

(** {{{
 *  TypeDef ::= type Id [TypeParamClause] `=` Type
 *            | FunSig `=` Expr
 *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
 *  }}}
*)
let typeDefOrDcl attrs in_ : definition =
  let itype = TH.info_of_tok in_.token in
  nextToken in_;
  newLinesOpt in_;
  let name = identForType in_ in
  (* a type alias as well as an abstract type may declare type parameters *)
  let tparams = typeParamClauseOpt name None in_ in
  let tbody =
    match in_.token with
    | EQUALS ii ->
        nextToken in_;
        let t = typ in_ in
        (* AST: TypeDef(mods, name, tparams, t) *)
        TDef (ii, t)
    | SEMI _
    | NEWLINE _
    | NEWLINES _
    | SUPERTYPE _
    | SUBTYPE _
    | RBRACE _
    | EOF _ ->
        let xs = typeBounds in_ in
        (* AST: TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds()) *)
        TDcl xs
    | _ -> error "`=`, `>:`, or `<:` expected" in_
  in
  let ent = { name; tparams; attrs } in
  let def = { ttok = itype; tbody } in
  DefEnt (ent, TypeDef def)

(*****************************************************************************)
(* Parsing Def or Dcl  *)
(*****************************************************************************)

(** {{{
  *  PatDef ::= Pattern2 {`,` Pattern2} [`:` Type] `=` Expr
  *  ValDcl ::= Id {`,` Id} `:` Type
  *  VarDef ::= PatDef | Id {`,` Id} `:` Type `=` `_`
  *  }}}
*)
let patDefOrDcl vkind attrs in_ : variable_definitions =
  nextToken in_;
  let lhs =
    commaSeparated
      (fun in_ ->
        let x = noSeq pattern2 in_ in
        stripParens x)
      in_
  in
  let tp = typedOpt in_ in
  let rhs =
    match in_.token with
    | EQUALS _ ->
        nextToken in_;
        let e = expr in_ in
        (* CHECK: "default initialization prohibited for literal-typed vars"*)
        (* AST: newmods |= DEFAULTINIT *)
        Some e
    | _ ->
        (* CHECK: if tp.isEmpty then accept EQUALS (* raise error *) *)
        (* AST: newmods |= DEFERRED *)
        None
  in
  (* CHECK: ensureNonOverlapping *)
  (* CHECK: "lazy values may not be abstract" *)
  (* CHECK: "pattern definition may not be abstract" *)
  (* AST: mkDefs (...) *)
  let attrs = AST.attrs_of_mods [ vkind ] @ attrs in
  { vattrs = attrs; vpatterns = lhs; vtype = tp; vbody = rhs }

(** {{{
 *  Def    ::= val PatDef
 *           | var PatDef
 *           | def DefDef
 *           | type [nl] TypeDef
 *           | TmplDef
 *  Dcl    ::= val PatDcl
 *           | var PatDcl
 *           | def FunDcl
 *           | type [nl] TypeDcl
 *  }}}
*)
let defOrDcl attrs in_ : definition =
  in_
  |> with_logging "defOrDcl" (fun () ->
         (* CHECK: "lazy not allowed here. Only vals can be lazy" *)
         match in_.token with
         | Kval ii -> VarDefs (patDefOrDcl (Val, ii) attrs (* ast:VAL *) in_)
         | Kvar ii ->
             VarDefs (patDefOrDcl (Var, ii) attrs (* ast:VAR and Mutable*) in_)
         | Kdef _ -> funDefOrDcl attrs (* ast:DEF *) in_
         | Ktype _ -> typeDefOrDcl attrs (* ast:TYPE *) in_
         (* classes/traits/objects (a.k.a templates) *)
         | _ -> !tmplDef_ attrs in_)

let nonLocalDefOrDcl in_ : definition =
  in_
  |> with_logging "nonLocalDefOrDcl" (fun () ->
         let annots = annotations ~skipNewLines:true in_ in
         let mods = modifiers in_ in
         (* ast: mods withAnnotations annots *)
         defOrDcl (mods_with_annots mods annots) in_)

let localDef implicitMod in_ : definition =
  in_
  |> with_logging "localDef" (fun () ->
         let annots = annotations ~skipNewLines:true in_ in
         let mods = localModifiers in_ in
         let mods = implicitMod @ mods in
         (* ast: let mods = mods | implicitMod withAnnotations annots *)
         (* crazy? parsing depends on AST *)
         let defs =
           (* STILL? !(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) *)
           defOrDcl (mods_with_annots mods annots) in_
           (* STILL?: else tmplDef mods in_ *)
         in
         defs
         (* AST: if RBRACE | CASE defs :+ literalUnit *))

(* ------------------------------------------------------------------------- *)
(* End Marker *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  EndMarker         ::=  ‘end’ EndMarkerTag    -- when followed by EOL
 *  EndMarkerTag      ::=  id | ‘if’ | ‘while’ | ‘for’ | ‘match’ | ‘try’
                        |  ‘new’ | ‘this’ | ‘given’ | ‘extension’ | ‘val’
 *  }}}
*)

let endMarker in_ : end_marker =
  let end_tok = TH.info_of_tok in_.token in
  accept (ID_LOWER ("end", ab)) in_;
  let end_kind = TH.info_of_tok in_.token in
  (* We could dispatch on the various cases, but every single case ends up
     with some kind of token. So let's just say that it's a single token.
  *)
  skipToken in_;
  { end_tok; end_kind }

(* ------------------------------------------------------------------------- *)
(* Extension *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  Extension         ::=  ‘extension’ [DefTypeParamClause] {UsingParamClause}
 *                         ‘(’ DefParam ‘)’ {UsingParamClause} ExtMethods
 *  ExtMethods        ::=  ExtMethod | [nl] <<< ExtMethod {semi ExtMethod} >>>
 *  ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
 *                      |  Export
 *  }}}
*)

let usingParamClauses ~owner in_ =
  let caseParam = ref false in
  let vds = ref [] in
  while in_.token =~= LPAREN ab do
    let x = inParens (usingParamClauseInner ~caseParam owner []) in_ in
    vds += x;
    caseParam := false;
    newLineOptWhenFollowedBy (LPAREN ab) in_
  done;
  List.rev !vds

let extMethod in_ : ext_method =
  match in_.token with
  | ID_LOWER ("export", _) -> ExtExport (exportClause in_)
  | _ ->
      let annots = annotations ~skipNewLines:true in_ in
      let mods = modifiers in_ in
      (* ast: mods withAnnotations annots *)
      ExtDef (!defOrDcl_ (mods_with_annots mods annots) in_)

let extMethodsInner in_ : ext_method list =
  let exts = ref [ extMethod in_ ] in
  acceptStatSepOpt in_;
  while not (TH.isStatSeqEnd in_.token) do
    exts += extMethod in_;
    acceptStatSepOpt in_
  done;
  List.rev !exts

let extMethods in_ =
  newLineOpt in_;
  opportunisticIndent in_;
  if Option.is_some (passedIndent in_) || in_.token =~= LBRACE ab then
    let _, x, _ = inBracesOrIndented extMethodsInner in_ in
    x
  else [ extMethod in_ ]

let extension in_ =
  let ext_tok = TH.info_of_tok in_.token in
  let owner = ("extension", ext_tok) in
  skipToken in_;
  let tparams = typeParamClauseOpt ("extension", ext_tok) None in_ in
  let caseParam = ref [] in
  let usings = ref [] in
  let params = ref [] in
  (* This is only slightly convoluted.
     The reasoning is because the grammar allows a single clause (with a
     single parameter) to be "sandwiched" in between two {UsingParamClause}s.

     This means that we have an unlimited amount of `using` clauses, with a
     `param` clause that must exist, somewhere in between.

     Our way of dealing with this will be to parse them separately, and then
     take the first `param` that we see as our canonical one. There should be
     only one.
  *)
  newLineOptWhenFollowedBy (LPAREN ab) in_;
  while in_.token =~= LPAREN ab do
    let inner =
      inParens
        (fun in_ ->
          match in_.token with
          | ID_LOWER ("using", _) ->
              Left (usingParamClauseInner ~caseParam owner [] in_)
          | _ -> Right (param owner [] caseParam in_))
        in_
    in
    match inner with
    | l, Left using, r -> usings += (l, using, r)
    | _, Right param, _ ->
        params += param;
        newLineOptWhenFollowedBy (LPAREN ab) in_
  done;
  let ext_methods = extMethods in_ in
  {
    ext_tok;
    ext_tparams = tparams;
    ext_using = !usings;
    ext_param =
      (match !params with
      | param :: _ -> param
      | _ -> failwith "should be impossible");
    ext_methods;
  }

(*****************************************************************************)
(* Parsing XxxStat and XxxStats  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Helpers *)
(* ------------------------------------------------------------------------- *)

let statSeq ?(errorMsg = "illegal start of definition") ?(rev = false) stat in_
    =
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) do
    (match stat in_ with
    | Some x -> stats += x
    | None -> if TH.isStatSep in_.token then () else error errorMsg in_);
    acceptStatSepOpt in_
  done;
  if rev then !stats else List.rev !stats

(* ------------------------------------------------------------------------- *)
(* BlockStat *)
(* ------------------------------------------------------------------------- *)
(** {{{
  *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
  *  BlockStat    ::= Import
  *                 | Annotations [implicit] [lazy] Def
  *                 | Annotations LocalModifiers TmplDef
  *                 | Expr1
  *                 | EndMarker
  *                 | Extension
  *                 |
  *  }}}
*)

let isCaseDefEnd in_ =
  match in_.token with
  | RBRACE _
  | EOF _ ->
      true
  | Kcase _ when nextTokNotClassOrObject in_ -> true
  | _ -> false

let blockStatSeqInner in_ : top_stat option =
  let acceptStatSepOptOrEndCase in_ =
    if not (TH.isCaseDefEnd in_.token) then acceptStatSepOpt in_
  in
  match in_.token with
  | ID_LOWER ("end", _) ->
      let x = endMarker in_ in
      Some (End x)
  | ID_LOWER ("extension", _) ->
      let x = extension in_ in
      Some (Ext x)
  | Kimport _ ->
      let x = importClause in_ in
      let res = I x in
      acceptStatSepOptOrEndCase in_;
      Some res
  | t when TH.isDefIntro t || TH.isLocalModifier t || TH.isAnnotation t ->
      let res =
        match in_.token with
        | Kimplicit ii ->
            skipToken in_;
            if TH.isIdentBool in_.token then
              E (implicitClosure (Implicit, ii) InBlock in_)
              (* ast: Flags.IMPLICIT*)
            else D (localDef [ (Implicit, ii) ] in_)
        | _ -> D (localDef [] in_)
      in
      acceptStatSepOptOrEndCase in_;
      Some res
  | t when TH.isExprIntro t ->
      let x = statement InBlock in_ in
      acceptStatSepOptOrEndCase in_;
      Some (E x)
  | t when TH.isStatSep t ->
      nextToken in_;
      None
  | _ when is_modifier in_ -> error "no modifiers allowed here" in_
  | _ -> error "illegal start of statement" in_

let blockStatSeq in_ : block_stat list =
  let stats = ref [] in
  while (not (TH.isStatSeqEnd in_.token)) && not (isCaseDefEnd in_) do
    match blockStatSeqInner in_ with
    | None -> ()
    | Some x -> stats += x
  done;
  List.rev !stats

(* This function is for parsing something which might be an expression,
   or just a regular block stat seq.

   This is hard to figure out because a block stat seq may itself start
   with an expression. You have to parse one such element, and then see if
   something suitable comes afterwards.

   It's also complicated because we might have contextual information over what
   token for sure terminates this block stat seq / expression. That's what the
   `until` argument is for. In this case, we don't even need to see a DEDENT.

   For instance, the "then" expression in an if-then-else obeys this form,
   because the "then" might be just an expression:
     if (true) 3 else 4
   or it might be an indented block:
     if (true) then
       val x = 2
     else 5
   but it is not necessarily the case that there needs to be a DEDENT. We might
   have:
     if (true) then
       1 else 4
*)
let indentedExprOrBlockStatSeqUntil ?until in_ =
  let hit_until tok =
    match until with
    | Some tok' -> tok' =~= tok
    | None -> false
  in
  let surround x = fb (Tok.unsafe_fake_tok "") x in
  let exp, close_indent =
    if Option.is_some in_.is_indented then (
      enterIndentRegion in_;
      (* This may be an indented block, in which case there's more stuff after
         the expr.
      *)
      match blockStatSeqInner in_ with
      | None ->
          (* I'm not convinced this is possible, but this should happen if it
             was like, a blockStatSeq separator. This should indicate we're
             in the BSS case.
          *)
          (BlockExpr (BEBlock (blockStatSeq in_) |> surround), true)
      | Some (E x) -> (
          match in_.token with
          | tok when hit_until tok ->
              (* This must have been a fake indentation region. We're looking at
                 just a simple expression that happens to be indented, after the if.
                 like
                 if
                   3 else 4
              *)
              (match in_.sepRegions with
              | DEDENT _ :: rest -> in_.sepRegions <- rest
              | _ -> ());
              (x, false)
          | DEDENT _ -> (x, true)
          | _ ->
              (BlockExpr (BEBlock (E x :: blockStatSeq in_) |> surround), true))
      | Some other ->
          (BlockExpr (BEBlock (other :: blockStatSeq in_) |> surround), true))
    else
      (* If it's not indented, we definitely have just a normal inline expression. *)
      (expr in_, false)
  in
  if close_indent then closeIndentRegion in_;
  exp

(* ------------------------------------------------------------------------- *)
(* TemplateStat *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TemplateStats    ::= TemplateStat {semi TemplateStat}
 *  TemplateStat     ::= Import
 *                     | Export
 *                     | Annotations Modifiers Def
 *                     | Annotations Modifiers Dcl
 *                     | Expr1
 *                     | super ArgumentExprs {ArgumentExprs}
 *                     | EndMarker
 *                     | Extension
 *                     |
 *                     | Annotations Modifiers EnumCase
 *                       ^ see "note: enum-body" and defOrDcl
 *  }}}
*)
let templateStat in_ : template_stat option =
  match in_.token with
  | Kimport _ ->
      let x = importClause in_ in
      Some (I x)
  | ID_LOWER ("export", _)
    when lookingAhead (fun in_ -> TH.isPathStart in_.token) in_ ->
      let x = exportClause in_ in
      Some (Ex x)
  | ID_LOWER ("end", _) ->
      let x = endMarker in_ in
      Some (End x)
  | ID_LOWER ("extension", _) ->
      let x = extension in_ in
      Some (Ext x)
  | t when TH.isDefIntro t || is_modifier in_ || TH.isAnnotation t ->
      let x = nonLocalDefOrDcl in_ in
      Some (D x)
  | t when TH.isExprIntro t ->
      let x = statement InTemplate in_ in
      Some (E x)
  | _ -> None

let templateStats in_ : template_stat list = statSeq templateStat in_

(* ------------------------------------------------------------------------- *)
(* TopStat *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TopStatSeq ::= TopStat {semi TopStat}
 *  TopStat ::= Annotations Modifiers Def  (see below for discrepancy with Scala 2)
 *            | Packaging
 *            | package object ObjectDef
 *            | Import
 *            | Export
 *            | EndMarker
 *            | Extension
 *            |
 *  }}}
*)
let topStat in_ : top_stat option =
  match in_.token with
  | Kpackage ii ->
      skipToken in_;
      let x = !packageOrPackageObject_ ii in_ in
      Some x
  | Kimport _ ->
      let x = importClause in_ in
      Some (I x)
  | ID_LOWER ("export", _)
    when lookingAhead (fun in_ -> TH.isPathStart in_.token) in_ ->
      let x = exportClause in_ in
      Some (Ex x)
  | ID_LOWER ("end", _) ->
      let x = endMarker in_ in
      Some (End x)
  | ID_LOWER ("extension", _) ->
      let x = extension in_ in
      Some (Ext x)
  (* This used to be a TmplDef, but in Scala 3, this can actually be any Def.
     Def is a superset of TmplDef anyways, so we lose nothing here.
  *)
  | t when TH.isAnnotation t || TH.isDefIntro t || is_modifier in_ ->
      let x = nonLocalDefOrDcl in_ in
      Some (D x)
  | _ -> None

let topStatSeq ?rev in_ : top_stat list =
  statSeq ?rev ~errorMsg:"expected class or object definition" topStat in_

(*****************************************************************************)
(* Parsing Template (classes/traits/objects)  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* "Template" *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TemplateStatSeq  ::= [id [`:` Type] `=>`] TemplateStats
 *  }}}
 * @param isPre specifies whether in early initializer (true) or not (false)
*)

let templateStatSeq ~isPre in_ : self_type option * block =
  ignore isPre;
  let self, firstOpt =
    (* a soft modifier like "inline" or "open" might be an expr intro, but
       also actually be a modifier.
       so let's check whether it's explicitly a modifier as well.
       see NOTE: soft modifiers
    *)
    (* we add this negation for "extension", because otherwise we will assume this is
       starting an expression, and never parse an extension declaration!
    *)
    if
      TH.isExprIntro in_.token
      && (not (is_modifier in_))
      (* We add this here, because there are some "soft" modifiers that can start
         a templateIntro, such as "extension", "end", etc.
         If we did not do this, template stats that start with those would always
         enter this expression case, even though they denote something else.
      *)
      && not (TH.isTemplateStatIntroSoftKeyword in_.token)
    then (
      let first = expr ~location:InTemplate in_ in
      match in_.token with
      | ARROW ii ->
          (* ast: case Typed(...) self := makeSelfDef(), convertToParam *)
          nextToken in_;
          let self_type =
            match param_of_e_opt first with
            | Some (id, topt) -> (id, topt, ii)
            (* stricter? *)
            | _ -> error "wrong self type syntax" in_
          in
          (Some self_type, None)
      | _ ->
          acceptStatSepOpt in_;
          (noSelfType, Some (E first)))
    else (noSelfType, None)
  in
  let xs = templateStats in_ in
  (self, Option.to_list firstOpt @ xs)

(** {{{
 *  TemplateBody ::= [nl] :<<< `{` TemplateStatSeq `}` >>>
 *  WithTemplateBody ::= <<< TemplateStatSeq >>>
 *  }}}
 * @param isPre specifies whether in early initializer (true) or not (false)
*)

let withTemplateBody in_ =
  inBracesOrIndented
    (fun in_ ->
      (* isPre: ??? *)
      templateStatSeq ~isPre:false in_)
    in_

let templateBody ~isPre in_ : template_body =
  match in_.token with
  | LBRACE _ ->
      (* ast: self, EmptyTree.asList *)
      inBraces (templateStatSeq ~isPre) in_
  | _ ->
      (* must be a colon *)
      accept (COLON ab) in_;
      let res = inIndented (templateStatSeq ~isPre) in_ in
      res

let templateBodyOpt ~parenMeansSyntaxError in_ : template_body option =
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  match in_.token with
  | COLON _ when isIndentAfterColon in_ -> Some (templateBody ~isPre:false in_)
  | LBRACE _ -> Some (templateBody ~isPre:false in_)
  | LPAREN _ ->
      if parenMeansSyntaxError then
        error "traits or objects may not have parameters" in_
      else error "unexpected opening parenthesis" in_
  | _ ->
      (* ast: noSelfType, [] *)
      None

(** {{{
 *  ClassParents       ::= AnnotType {`(` [Exprs] `)`} {with AnnotType}
 *  TraitParents       ::= AnnotType {with AnnotType}
 *  }}}
*)
let templateParents in_ : template_parents =
  let readAppliedParent () =
    let parent = startAnnotType in_ in
    let args =
      match in_.token with
      | LPAREN _ ->
          let xs = multipleArgumentExprs in_ in
          (* AST: fold_left Apply.apply xs *)
          xs
      | _ -> []
    in
    (parent, args)
  in
  let parent, args = readAppliedParent () in
  let cwith = ref [] in
  (* less: could use separatedToken (Kwith ab) instead of while *)
  while in_.token =~= Kwith ab do
    nextToken in_;
    let parent, args = readAppliedParent () in
    (* stricter? *)
    if args <> [] then error "only the first parent type can have arguments" in_;
    cwith += parent
  done;
  { cextends = Some (parent, args); cwith = List.rev !cwith }

(** {{{
 *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
 *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
 *  EarlyDefs     ::= `{` [EarlyDef {semi EarlyDef}] `}`
 *  EarlyDef      ::= Annotations Modifiers PatDef
 *  }}}
*)

let afterTemplate in_ body =
  match in_.token with
  | Kwith _ (* crazy? && self eq noSelfType *) ->
      (* CHECK: "use traint parameters instead" when scala3 *)
      let _earlyDefs = body (* CHECK: map ensureEarlyDef AST: filter *) in
      nextToken in_;
      let parents = templateParents in_ in
      let body1opt = templateBodyOpt ~parenMeansSyntaxError:false in_ in
      (* AST: earlyDefs @ *)
      (parents, body1opt)
  | _ -> (AST.empty_cparents, Some body)

let template in_ : template_parents * template_body option =
  in_
  |> with_logging "template" (fun () ->
         newLineOptWhenFollowedBy (LBRACE ab) in_;
         match in_.token with
         | COLON _ when isIndentAfterColon in_ ->
             let body = templateBody ~isPre:true in_ in
             afterTemplate in_ body
         | LBRACE _ ->
             let body = templateBody ~isPre:true in_ in
             afterTemplate in_ body
         | _ ->
             let parents = templateParents in_ in
             let bodyopt = templateBodyOpt ~parenMeansSyntaxError:false in_ in
             (parents, bodyopt))

(* AST: name, mods, constrMods, vparams *)

(** {{{
 *  ClassTemplateOpt ::= `extends` ClassTemplate | [[`extends`] TemplateBody]
 *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[TraitExtends] TemplateBody]
 *  TraitExtends     ::= `extends` | `<:` (deprecated)
 *  }}}
*)
let templateOpt ckind vparams in_ : template_definition =
  let cparents, cbody =
    match in_.token with
    | Kextends _
    | SUBTYPE _ (* CHECK: deprecated in 2.12.5 *) ->
        nextToken in_;
        template in_
    | _ ->
        newLineOptWhenFollowedBy (LBRACE ab) in_;
        let bodyopt =
          (* AST: mods.isTrait || name.isTermName *)
          let parenMeansSyntaxError = true in
          templateBodyOpt ~parenMeansSyntaxError in_
        in
        (AST.empty_cparents, bodyopt)
  in
  (* AST: Template(parents, self, anyvalConstructor()::body))
   * CHECK: "package object inheritance is deprecated"
   *)
  { ckind; cparams = vparams; cparents; cbody }

(* ------------------------------------------------------------------------- *)
(* Object *)
(* ------------------------------------------------------------------------- *)

(* pad: I've added isCase, it was passed via mods in the original code *)

(** {{{
 *  ObjectDef       ::= Id ClassTemplateOpt
 *  }}}
*)
let objectDef ?isCase ?isPackageObject attrs in_ : definition =
  in_
  |> with_logging "objectDef" (fun () ->
         let ikind = TH.info_of_tok in_.token in
         nextToken in_;
         (* 'object' *)
         let name = ident in_ in
         (* ast: mods if isPackageObject ... *)
         let mods =
           (match isCase with
           | None -> []
           | Some ii -> [ (CaseClassOrObject, ii) ])
           @
           match isPackageObject with
           | None -> []
           | Some ii -> [ (PackageObject, ii) ]
         in
         let ckind = (Object, ikind) in
         let tmpl = templateOpt ckind [] in_ in
         (* AST: ModuleDef (mods, name.toTermName, template) *)
         let ent =
           { name; attrs = attrs @ AST.attrs_of_mods mods; tparams = None }
         in
         DefEnt (ent, Template tmpl))

(* ------------------------------------------------------------------------- *)
(* Package object *)
(* ------------------------------------------------------------------------- *)

(* scala3: deprecated *)

(** Create a tree representing a package object, converting
 *  {{{
 *    package object foo { ... }
 *  }}}
 *  to
 *  {{{
 *    package foo {
 *      object `package` { ... }
 *    }
 *  }}}
*)
let packageObjectDef ipackage in_ : definition =
  objectDef noMods ~isPackageObject:ipackage in_
(* AST: gen.mkPackageObject(defn, pidPos, pkgPos) *)

let packageOrPackageObject ipackage in_ : top_stat =
  if in_.token =~= Kobject ab then
    let x = packageObjectDef ipackage in_ in
    (* ast: joinComment(x::Nil).head *)
    D x
  else
    let x = pkgQualId in_ in
    let body = inBracesOrColonIndented topStatSeq in_ in
    (* ast: makePackaging(x, body) *)
    let pack = (ipackage, x) in
    Packaging (pack, body)

(* ------------------------------------------------------------------------- *)
(* Class/trait *)
(* ------------------------------------------------------------------------- *)
let constructorAnnotations in_ : annotation list =
  in_
  |> with_logging "constructorAnnotations" (fun () ->
         readAnnots
           (fun iat in_ ->
             let t = exprSimpleType in_ in
             let (es : arguments) = argumentExprs in_ in
             (* ast: New(t, List(es)) *)
             (iat, t, [ es ]))
           in_)

(** {{{
 *  ClassDef ::= Id [TypeParamClause] ConstrAnnotations
 *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
 *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
 *  }}}
*)

(* pad: I added isTrait and isCase instead of abusing mods *)
let classDef ?(isTrait = false) ?isCase attrs in_ : definition =
  in_
  |> with_logging "classDef" (fun () ->
         let ikind = TH.info_of_tok in_.token in
         nextToken in_;
         (* 'class' or 'trait' *)
         let name = identForType in_ in

         (* AST: savingClassContextBounds *)
         let contextBoundBuf = ref [] in
         let tparams = typeParamClauseOpt name (Some contextBoundBuf) in_ in
         let classContextBounds = !contextBoundBuf in

         (* CHECK: "traits cannot have type parameters with context bounds" *)
         let constrAnnots =
           if not isTrait then constructorAnnotations in_ else []
         in

         let constrMods, vparamss =
           if isTrait then ([], [] (* AST: (Modifiers(Flags.TRAIT), List()) *))
           else
             let constrMods = accessModifierOpt in_ in
             let vparamss =
               paramClauses ~ofCaseClass:(isCase <> None) name
                 classContextBounds in_
             in
             (Option.to_list constrMods, vparamss)
         in

         let kind = ((if isTrait then Trait else Class), ikind) in
         let mods =
           match isCase with
           | None -> []
           | Some ii -> [ (CaseClassOrObject, ii) ]
         in
         (* ast: name ... constrMods withAnnotations...*)
         let attrs =
           mods_with_annots (mods @ constrMods) constrAnnots @ attrs
         in
         let ent = { name; attrs; tparams } in
         let tmpl = templateOpt kind vparamss in_ in
         (* AST: gen.mkClassDef(mods, name, tparams, template) *)
         (* CHECK: Context bounds generate implicit parameters (part of the template)
          *  with types from tparams: we need to ensure these don't overlap
          * ensureNonOverlapping(template, tparams)
          *)
         DefEnt (ent, Template tmpl))

(* ------------------------------------------------------------------------- *)
(* Enums *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  NB: This here is technically a "ClsTypeParamClause" from the Scala 3 grammar.
 *  From what I (Brandon) have observed, this seems to closely resemble the ordinary
 *  TypeParamClause from the Scala 2 grammar, so here I will assume it is sufficient
 *  to approximate it.
 *
 *  ConstrApps  ::=  ConstrApp ({‘,’ ConstrApp} | {‘with’ ConstrApp})
 *  ConstrApp   ::=  SimpleType1 {Annotation} {ParArgumentExprs}
 *
 *  EnumCase    ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)
 *  ClassConstr ::= [TypeParamClause] {Annotation} [AccessModifier] ClsParamClauses
 *
 *  EnumDef     ::= id ClassConstr InheritClauses EnumBody
 *  EnumBody    ::=  :<<< [SelfType] EnumStat {semi EnumStat} >>>
 *  EnumStat    ::=  TemplateStat
 *                |  {Annotation [nl]} {Modifier} EnumCase
 *
 *  note: enum-body
 *  `EnumBody` is essentially a superset of `TmplBody`, because `EnumStat` is
 *  a superset of `TemplateStat`.
 *  To make implementation easier, we will choose to simply extend `templateStat` to
 *  also handle the `EnumCase` case, since this will only permit more programs to be
 *  parsed.
 *  }}}
*)

let constrAppRest in_ : arguments list =
  let vds = ref [] in
  while in_.token =~= LPAREN ab do
    let x = parArgumentExprs in_ in
    vds += x;
    newLineOptWhenFollowedBy (LPAREN ab) in_
  done;
  List.rev !vds

let constrApp in_ : constr_app =
  let annotated_ty = annotType in_ in
  let arguments = constrAppRest in_ in
  (annotated_ty, arguments)

let constrApps in_ : constr_app list =
  let first = constrApp in_ in
  let constrs = ref [ first ] in
  while in_.token =~= COMMA ab || in_.token =~= Kwith ab do
    skipToken in_;
    constrs += constrApp in_
  done;
  List.rev !constrs

let classConstr name in_ =
  (* AST: savingClassContextBounds *)
  let contextBoundBuf = ref [] in
  let tparams = typeParamClauseOpt name (Some contextBoundBuf) in_ in
  let classContextBounds = !contextBoundBuf in

  (* CHECK: "traits cannot have type parameters with context bounds" *)
  let constrAnnots =
    (* TODO: These are actually optional. *)
    annotations ~skipNewLines:true in_
  in

  let access_mods, vparamss =
    let access_mods = accessModifierOpt in_ in
    let vparamss =
      (* ofCaseClass ??? *)
      paramClauses ~ofCaseClass:false name classContextBounds in_
    in
    (Option.to_list access_mods, vparamss)
  in
  (tparams, constrAnnots, access_mods, vparamss)

let enumCaseRest in_ : enum_case_definition =
  let id = ident in_ in

  match in_.token with
  | COMMA _ ->
      let ids = ref [ id ] in
      while in_.token =~= COMMA ab do
        accept (COMMA ab) in_;
        ids += ident in_
      done;
      EnumIds (List.rev !ids)
  | _ ->
      let etyparams, constrAnnots, access_mods, vparamss = classConstr id in_ in
      let eattrs = mods_with_annots access_mods constrAnnots in
      let constr_apps =
        match in_.token with
        | Kextends _ ->
            skipToken in_;
            constrApps in_
        | _ -> []
      in
      EnumConstr
        {
          eid = id;
          etyparams;
          eparams = vparamss;
          eattrs;
          eextends = constr_apps;
        }

let enumDef attrs in_ =
  in_
  |> with_logging "enumDef" (fun () ->
         (* 'enum' *)
         let ikind = TH.info_of_tok in_.token in
         accept (ID_LOWER ("enum", ab)) in_;

         let name = ident in_ in

         let tparams, constrAnnots, access_mods, vparamss =
           classConstr name in_
         in

         let kind = (Enum, ikind) in

         let attrs = mods_with_annots access_mods constrAnnots @ attrs in
         let ent = { name; attrs; tparams } in

         (* TODO: InheritClauses *)
         let tmpl = templateOpt kind vparamss in_ in
         (* AST: gen.mkClassDef(mods, name, tparams, template) *)
         (* CHECK: Context bounds generate implicit parameters (part of the template)
          *  with types from tparams: we need to ensure these don't overlap
          * ensureNonOverlapping(template, tparams)
          *)
         DefEnt
           ( { ent with attrs = M (EnumClass, ikind) :: ent.attrs },
             Template tmpl ))

(* ------------------------------------------------------------------------- *)
(* GivenDef *)
(* ------------------------------------------------------------------------- *)
(** {{{
 *  GivenDef ::= [GivenSig] (AnnotType [`=` Expr] | StructuralInstance)
 *  GivenSig ::= [id] [DefTypeParamClause] {UsingParamClause} `:`
 *  StructuralInstance ::= ConstrApp {`with` ConstrApp} [`with` WithTemplateBody]
 *  ConstrApp ::= SimpleType1 {Annotation} {ParArgumentExprs}
 *  }}}
*)

let structuralInstance (constr_app : constr_app) in_ : given_kind =
  let constrs = ref [] in
  let template_opt = ref None in
  while in_.token =~= Kwith ab do
    skipToken in_;
    (* This could be either a ConstrApp, or a WithTemplateBody.
       A WithTemplateBody is <<< >>>, meaning it can start with
       either an LBRACE or an indented first token.
       In the indented first token case, this could be an `id`,
       `this`, or `TemplateStatSeq`. That's kind of complicated.

       We'll just assume that starting with an indented first
       token means that we are inside of a `WithTemplateBody`.
    *)
    match in_.token with
    | LBRACE _ -> template_opt := Some (withTemplateBody in_)
    | _ when Option.is_some (passedIndent in_) ->
        template_opt := Some (withTemplateBody in_)
    | _ -> constrs += constrApp in_
  done;
  GivenStructural (constr_app :: List.rev !constrs, !template_opt)

let givenSig in_ : given_sig =
  let id_opt = ident_opt in_ in
  let owner =
    match id_opt with
    | None -> ("", Tok.unsafe_fake_tok "")
    | Some x -> x
  in
  let tparams = typeParamClauseOpt owner None in_ in
  let using_params = usingParamClauses ~owner in_ in
  let colon_ii = TH.info_of_tok in_.token in
  accept (COLON ab) in_;
  (* TODO: make name opt *)
  {
    g_id = id_opt;
    g_tparams = tparams;
    g_using = using_params;
    g_colon = colon_ii;
  }

(* This function is as implemented in the Dotty compiler.
 * https://github.com/lampepfl/dotty/blob/865aa639c98e0a8771366b3ebc9580cc8b61bfeb/compiler/src/dotty/tools/dotc/parsing/Parsers.scala#L879
 *)
let isGivenSigAfterIdent in_ =
  let rec skipParams in_ =
    if in_.token =~= LPAREN ab || in_.token =~= LBRACKET ab then (
      skipMatching in_;
      skipParams in_)
    else if in_.token =~= NEWLINE ab then (
      skipToken in_;
      skipParams in_)
  in
  skipParams in_;
  in_.token =~= COLON ab

let givenDef in_ : given_definition =
  let given_sig =
    match in_.token with
    | _ when TH.isIdentBool in_.token && lookingAhead isGivenSigAfterIdent in_
      ->
        Some (givenSig in_)
    | LBRACKET _ (* DefTypeParamClause *)
    | LPAREN _ (* UsingParamClause *)
    | COLON _ (* none of them *) ->
        Some (givenSig in_)
    | _ -> None
  in
  (* how to differentiate structuralInstance versus AnnotType?
     they both start with a type...

     The next thing we see can be either
       ConstrApp {`with` ConstrApp} [`with` WithTemplateBody]
     or
       AnnotType [`=` Expr]

     ConstrApp starts with SimpleType1 {Annotation}, meaning it
     basically is the same as AnnotType, except it also has args.

     So let's parse an AnnotType, and then look for a `(`, `with`, or `=`.
     This will tell us whether we enter the first or second case, respectively.
  *)
  let initAnnotType = annotType in_ in
  match in_.token with
  | EQUALS _ ->
      (* must be AnnotType `=` Expr *)
      accept (EQUALS ab) in_;
      let exp_opt = Some (expr in_) in
      { gsig = given_sig; gkind = GivenType (initAnnotType, exp_opt) }
  | LPAREN _ ->
      (* This means that our ConstrApp has arguments, and we are in
         StructuralInstance
      *)
      let first_constr_app_arguments = constrAppRest in_ in
      {
        gsig = given_sig;
        gkind =
          structuralInstance (initAnnotType, first_constr_app_arguments) in_;
      }
  | Kwith _ ->
      (* otherwise, StructuraLInstance *)
      { gsig = given_sig; gkind = structuralInstance (initAnnotType, []) in_ }
  | _ ->
      (* must be AnnotType *)
      { gsig = given_sig; gkind = GivenType (initAnnotType, None) }

(* ------------------------------------------------------------------------- *)
(* TmplDef *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TmplDef ::= [case] class ClassDef
 *            |  [case] object ObjectDef
 *            |  [override] trait TraitDef
 *            | `given` GivenDef
 *            | `enum` EnumDef
 *            | `case` (id ClassConstr [`extends` ConstrApps] | ids)
 *              ^ see "note: enum-body"
 *  }}}
*)
let tmplDef attrs in_ : definition =
  (* CHECK: "classes cannot be lazy" *)
  match in_.token with
  | Ktrait _ -> classDef ~isTrait:true attrs (* AST: | TRAIT | ABSTRACT *) in_
  | Kclass _ -> classDef attrs in_
  | Kobject _ -> objectDef attrs in_
  (* pad: was done via a lexing trick in postProcessToken in the original
   * code; not sure you needed that
   *)
  | Kcase ii -> (
      nextToken in_;
      match in_.token with
      | Kclass _ -> classDef ~isCase:ii attrs in_
      | Kobject _ -> objectDef ~isCase:ii attrs in_
      | _ when TH.isIdentBool in_.token -> EnumCaseDef (attrs, enumCaseRest in_)
      (* pad: my error message *)
      | _ -> error "expecting class or object after a case" in_)
  | ID_LOWER ("enum", _) -> enumDef attrs in_
  (* "given" is a soft keyword *)
  | ID_LOWER ("given", _) ->
      skipToken in_;
      GivenDef (givenDef in_)
  | _ -> error "expected start of definition" in_

(*****************************************************************************)
(* Entry points  *)
(*****************************************************************************)

(* set the forward reference *)
let _ =
  template_ := template;
  defOrDcl_ := defOrDcl;
  tmplDef_ := tmplDef;
  blockStatSeq_ := blockStatSeq;
  (indentedExprOrBlockStatSeqUntil_ :=
     fun env ~until -> indentedExprOrBlockStatSeqUntil ?until env);
  packageOrPackageObject_ := packageOrPackageObject;

  exprTypeArgs_ := exprTypeArgs;
  interpolatedString_ := interpolatedString;

  annotTypeRest_ := annotTypeRest;

  paramType_ := paramType;

  typeCaseClauses_ := typeCaseClauses;
  paramClauseInner_ := paramClauseInner;
  ()

(** {{{
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 *  }}}
*)
let compilationUnit in_ : top_stat list =
  let rec topstats in_ : top_stat list =
    let ts = ref [] in
    while in_.token =~= SEMI ab do
      nextToken in_
    done;
    (match in_.token with
    | Kpackage ipackage -> (
        nextToken in_;
        match in_.token with
        | Kobject _ ->
            let x = packageObjectDef ipackage in_ in
            ts += D x;
            if not (in_.token =~= EOF ab) then (
              acceptStatSep in_;
              let xs = topStatSeq ~rev:true in_ in
              ts ++= xs)
        | _ -> (
            let pkg = pkgQualId in_ in
            match in_.token with
            | EOF _ ->
                (* ast: makePackaging(pkg, []) *)
                let pack = Package (ipackage, pkg) in
                ts += pack
                (* newline: needed here otherwise parsed as package def *)
            | x when TH.isStatSep x ->
                let pack = Package (ipackage, pkg) in
                ts += pack;
                nextToken in_;
                let xs = topstats in_ in
                (* ast: makePackaging(pkg, xs) *)
                ts ++= List.rev xs
            | _ ->
                let xs = inBraces topStatSeq in_ in
                let pack = Packaging ((ipackage, pkg), xs) in
                (* ast: makePackaging(pkg, xs) *)
                ts += pack;
                acceptStatSepOpt in_;
                let xs = topStatSeq ~rev:true in_ in
                ts ++= xs))
    | _ ->
        let xs = topStatSeq ~rev:true in_ in
        ts ++= xs);
    (* ast: resetPackage *)
    List.rev !ts
  in
  (* ast:  case ... makeEmptyPackage ... *)
  topstats in_

let try_rule toks frule =
  let in_ = mk_env toks in
  init in_;
  let x = frule in_ in
  accept (EOF ab) in_;
  x

let parse toks =
  try try_rule toks compilationUnit with
  | Parsing_error.Syntax_error _ as err1 -> (
      let e1 = Exception.catch err1 in
      try try_rule toks block with
      | Parsing_error.Syntax_error _ -> Exception.reraise e1)

let semgrep_pattern toks =
  try try_rule toks (fun in_ -> Ex (expr in_)) with
  | Parsing_error.Syntax_error _ -> (
      try try_rule toks (fun in_ -> Ss (block in_)) with
      | Parsing_error.Syntax_error _ ->
          try_rule toks (fun in_ -> Pr (compilationUnit in_)))
