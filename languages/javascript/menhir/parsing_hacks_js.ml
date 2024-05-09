(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

module Flag = Flag_parsing
module Ast = Ast_js
module T = Parser_js
module TH = Token_helpers_js
module F = Ast_fuzzy
module Log = Log_parser_javascript.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to retag tokens
 * (e.g., a T_LPAREN in T_LPAREN_ARROW)
 * or insert tokens (e.g., T_VIRTUAL_SEMICOLON) to
 * help the grammar remains simple and unambiguous. See
 * lang_cpp/parsing/parsing_hacks.ml for more information about
 * this technique.
 *
 * This module inserts fake virtual semicolons, which is known as
 * Automatic Semicolon Insertion, or ASI for short.
 * Those semicolons can be ommitted by the user (but really should not).
 * ASI works in two steps:
 *  - certain tokens can not be followed by a newline (e.g., continue)
 *    and we detect those tokens in this file.
 *  - we also insert semicolons during error recovery in parser_js.ml. After
 *    all that was what the spec says.
 * Note that we need both techniques. See parse_js.ml comment for
 * the limitations of using just the second technique.
 *
 * reference:
 *  -http://www.bradoncode.com/blog/2015/08/26/javascript-semi-colon-insertion
 *  -http://www.ecma-international.org/ecma-262/6.0/index.html#sec-automatic-semicolon-insertion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
(* obsolete *)
let is_toplevel_keyword = function
  | T.T_IMPORT _ | T.T_EXPORT _
  | T.T_VAR _ | T.T_LET _ | T.T_CONST _
  | T.T_FUNCTION _
    -> true
  | _ -> false

(* obsolete *)
let rparens_of_if toks =
  let toks = Common.exclude TH.is_comment toks in

  let stack = ref [] in

  let rparens_if = ref [] in

  toks |> Common2.iter_with_previous_opt (fun prev x ->
    (match x with
     | T.T_LPAREN _ ->
         Common.push prev stack;
     | T.T_RPAREN info ->
         if !stack <> [] then begin
           let top = Common2.pop2 stack in
           (match top with
            | Some (T.T_IF _) ->
                Common.push info rparens_if
            | _ ->
                ()
           )
         end
     | _ -> ()
    )
  );
  !rparens_if
*)

(* alt: could have instead a better Ast_fuzzy type instead of putting
 * everything in the Tok category?
 *)
let is_identifier horigin (info : Tok.t) =
  match Hashtbl.find_opt horigin info with
  | Some (T.T_ID _) -> true
  | _ -> false

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* retagging:
 *  - '(' when part of an arrow expression
 *  - '{' when first token in sgrep mode
 *  - less: '<' when part of a polymorphic type (aka generic)
 *  - less: { when part of a pattern before an assignment
 *)
let fix_tokens toks =
  try
    let trees =
      Lib_ast_fuzzy.mk_trees
        { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
        toks
    in
    let horigin =
      toks
      |> List_.map (fun t -> (TH.info_of_tok t, t))
      |> Hashtbl_.hash_of_list
    in

    let retag_lparen_arrow = Hashtbl.create 101 in
    let retag_lparen_method = Hashtbl.create 101 in
    let retag_keywords = Hashtbl.create 101 in
    let retag_lbrace = Hashtbl.create 101 in

    (match trees with
    (* probably an object pattern
     * TODO: check that no stmt-like keywords inside body?
     *)
    | F.Braces (t1, _body, _) :: _ when !Flag_parsing.sgrep_mode ->
        Hashtbl.add retag_lbrace t1 true
    (* TODO: skip keywords, attributes that may be before the method id *)
    | F.Tok (_s, info) :: F.Parens (i1, _, _) :: F.Braces (_, _, _) :: _
      when !Flag_parsing.sgrep_mode && is_identifier horigin info ->
        Hashtbl.add retag_lparen_method i1 true
    | _ -> ());

    (* visit and tag *)
    let visitor =
      Lib_ast_fuzzy.mk_visitor
        {
          Lib_ast_fuzzy.default_visitor with
          Lib_ast_fuzzy.ktrees =
            (fun (k, _) xs ->
              (match xs with
              | F.Parens (i1, _, _) :: F.Tok ("=>", _) :: _res ->
                  Hashtbl.add retag_lparen_arrow i1 true
              (* TODO: also handle typed arrows! *)
              | F.Tok ("import", i1) :: F.Parens _ :: _res ->
                  Hashtbl.add retag_keywords i1 true
              | _ -> ());
              k xs);
        }
    in
    visitor trees;

    (* use the tagged information and transform tokens *)
    toks
    |> List_.map (function
         | T.T_LPAREN info when Hashtbl.mem retag_lparen_arrow info ->
             T.T_LPAREN_ARROW info
         | T.T_LPAREN info when Hashtbl.mem retag_lparen_method info ->
             T.T_LPAREN_METHOD_SEMGREP info
         | T.T_LCURLY info when Hashtbl.mem retag_lbrace info ->
             T.T_LCURLY_SEMGREP info
         | T.T_IMPORT info when Hashtbl.mem retag_keywords info ->
             T.T_ID (Tok.content_of_tok info, info)
         | x -> x)
  with
  | Lib_ast_fuzzy.Unclosed (msg, info) ->
      if !Flag.error_recovery then toks
      else raise (Parsing_error.Lexical_error (msg, info))

(*****************************************************************************)
(* ASI (Automatic Semicolon Insertion) part 1 *)
(*****************************************************************************)

let fix_tokens_ASI xs =
  let res = ref [] in
  let rec aux prev f xs =
    match xs with
    | [] -> ()
    | e :: l ->
        if TH.is_comment e then (
          Stack_.push e res;
          aux prev f l)
        else (
          f prev e;
          aux e f l)
  in

  let push_sc_before_x x =
    let info = TH.info_of_tok x in
    let fake = Ast.fakeInfoAttach info in
    Log.debug (fun m ->
        m "ASI: insertion fake ';' before %s" (Tok.stringpos_of_tok info));
    Stack_.push (T.T_VIRTUAL_SEMICOLON fake) res
  in

  let f prev x =
    (match (prev, x) with
    (* continue
     * <newline>
     *)
    | (T.T_CONTINUE _ | T.T_BREAK _), _
      when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x
    (* x
     * ++
     *
     * very conservative; should be any last(left_hand_side_expression)
     * but for that better to rely on ASI via parse-error recovery;
     * no ambiguity like for continue because
     *    if(true) x
     *    ++y;
     * is not valid.
     *)
    | (T.T_ID _ | T.T_FALSE _ | T.T_TRUE _), (T.T_INCR _ | T.T_DECR _)
      when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x
    (* Note that we would like to insert a ';' for semgrep patterns like:
     * xxxx()
     * ...
     *
     * But we can't because we also have patterns like
     *  if($X)
     *     ...
     * in which case we don't want the semicolon.
     * We need to rely on the parse-error-based ASI to handle this.
     *
     * Note that we used to support ellipsis in method chaining in semgrep
     * with a single '...' such as '$o.foo() ... .bar()' but when you wrote:
     *   $APP = express()
     *   ...
     * without the semicolon, it was interpreted as a method chaining
     *   $APP = express() ...
     * where we accept any method calls after express().
     * Before 0.35 we did not support this feature, so when the parser found
     *   $APP = express()
     *   ...
     * it internally first generated a parse error, and the ASI kicked in,
     * but after 0.35 we did not generate a parse error,
     * because it was a valid semgrep pattern, and so we don't get ASI
     * to trigger.
     * The only way to fix it is to change the syntax for method chaining
     * to require an extra dot, to disambiguate.
     *
     * old: does not work:
     * | (T.T_RPAREN _ | T.T_ID _), T.T_DOTS _
     * when TH.line_of_tok x <> TH.line_of_tok prev &&!Flag_parsing.sgrep_mode
     * -> push_sc_before_x x;
     *)
    | _ -> ());
    Stack_.push x res
  in

  (*
  (* obsolete *)
  let rparens_if = rparens_of_if xs in
  let hrparens_if = Common.hashset_of_list rparens_if in

  (* history: this had too many false positives, which forced
   * to rewrite the grammar to add extra virtual semicolons which
   * then make the whole thing worse
  *)
  let _fobsolete = (fun prev x ->
    match prev, x with
    (* { } or ; } TODO: source of many issues *)
    | (T.T_LCURLY _ | T.T_SEMICOLON _),
      T.T_RCURLY _ ->
        Common.push x res;
        (* <not } or ;> } *)
    | _,
      T.T_RCURLY _ ->
        push_sc_before_x x;
        Common.push x res;

        (* ; EOF *)
    | (T.T_SEMICOLON _),
      T.EOF _ ->
        Common.push x res;
        (* <not ;> EOF *)
    | _, T.EOF _ ->
        push_sc_before_x x;
        Common.push x res;

        (* }
         * <keyword>
        *)
    | T.T_RCURLY _,
      (T.T_ID _
      | T.T_IF _ | T.T_SWITCH _ | T.T_FOR _
      | T.T_VAR _  | T.T_FUNCTION _ | T.T_LET _ | T.T_CONST _
      | T.T_RETURN _
      | T.T_BREAK _ | T.T_CONTINUE _
      (* todo: sure? *)
      | T.T_THIS _ | T.T_NEW _
      ) when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res

    (* )
     * <keyword>
    *)
    (* this is valid only if the RPAREN is not the closing paren of an if*)
    | T.T_RPAREN info,
      (T.T_VAR _ | T.T_IF _ | T.T_THIS _ | T.T_FOR _ | T.T_RETURN _ |
       T.T_ID _ | T.T_CONTINUE _
      ) when TH.line_of_tok x <> TH.line_of_tok prev
          && not (Hashtbl.mem hrparens_if info) ->
        push_sc_before_x x;
        Common.push x res;


        (* ]
         * <keyword>
        *)
    | T.T_RBRACKET _,
      (T.T_FOR _ | T.T_IF _ | T.T_VAR _ | T.T_ID _)
      when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res;

        (* <literal>
         * <keyword>
        *)
    | (T.T_ID _
      | T.T_NULL _ | T.T_STRING _ | T.T_REGEX _
      | T.T_FALSE _ | T.T_TRUE _
      ),
      (T.T_VAR _ | T.T_ID _ | T.T_IF _ | T.T_THIS _ |
       T.T_RETURN _ | T.T_BREAK _ | T.T_ELSE _
      ) when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res;

        (* } or ; or , or =
         * <keyword> col 0
        *)
    | (T.T_RCURLY _ | T.T_SEMICOLON _ | T.T_COMMA _ | T.T_ASSIGN _),
      _
      when is_toplevel_keyword x &&
           TH.line_of_tok x <> TH.line_of_tok prev && TH.col_of_tok x = 0
      ->
        Common.push x res;

        (* <no ; or }>
         * <keyword> col 0
        *)
    | _, _
      when is_toplevel_keyword x &&
           TH.line_of_tok x <> TH.line_of_tok prev && TH.col_of_tok x = 0
      ->
        push_sc_before_x x;
        Common.push x res;


        (* else *)
    | _, _ ->
        Common.push x res;
  )
  in
   *)
  match xs with
  | [] -> []
  | x :: _ ->
      let sentinel =
        let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
        T.T_SEMICOLON fake
      in
      aux sentinel f xs;
      List.rev !res
