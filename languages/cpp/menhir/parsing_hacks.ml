(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011,2014 Facebook
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
module Flag = Flag_parsing
module Flag_cpp = Flag_parsing_cpp
module TH = Token_helpers_cpp
module TV = Token_views_cpp
module T = Parser_cpp
module F = Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * This module tries to detect some cpp, C, or C++ idioms so that we can
 * parse as-is files by adjusting or commenting some tokens.
 *
 * Sometimes we use some name conventions, sometimes indentation information,
 * sometimes we do some kind of lalr(k) by finding patterns. We often try to
 * work on a better token representation, like ifdef-paren-ized, brace-ized,
 * paren-ized, so that we can pattern-match more easily
 * complex idioms (see token_views_cpp.ml).
 * We also try to get more contextual information such as whether the
 * token is in an initializer because many idioms are different
 * depending on the context (see token_views_context.ml).
 *
 * Examples of cpp idioms:
 *  - if 0 for commenting stuff (not always code, sometimes any text)
 *  - ifdef old version
 *  - ifdef funheader
 *  - ifdef statements, ifdef expression, ifdef-mid
 *  - macro toplevel (with or without a trailing ';')
 *  - macro foreach
 *  - macro higher order
 *  - macro declare
 *  - macro debug
 *  - macro no ';'
 *  - macro string, and macro function string taking param and ##
 *  - macro attribute
 *
 * Examples of C typedef idioms:
 *  - x * y
 *
 * Examples of C++ idioms:
 *  - x<...> for templates. People rarely do x < y > z to express
 *    relational expressions, so a < followed later by a > is probably a
 *    template.
 *
 * See the TIdent_MacroXxx in parser_cpp.mly and MacroXxx in ast_cpp.ml
 *
 * We also do other stuff involving cpp like expanding macros,
 * and we try to parse define body by finding the end of define virtual
 * end-of-line token. But now most of the code is actually in pp_token.ml
 * It is related to what is in the yacfe configuration file (e.g. standard.h)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let filter_comment_stuff xs =
  xs |> List.filter (fun x -> not (TH.is_comment x.TV.t))

(*****************************************************************************)
(* Post processing *)
(*****************************************************************************)

(* to do at the very very end *)
let insert_virtual_positions l =
  let strlen x = String.length (Tok.content_of_tok x) in
  let rec loop acc prev offset = function
    (* Tail-recursive to prevent stack overflows. *)
    | [] -> List.rev acc
    | x :: xs -> (
        let ii = TH.info_of_tok x in
        let inject pi =
          TH.visitor_info_of_tok
            (function
              | ii -> Ast_cpp.rewrap_pinfo pi ii)
            x
        in
        match ii with
        | Tok.OriginTok _pi ->
            let prev = Tok.unsafe_loc_of_tok ii in
            loop (x :: acc) prev (strlen ii) xs
        | Tok.ExpandedTok (pi, _) ->
            let acc' = inject (Tok.ExpandedTok (pi, (prev, offset))) :: acc in
            loop acc' prev (offset + strlen ii) xs
        | Tok.FakeTokStr (s, _) ->
            let acc' =
              inject (Tok.FakeTokStr (s, Some (prev, offset))) :: acc
            in
            loop acc' prev (offset + strlen ii) xs
        | Tok.Ab -> failwith "abstract not expected")
  in
  let rec skip_fake acc = function
    (* Tail-recursive to prevent stack overflows. *)
    | [] -> List.rev acc
    | x :: xs -> (
        let ii = TH.info_of_tok x in
        match ii with
        | Tok.OriginTok _pi ->
            let prev = Tok.unsafe_loc_of_tok ii in
            loop (x :: acc) prev (strlen ii) xs
        | _ -> skip_fake (x :: acc) xs)
  in
  skip_fake [] l

(*****************************************************************************)
(* C vs C++ *)
(*****************************************************************************)
let fix_tokens_for_language lang xs =
  xs
  |> Common.map (fun tok ->
         if lang =*= Flag_parsing_cpp.C && TH.is_cpp_keyword tok then
           let ii = TH.info_of_tok tok in
           T.TIdent (Tok.content_of_tok ii, ii)
         else tok)

(*****************************************************************************)
(* Fuzzy build *)
(*****************************************************************************)

let fix_tokens_fuzzy toks =
  try
    let trees =
      Lib_ast_fuzzy.mk_trees
        { Lib_ast_fuzzy.tokf = TH.info_of_tok; kind = TH.token_kind_of_tok }
        toks
    in
    let retag_lambda = Hashtbl.create 101 in

    let rec aux env trees =
      match trees with
      | [] -> ()
      (* [...] { } *)
      | F.Bracket (l, xs, _r) :: F.Braces _ :: ys ->
          Hashtbl.add retag_lambda l true;
          aux env xs;
          aux env ys
      | x :: xs ->
          (match x with
          | F.Parens (_, xs, _) -> iter_parens env xs
          | F.Braces (_, xs, _) -> aux env xs
          | F.Angle _
          | F.Bracket _
          | F.Metavar _
          | F.Dots _
          | F.Tok _ ->
              ());
          aux env xs
    and iter_parens env xs =
      xs
      |> List.iter (function
           | Left trees -> aux env trees
           | Right _comma -> ())
    in
    aux () trees;

    (* use the tagged information and transform tokens *)
    toks
    |> List.map (function
         | T.TOCro info when Hashtbl.mem retag_lambda info ->
             T.TOCro_Lambda info
         | x -> x)
  with
  | Lib_ast_fuzzy.Unclosed (msg, info) ->
      if !Flag.error_recovery then toks
      else raise (Parsing_error.Lexical_error (msg, info))

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)
(*
 * Main entry point for the token reclassifier which generates "fresh" tokens.
 *
 * The order of the rules is important. For instance if you put the
 * action heuristic first, then because of ifdef, can have not closed paren
 * and so may believe that higher order macro
 * and it will eat too much tokens. So important to do
 * first the ifdef heuristic.
 *
 * Note that the functions below work on a list of token_extended
 * or on views on top of a list of token_extended. The token_extended record
 * contains mutable fields which explains the (ugly but working) imperative
 * style of the code below.
 *
 * I recompute multiple times 'cleaner' cos the mutable
 * can have be changed and so we may have more comments
 * in the token original list.
 *)

(* we could factorize with fix_tokens_cpp, but for debugging purpose it
 * might be good to have two different functions and do far less in
 * fix_tokens_c (even though the extra steps in fix_tokens_cpp should
 * have no effect on regular C code).
 *)
let fix_tokens_c ~macro_defs tokens =
  let tokens = Parsing_hacks_define.fix_tokens_define tokens in
  let tokens = fix_tokens_for_language Flag_cpp.C tokens in

  let tokens2 = ref (tokens |> Common2.acc_map TV.mk_token_extended) in

  (* ifdef *)
  let cleaner = !tokens2 |> filter_comment_stuff in

  let ifdef_grouped = TV.mk_ifdef cleaner in
  Parsing_hacks_pp.find_ifdef_funheaders ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_bool ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_mid ifdef_grouped;

  (* macro part 1 *)
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in
  let paren_grouped = TV.mk_parenthised cleaner in
  Pp_token.apply_macro_defs macro_defs paren_grouped;
  (* because the before field is used by apply_macro_defs *)
  tokens2 := TV.rebuild_tokens_extented !tokens2;

  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  let paren_grouped = TV.mk_parenthised cleaner in
  Parsing_hacks_pp.find_define_init_brace_paren paren_grouped;
  Parsing_hacks_pp.find_string_macro_paren paren_grouped;
  Parsing_hacks_pp.find_macro_paren paren_grouped;
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  (* tagging contextual info (InFunc, InStruct, etc) *)
  let multi_grouped = TV.mk_multi cleaner in
  Token_views_context.set_context_tag_multi multi_grouped;
  let xxs = Parsing_hacks_typedef.filter_for_typedef multi_grouped in
  Parsing_hacks_typedef.find_typedefs xxs;

  insert_virtual_positions (!tokens2 |> Common2.acc_map (fun x -> x.TV.t))

let fix_tokens_cpp ~macro_defs tokens =
  let tokens = Parsing_hacks_define.fix_tokens_define tokens in

  (* let tokens = fix_tokens_for_language Flag_cpp.Cplusplus tokens in *)
  let tokens2 = ref (tokens |> Common2.acc_map TV.mk_token_extended) in

  (* ifdef *)
  let cleaner = !tokens2 |> filter_comment_stuff in

  let ifdef_grouped = TV.mk_ifdef cleaner in
  Parsing_hacks_pp.find_ifdef_funheaders ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_bool ifdef_grouped;
  Parsing_hacks_pp.find_ifdef_mid ifdef_grouped;

  (* macro part 1 *)
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  (* find '<' '>' template symbols. We need that for the typedef
   * heuristics. We actually need that even for the paren view
   * which is wrong without it.
   *
   * todo? expand macro first? some expand to lexical_cast ...
   * but need correct parenthized view to expand macros => mutually recursive :(
   *)
  Parsing_hacks_cpp.find_template_inf_sup cleaner;

  let paren_grouped = TV.mk_parenthised cleaner in
  Pp_token.apply_macro_defs macro_defs paren_grouped;

  (* because the before field is used by apply_macro_defs *)
  tokens2 := TV.rebuild_tokens_extented !tokens2;

  (* could filter also #define/#include *)
  let cleaner = !tokens2 |> filter_comment_stuff in

  (* tagging contextual info (InFunc, InStruct, etc). Better to do
   * that after the "ifdef-simplification" phase.
   *)
  let multi_grouped = TV.mk_multi cleaner in
  Token_views_context.set_context_tag_multi multi_grouped;

  (* macro part 2 *)
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  let paren_grouped = TV.mk_parenthised cleaner in
  let line_paren_grouped = TV.mk_line_parenthised paren_grouped in
  Parsing_hacks_pp.find_define_init_brace_paren paren_grouped;
  Parsing_hacks_pp.find_string_macro_paren paren_grouped;
  if not !Flag_parsing.sgrep_mode then (
    Parsing_hacks_pp.find_macro_lineparen line_paren_grouped;
    Parsing_hacks_pp.find_macro_paren paren_grouped);

  (* todo: at some point we need to remove that and use
   * a better filter_for_typedef that also
   * works on the nested template arguments.
   *)
  (* COMMENT OR STUFF NOT IN AST
     Parsing_hacks_cpp.find_template_commentize multi_grouped;
  *)
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  (* must be done before the qualifier filtering *)
  Parsing_hacks_cpp.find_constructor_outside_class cleaner;

  (* COMMENT OR STUFF NOT IN AST
     Parsing_hacks_cpp.find_qualifier_commentize cleaner;
  *)
  let cleaner = !tokens2 |> Parsing_hacks_pp.filter_pp_or_comment_stuff in

  let multi_grouped = TV.mk_multi cleaner in
  Token_views_context.set_context_tag_cplus multi_grouped;

  Parsing_hacks_cpp.find_constructor cleaner;

  (* TODO: need improve if we do not call find_template_commentize and
   * find_qualifier_commentize. We need to find a way to skip those
   * tokens just temporarily, and then put them back.
   *)
  let xxs = Parsing_hacks_typedef.filter_for_typedef multi_grouped in
  Parsing_hacks_typedef.find_typedefs xxs;

  (* must be done after the typedef inference *)
  Parsing_hacks_cpp.find_constructed_object_and_more cleaner;
  (* the pending of find_qualifier_comentize *)
  Parsing_hacks_cpp.reclassify_tokens_before_idents_or_typedefs multi_grouped;

  let toks =
    insert_virtual_positions (!tokens2 |> Common2.acc_map (fun x -> x.TV.t))
  in
  fix_tokens_fuzzy toks

let fix_tokens ~macro_defs lang a =
  Profiling.profile_code "C++ parsing.fix_tokens" (fun () ->
      match lang with
      | Flag_parsing_cpp.C -> fix_tokens_c ~macro_defs a
      | Flag_parsing_cpp.Cplusplus -> fix_tokens_cpp ~macro_defs a)
