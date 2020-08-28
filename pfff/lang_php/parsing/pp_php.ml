(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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

module Ast  = Cst_php
module Flag = Flag_parsing_php
module TH   = Token_helpers_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Preprocessor (e.g. XHP) helper functions.
 *  
 * update: now that pfff supports XHP construts directly, there is far
 * less need for preprocessor support and merging tokens.
 * 
 * 
 * Pfff allows the user to pass a preprocessor command on the command line,
 * which makes it possible to parse for instance XHP code.
 * 
 * The preprocessor will generate a file in /tmp which means
 * errors or further analysis will report position information
 * on this tmp file. This can be inconvenient. If the
 * preprocessor maintain line positions (which is the mostly case for instance
 * with XHP), we can slightly improve the situation by
 * changing the .file field in parse_info.
 * 
 * Preprocessors such as XHP also remove comments, spaces, and indentations.
 * It can be useful to merge the original comments/spaces/indents in the 
 * original file with the tokens in the expanded file. This makes it easier
 * to support refactoring on XHP code for most of the tokens. Our solution
 * to merge is: 
 *  - transform certain tokens in the original file as 
 *    TCommentPP tokens when they were transformed or passed by
 *    the preprocessor,
 *  - mark the new tokens from the preprocessed code with a ExpandedTok
 *  - adjust the file and position information from similar tokens with
 *    the information from the original file.
 * 
 * Merging is not always possible if the preprocessor introduce really new
 * tokens than the original PHP parser does not understand. In the case
 * of XHP, which mainly use '<', ':'. '>' and strings, this is mostly ok.
 * Nevertheless the original code can also contain 
 * quotes inside HTML snippet which confuse completely the 
 * original lexer which then produce tokens that are too different
 * to the tokens after preprocessing (see tests/xhp/xhp_line_bug4.php). 
 * It is then too hard to synchronize them. What is important 
 * when we have problems synchronizing, is to fall back to a default mode
 * where everything is marked as ExpandedTok and TCommentPP.
 *)

let (<=>) = Common2.(<=>)

(* a few helpers *)

let group_tokens_by_line toks = 

  let rec aux current_line current_toks  xs =
    match xs with
    | [] -> 
        [current_line,  List.rev current_toks]
    | x::xs ->
        let info = TH.info_of_tok x in
        let line = PI.line_of_info info in
        (match line <=> current_line with
        | Common2.Inf -> 
            failwith ("Impossible: wrong order of tokens: " ^ 
                         PI.str_of_info info)
        | Common2.Equal ->
            aux current_line (x::current_toks) xs
        | Common2.Sup -> 
            let hd = current_line, List.rev current_toks in
            hd::aux line [x] xs
        )
  in
  aux 1 [] toks

let comment_pp_ize toks = 
  toks |> List.map (fun tok -> 
    let info = TH.info_of_tok tok in 
    Parser_php.TCommentPP info
  )

let mark_as_expanded last_orig_parse_info toks =
  let cnt = ref (String.length (last_orig_parse_info.Parse_info.str)) in
  toks |> List.map (fun tok ->
    let info = TH.info_of_tok tok in
    let str = PI.str_of_info info in
    let len = String.length str in
    cnt := !cnt + len;
    tok |> TH.visitor_info_of_tok (fun info -> 
      let parse_info_in_pp = Parse_info.token_location_of_info info in
      { info with 
        Parse_info.token = Parse_info.ExpandedTok (
          parse_info_in_pp, 
          last_orig_parse_info, 
          !cnt)
      }
    )
  )


(* Merging tokens on a single line.
 *
 * As a first cut, we just want to know if this line has the same non-comment 
 * tokens in the original and preprocessed code. In that case the line 
 * didn't really needed XHP so we can return the original
 * set of tokens which will have better indentation, comments, etc.
 * Otherwise we return the tokens in the preprocessed code, but with
 * a special mark, ExpandedTok. We also add the original tokens 
 * as TCommentPP so that the unparser can still print back those
 * original tokens and pass the ExpandedTok one.
 * 
 * todo: could try to be more precise and merge more tokens inside
 * a line modified by XHP. Could do a diff at token level 
 * and adjust accordingly.
 * 
 *)
let merge_tokens_line ~orig_toks ~pp_toks = 
  let a = orig_toks in
  let b = pp_toks in

  let al_info_tok tok = 
    TH.visitor_info_of_tok (fun info -> Ast.al_info info) tok
  in
  
  let a' = Common.exclude TH.is_comment a |> List.map al_info_tok in
  let b' = Common.exclude TH.is_comment b |> List.map al_info_tok in

  if a' =*= b'
  then a
  else
    (* todo: could do finer grained things here *)
    let commented_a = comment_pp_ize a in
    if null commented_a
    then failwith "WEIRD: a XHP line has tokens but not the original one";
    
    let last_orig_info = Common2.list_last commented_a |> TH.info_of_tok in
    let last_orig_parse_info = 
      Parse_info.token_location_of_info last_orig_info in

    let expanded_b = mark_as_expanded last_orig_parse_info b in

    commented_a @ expanded_b



let  zip_and_sync ~toks_orig_lines ~toks_pp_lines = 
  (* old: List.map snd toks_pp_lines +> List.flatten *)

  (* This is used below just in one very ugly situation *)
  let last_orig_tok = ref
    (match toks_orig_lines with
    | (_xline, xtoks)::_xs -> 
        List.hd xtoks
    | _ -> failwith  
        "Impossible: if the file is empty then we should not be called at all"
    )
  in

  let rec aux xs ys = 
    match (xs, ys) with
    | [], [] -> []

    | ((_xline, xtoks)::xs), [] -> 
        (* The original can have some comments at the end of the file, which
         * would be removed by XHP
         *
         * TODO: assert only space or eof 
         *)
        xtoks::aux xs []

    | [], (_yline, ytoks)::ys -> 
        (* XHP usually cut the space and comments at the end of the file 
         * so the original file should always be longer.
         * 
         * Nevertheless, in certain situations where the tokens 
         * are very different with very different line positions, 
         * sync can get confused. See xhp_line_bug4.php. So the best
         * we can do here is to fall back to our invariant and 
         * mark the tokens as ExpandedTok.
         *)
        let all_remaining_toks = 
          (ytoks::List.map snd ys) |> List.flatten
        in

        let last_orig_parse_info = 
          !last_orig_tok |> TH.info_of_tok |> Parse_info.token_location_of_info in
        let toks = mark_as_expanded last_orig_parse_info all_remaining_toks in
        [toks]
        
        
    | (((xline, xtoks)::xs) as a), (((yline, ytoks)::ys) as b) -> 
        last_orig_tok := Common2.list_last xtoks;
        (match xline <=> yline with
        | Common2.Inf ->
            (* Sometimes XHP just remove certain tokens, like
             * lines with   attribute x y; in which case we must
             * remove them also from the original file. 
             * 
             * It's also usually because comments are passed by XHP.
             * We could maybe assert to have only space  here or 
             * tokens known to be passed by XHP like attribute
             *)
            let xtoks' = comment_pp_ize xtoks in
            xtoks'::aux xs b
        | Common2.Equal ->
            let merged = merge_tokens_line ~orig_toks:xtoks ~pp_toks:ytoks in
            merged::aux xs ys
        | Common2.Sup ->
            (* sometimes XHP remove some lines ... so have to adjust things 
             *
             *)
            pr2 (spf "WEIRD, wrong line numbers in preprocessed file %d > %d"
                     xline yline);
                    
            let b' = b |> List.map (fun (yline, ytoks) -> yline+1, ytoks) in
            aux a b'
        )
  in
  aux toks_orig_lines toks_pp_lines |> List.flatten


(* Trying to merge tokens from the original file with the preprocessed file,
 * for instance to put back space, indentation and comments information
 * in the preprocessed file.
 *)
let adapt_tokens_pp2 ~tokenizer ~orig_filename toks_pp = 

  (* The old algorithm was just to adjust the .file field so that error 
   * reporting was slightly improved.
   *)
  if not !Flag.obsolete_merge_tokens_xhp then
   toks_pp |> List.rev_map (fun tok ->
    tok |> TH.visitor_info_of_tok (fun ii ->
      let pinfo = ii.PI.token in
      { ii with Parse_info.token =
          match ii.PI.token with
          | Parse_info.OriginTok pi ->
              Parse_info.OriginTok { pi with
                Parse_info.file = orig_filename;
              }
          | Parse_info.FakeTokStr _
          | Parse_info.Ab  
            -> pinfo

          | Parse_info.ExpandedTok _ -> 
              raise Impossible
      })
  )  |> List.rev (* ugly, but need tail-call rev_map and so this rev *)
  else 
    (* algo: 
     *  - split by line the tokens 
     *  - for each line try to synchronize, 
     *    by marking as TCommentPP the relevant tokens from toks_orig
     *    and adding one from toks as ExpandedTok
     *)
    let toks_orig = tokenizer orig_filename in

    let toks_pp_lines   = group_tokens_by_line toks_pp in
    let toks_orig_lines = group_tokens_by_line toks_orig in
    zip_and_sync ~toks_orig_lines ~toks_pp_lines

let adapt_tokens_pp ~tokenizer ~orig_filename toks_pp =
  Common.profile_code "Parse_php.merge tokens" (fun () -> 
    adapt_tokens_pp2 ~tokenizer ~orig_filename toks_pp)
