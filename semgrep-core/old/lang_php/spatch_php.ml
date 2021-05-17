(* Yoann Padioleau
 *
 * Copyright (C) 2010-2012 Facebook
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
open Cst_php
open Parse_info
module V = Visitor_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See https://github.com/facebook/pfff/wiki/Spatch
 *
 * Here is an example of a spatch file:
 *
 *    foo(2,
 * -      bar(2)
 * +      foobar(4)
 *       )
 *
 * This will replace all calls to bar(2) by foobar(4) when
 * the function call is the second argument of a call to
 * foo where its first argument is 2.
 *
 *
 * todo: can we produce syntactically incorrect code? Yes.
 * Also now that static_scalar is actually a scalar, one could
 * do - 1 + foo() which should not be allowed. But there are many
 * things in spatch we actually allow (we don't really look at the + part),
 * so ...
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* right now only Expr and Stmt are supported *)
type pattern = Cst_php.any

type line_kind = XContext | XPlus of string | XMinus

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
 ./spatch -c tests/php/spatch/foo.spatch tests/php/spatch/foo.php
*)

(* just to test the backend part of spatch *)
let (_dumb_spatch_pattern : Cst_php.expr) =
  (* ./pfff -dump_php_ml tests/php/spatch/1.php *)
  let i_1 =
    {
      PI.token =
        PI.OriginTok
          {
            PI.str = "1";
            charpos = 6;
            line = 2;
            column = 0;
            file = "tests/php/spatch/1.php";
          };
      (* the spatch is to replace every 1 by 42 *)
      PI.transfo = PI.Replace (PI.AddStr "42");
    }
  in
  Sc (C (Int ("1", i_1)))

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

(*
 * Algorithm to parse a spatch file:
 *  - take lines of the file, index the lines
 *  - replace the + lines by an empty line and remember in a line_env
 *    the line and its index
 *  - remove the - in the first column and remember in a line_env
 *    that is was a minus line
 *  - unlines the filtered lines into a new string
 *  - call the PHP expr parser on this new string
 *  - go through all tokens and adjust its transfo field using the
 *    information in line_env
 *)
let parse file =
  let xs = Common.cat file |> Common.index_list_1 in

  let hline_env = Hashtbl.create 11 in

  let ys =
    xs
    |> List.map (fun (s, lineno) ->
           match s with
           (* ugly: for now I strip the space after the + because.
            * at some point we need to parse this stuff and
            * add the correct amount of indentation when it's processing
            * a token.
            *)
           | _ when s =~ "^\\+[ \t]*\\(.*\\)" ->
               let rest_line = Common.matched1 s in
               Hashtbl.add hline_env lineno (XPlus rest_line);
               ""
           | _ when s =~ "^\\-\\(.*\\)" ->
               let rest_line = Common.matched1 s in
               Hashtbl.add hline_env lineno XMinus;
               rest_line
           | _ when s =~ "^[ \t]+[-+]" ->
               failwith
                 "you must put the minus or plus annotations in the first \
                  column"
           | _ ->
               Hashtbl.add hline_env lineno XContext;
               s)
  in
  let spatch_without_patch_annot = Common2.unlines ys in

  (* pr2 spatch_without_patch_annot; *)
  let pattern =
    Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
        try Parse_php.any_of_string spatch_without_patch_annot
        with Parsing.Parse_error ->
          failwith (spf "could not parse: %s" spatch_without_patch_annot))
  in

  (* need adjust the tokens in it now *)
  let toks = Lib_parsing_php.ii_of_any pattern in

  (* adjust with Minus info *)
  toks
  |> List.iter (fun tok ->
         let line = Parse_info.line_of_info tok in
         let annot =
           try Hashtbl.find hline_env line with Not_found -> raise Not_found
         in
         (* nosem *)
         match annot with
         | XContext -> ()
         | XMinus -> tok.PI.transfo <- PI.Remove
         | XPlus _ ->
             (* normally impossible since we removed the elements in the
              * plus line, except the newline. should assert it's only newline
              *)
             ());

  (* adjust with the Plus info. We need to annotate the last token
   * on the preceding line or next line.
   * e.g. on
   *     foo(2,
   *   +        42,
   *         3)
   * we could either put the + on the ',' of the first line (as an AddAfter)
   * or on the + on the '3' of the thirdt line (as an AddBefore).
   * The preceding and next line could also be a minus line itself.
   * Also it could be possible to have multiple + line in which
   * case we want to concatenate them together.
   *
   * TODO: for now I just associate it with the previous line ...
   * what if the spatch is:
   *   + foo();
   *     bar();
   * then there is no previous line ...
   *)
  let grouped_by_lines =
    toks |> Common.group_by_mapped_key (fun tok -> Parse_info.line_of_info tok)
  in
  let rec aux xs =
    match xs with
    | (line, toks_at_line) :: rest ->
        (* if the next line was a +, then associate with the last token
         * on this line
         *)
        ( match Common2.hfind_option (line + 1) hline_env with
        | None ->
            (* probably because was last line *)
            ()
        | Some (XPlus toadd) -> (
            (* todo? what if there is no token on this line ? *)
            let last_tok = Common2.list_last toks_at_line in

            (* ugly hack *)
            let toadd =
              match Parse_info.str_of_info last_tok with
              | ";" -> "\n" ^ toadd
              | _ -> toadd
            in

            match last_tok.transfo with
            | Remove -> last_tok.transfo <- Replace (AddStr toadd)
            | NoTransfo -> last_tok.transfo <- AddAfter (AddStr toadd)
            | _ -> raise Impossible )
        | Some _ -> () );
        aux rest
    | [] -> ()
  in
  aux grouped_by_lines;

  (* both the ast (here pattern) and the tokens share the same
   * reference so by modifying the tokens we actually also modifed
   * the AST.
   *)
  pattern |> Metavars_php.check_pattern

let parse_string spatch_str =
  Common2.with_tmp_file ~str:spatch_str ~ext:".spatch" (fun file -> parse file)

let spatch ?(case_sensitive = false) pattern file =
  let was_modifed = ref false in

  (* quite similar to what we do in main_sgrep.ml *)
  let ast, tokens =
    try Parse_php.parse file |> fst
    with Parse_php.Parse_error _err ->
      Common.pr2 (spf "warning: parsing problem in %s" file);
      ([], [])
  in

  let hook =
    match pattern with
    | Expr (XhpHtml xhp) ->
        {
          V.default_visitor with
          V.kxhp_html =
            (fun (k, _) x ->
              let matches_with_env = Matching_php.match_xhp_xhp xhp x in
              if matches_with_env = [] then k x
              else (
                was_modifed := true;
                Transforming_php.transform_xhp_xhp xhp x
                  (* TODO, maybe could get multiple matching env *)
                  (List.hd matches_with_env) ));
        }
    | Expr pattern_expr ->
        {
          V.default_visitor with
          V.kexpr =
            (fun (k, _) x ->
              let matches_with_env = Matching_php.match_e_e pattern_expr x in
              if matches_with_env = [] then k x
              else (
                was_modifed := true;
                Transforming_php.transform_e_e pattern_expr x
                  (* TODO, maybe could get multiple matching env *)
                  (List.hd matches_with_env) ));
        }
    | Stmt2 pattern ->
        {
          V.default_visitor with
          V.kstmt =
            (fun (k, _) x ->
              let matches_with_env = Matching_php.match_st_st pattern x in
              if matches_with_env = [] then k x
              else (
                was_modifed := true;
                Transforming_php.transform_st_st pattern x
                  (* TODO, maybe could get multiple matching env *)
                  (List.hd matches_with_env) ));
        }
    | Hint2 pattern ->
        {
          V.default_visitor with
          V.khint_type =
            (fun (k, _) x ->
              let matches_with_env = Matching_php.match_hint_hint pattern x in
              if matches_with_env = [] then k x
              else (
                was_modifed := true;
                Transforming_php.transform_hint_hint pattern x
                  (* TODO, maybe could get multiple matching env *)
                  (List.hd matches_with_env) ));
        }
    | _ ->
        failwith
          ( spf "pattern not yet supported:"
          ^ "TODO" (* Export_ast_php.ml_pattern_string_of_any pattern*) )
  in
  Common.save_excursion Php_vs_php.case_sensitive case_sensitive (fun () ->
      (V.mk_visitor hook) (Program ast));

  if !was_modifed then
    Some
      (Unparse_php.string_of_program_with_comments_using_transfo (ast, tokens))
  else None
