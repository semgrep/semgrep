(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

module TH = Token_helpers_php
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * The goal of this module is to help pretty print php code
 * that has been modified by spatch.
 * 
 * One could just run julien's pretty printer on the whole file, but
 * there are still a few style issues or comment idioms not handled by
 * the pretty printer. Because in many cases spatch modifies just a few
 * functions or methods in a file (called "chunks" below), we can 
 * just pretty print what has been modified and use the regular unparser
 * in lang_php/parsing/unparse_php.ml for the rest.
 * 
 * One could also improve the unparser used by spatch (as in coccinelle)
 * to handle newlines in spatch file and indentation, but I think it's
 * better to have some separation of concerns here: have spatch just
 * patch code (in a gross way) and have the pretty printer pretty print!
 * update: unparse_php.ml (and matcher/lib_unparser.ml) now actually 
 * also use better heuristics.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* A union of Ast.toplevel and Ast.class_stmt *)
type chunk =
  (* toplevel *)
  | Stmts of Cst_php.stmt list
  | Func of Cst_php.func_def
  | ClassHeader of Cst_php.class_def (* no body *)
  | ClassFooter of Cst_php.info (* just the closing brace *)

  | FinalDef of Cst_php.info
  (* class_stmt *)
  | ClassStmt of Cst_php.class_stmt

type chunks = (chunk * Parser_php.token list) list

type diff = Match | BnotinA | AnotinB

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let toks_before_after_ii ii toks =
  let (_min, max) = Parse_info.min_max_ii_by_pos ii in
  let toks_before_max, toks_after = 
    Common.profile_code "spanning tokens" (fun () ->
      toks |> Common2.span_tail_call (fun tok ->
        let info = TH.info_of_tok tok in
        match Parse_info.compare_pos info max with
        | -1 | 0 -> true
        | 1 -> 
            (* do exception for newline that we also include with the
             * chunk
             *)
            let pos_max = Parse_info.pos_of_info max in
            let pos_here = PI.pos_of_info info in
            (match tok with
            | Parser_php.TNewline _ when pos_here = pos_max + 1 ->
                true
            | _ -> false
            )
        | _ -> raise Impossible
      ))
  in
  toks_before_max, toks_after

let rec diff xs ys =
  match xs, ys with
  | [], [] -> []
  | ((_a1, _a2, sa) as a)::xs, ((_b1, _b2, sb) as b)::ys ->
      if sa =$= sb
      then (Match, b)::diff xs ys
      else
        if not (ys |> List.exists (fun (_, _, s) -> s =$= sa))
        then (AnotinB, a)::diff xs (b::ys)
        else (BnotinA, b)::diff (a::xs) ys
  | [], y::ys -> (BnotinA, y):: diff [] ys
  | x::xs, [] -> (AnotinB, x):: diff xs []
      

(* quite similar to Parse_php.distribute_info_items_toplevel *)
let split_chunks tokens ast =

  let rec aux xs toks =
  match xs with
  | [] -> raise Impossible
  | [Cst_php.FinalDef e] -> 
      (* assert (null toks) ? no cos can have whitespace tokens *)
      [FinalDef e, toks]
  | ast::xs ->
      let ii = Lib_parsing_php.ii_of_any (Cst_php.Toplevel ast) in
      let (toks_before_max, toks_after) = toks_before_after_ii ii toks in
      match ast with
      | Cst_php.StmtList stmts ->
          (Stmts stmts, toks_before_max)::aux xs toks_after
      | Cst_php.FuncDef def ->
          (Func def, toks_before_max)::aux xs toks_after
      | Cst_php.ConstantDef _ ->
          raise Common.Todo
      | Cst_php.TypeDef _ ->
          raise Common.Todo
      | Cst_php.NamespaceDef _  | Cst_php.NamespaceBracketDef _
      | Cst_php.NamespaceUse _
        ->
          raise Common.Todo
      | Cst_php.ClassDef def ->

          let toks = toks_before_max in
          let toks_after_class = toks_after in

          let (obrace, body, cbrace) = def.Cst_php.c_body in
          let just_class = { def with Cst_php.c_body = (obrace, [], cbrace) }in
          
          let ii_header = 
            Lib_parsing_php.ii_of_any 
              (Cst_php.Toplevel (Cst_php.ClassDef just_class)) 
            (* don't count ending } *)
            |> Common2.list_init
          in
          let toks_header, toks = 
            toks_before_after_ii ii_header toks in

          let chunks_body, toks =
            body |> List.fold_left (fun (acc, toks) class_stmt ->
              let ii = 
                Lib_parsing_php.ii_of_any (Cst_php.ClassStmt class_stmt) in
              let (toks_before_max, toks) = toks_before_after_ii ii toks in
              (ClassStmt class_stmt, toks_before_max)::acc, toks
            ) ([], toks)
          in
          let ii_footer = cbrace in
          let toks_footer, _toks =
            toks_before_after_ii [ii_footer] toks in
          
          let rest = aux xs toks_after_class in
          [ClassHeader just_class, toks_header] @
          List.rev chunks_body @
          [ClassFooter ii_footer, toks_footer] @
          rest

      | Cst_php.FinalDef _ 
      | Cst_php.NotParsedCorrectly _
        -> raise Impossible
  in
  aux ast tokens


let pretty_print _buf env chunk =
  let (chunk, toks) = chunk in
  match chunk with
  | Func def ->
      let ast = Ast_pp_build.toplevels toks [Cst_php.FuncDef def] in
      Pretty_print.stmts env ast
  | Stmts xs ->
      let ast = Ast_pp_build.toplevels toks [Cst_php.StmtList xs] in
      Pretty_print.stmts env ast

  | ClassHeader def ->
      let ast = Ast_pp_build.toplevels toks [Cst_php.ClassDef def] in
      Pretty_print.class_header env ast

  | ClassFooter _ -> 
      Pretty_print.class_footer env ()

  | ClassStmt x -> 
      let ast = Ast_pp_build.class_stmts toks [x] in
      Pretty_print.class_elements env ast
  | FinalDef _ -> ()

let unparse buf (_chunk, toks) =
  let pp s = Buffer.add_string buf s in

  let pp_tok tok = 
    let info = TH.info_of_tok tok in
      match info.Parse_info.token with
      | Parse_info.OriginTok _ -> pp (PI.str_of_info info)
      | Parse_info.ExpandedTok _ -> ()
      | Parse_info.FakeTokStr ("fake_token", _) -> ()
      | Parse_info.Ab | Parse_info.FakeTokStr _ -> raise Impossible
    in
  toks |> List.iter pp_tok;
  ()

let string_of_toks toks =
  let buf = Buffer.create 256 in
  unparse buf ((), toks);
  Buffer.contents buf

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* algorithm:
 *  - split in chunks (functions/methods/stmts) the old and new content
 *  - if no diff then use the conservative unparser
 *  - if diff then use the new pretty printer
 *)
let pretty_print_when_need_it ~oldfile ~newfile =
  
  let toks_old = Parse_php.tokens oldfile in
  let ast_old = Parse_php.parse_program oldfile in
  let toks_new = Parse_php.tokens newfile in
  let ast_new = Parse_php.parse_program newfile in

  let buf = Buffer.create 256 in
  let env = Pretty_print_code.empty (Buffer.add_string buf) in

  (* old: 
   * let ast = Ast_pp_build.program_with_comments toks_new ast_new in
   * Pretty_print.program_env env ast;
   *)
  let chunks_old = 
    split_chunks toks_old ast_old 
    |> List.map (fun (a, toks) -> a, toks, string_of_toks toks) in
  let chunks_new = split_chunks toks_new ast_new
    |> List.map (fun (a, toks) -> a, toks, string_of_toks toks) in

  let diffs = diff chunks_old chunks_new in
  diffs |> List.iter (function
  | Match, (bchunk, btoks, _bstr) ->
      (* if no change then use conservative unparser *)
      unparse buf (bchunk, btoks)
  (* new stuff *)
  | BnotinA, (bchunk, btoks, _bstr) ->
      pretty_print buf env (bchunk, btoks);
  | AnotinB, _ ->
      ()
  );
  let s = Buffer.contents buf in
  Common.write_file ~file:newfile s;
  ()
