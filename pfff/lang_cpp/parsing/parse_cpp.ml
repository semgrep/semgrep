(* Yoann Padioleau
 *
 * Copyright (C) 2002-2013 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

module Flag = Flag_parsing
module PI = Parse_info
module Stat = Parse_info
module FT = File_type

module Ast = Cst_cpp
module Flag_cpp = Flag_parsing_cpp
module T = Parser_cpp
module TH = Token_helpers_cpp
module Lexer = Lexer_cpp
module Semantic = Parser_cpp_mly_helper

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * A heuristic-based C/cpp/C++ parser.
 * 
 * See "Parsing C/C++ Code without Pre-Preprocessing - Yoann Padioleau, CC'09"
 * avalaible at http://padator.org/papers/yacfe-cc09.pdf
 *)
let use_dypgen = false

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type toplevels_and_tokens = (Ast.toplevel * Parser_cpp.token list) list

let program_of_program2 xs = 
  xs |> List.map fst

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag.verbose_parsing

(*****************************************************************************)
(* Error diagnostic *)
(*****************************************************************************)

let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Stats on what was passed/commentized  *)
(*****************************************************************************)

let commentized xs = xs |> Common.map_filter (function
  | T.TComment_Pp (cppkind, ii) -> 
      if !Flag_cpp.filter_classic_passed
      then 
        (match cppkind with
        | Token_cpp.CppOther -> 
            let s = PI.str_of_info ii in
            (match s with
            | s when s =~ "KERN_.*" -> None
            | s when s =~ "__.*" -> None
            | _ -> Some (ii.PI.token)
            )
             
        | Token_cpp.CppDirective | Token_cpp.CppAttr | Token_cpp.CppMacro
            -> None
        | Token_cpp.CppMacroExpanded
        | Token_cpp.CppPassingNormal
        | Token_cpp.CppPassingCosWouldGetError 
          -> raise Todo
        )
      else Some (ii.PI.token)
      
  | T.TAny_Action ii ->
      Some (ii.PI.token)
  | _ -> 
      None
 )
  
let count_lines_commentized xs = 
  let line = ref (-1) in
  let count = ref 0 in
  commentized xs |> List.iter (function
    | PI.OriginTok pinfo 
    | PI.ExpandedTok (_,pinfo,_) -> 
        let newline = pinfo.PI.line in
        if newline <> !line
        then begin
          line := newline;
          incr count
        end
    | _ -> ()
  );
  !count


(* See also problematic_lines and parsing_stat.ml *)

(* for most problematic tokens *)
let is_same_line_or_close line tok =
  TH.line_of_tok tok =|= line ||
  TH.line_of_tok tok =|= line - 1 ||
  TH.line_of_tok tok =|= line - 2

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* called by parse below *)
let tokens2 file = 
 let table     = Parse_info.full_charpos_to_pos_large file in

 Common.with_open_infile file (fun chan -> 
  let lexbuf = Lexing.from_channel chan in

    let rec tokens_aux () = 
      let tok = Lexer.token lexbuf in
      (* fill in the line and col information *)
      let tok = tok |> TH.visitor_info_of_tok (fun ii -> 
        { ii with PI.token=
          (* could assert pinfo.filename = file ? *)
          match ii.PI.token with
          |  PI.OriginTok pi ->
             PI.OriginTok (Parse_info.complete_token_location_large file 
                             table pi)
          | PI.ExpandedTok (pi,vpi, off) ->
              PI.ExpandedTok(
                (Parse_info.complete_token_location_large file table pi),vpi, 
                off)
          | PI.FakeTokStr (s,vpi_opt) -> PI.FakeTokStr (s,vpi_opt)
          | PI.Ab -> raise Impossible
      })
      in

      if TH.is_eof tok
      then [tok]
      else tok::(tokens_aux ())
    in
    tokens_aux ()
 )
   
let tokens a = 
  Common.profile_code "Parse_cpp.tokens" (fun () -> tokens2 a)

(*****************************************************************************)
(* Fuzzy parsing *)
(*****************************************************************************)

let rec multi_grouped_list xs = 
  xs |> List.map multi_grouped

and multi_grouped = function
  | Token_views_cpp.Braces (tok1, xs, (Some tok2)) ->
      Ast_fuzzy.Braces (tokext tok1, multi_grouped_list xs, tokext tok2)
  | Token_views_cpp.Parens (tok1, xs, (Some tok2)) ->
      Ast_fuzzy.Parens (tokext tok1, multi_grouped_list_comma xs, tokext tok2)
  | Token_views_cpp.Angle (tok1, xs, (Some tok2)) ->
      Ast_fuzzy.Angle (tokext tok1, multi_grouped_list xs, tokext tok2)
  | Token_views_cpp.Tok (tok) ->
    (match PI.str_of_info (tokext tok) with
    | "..." -> Ast_fuzzy.Dots (tokext tok)
    | s when Ast_fuzzy.is_metavar s -> Ast_fuzzy.Metavar (s, tokext tok)
    | s -> Ast_fuzzy.Tok (s, tokext tok)
    )
  | _ -> failwith "could not find closing brace/parens/angle"
and tokext tok_extended =
  TH.info_of_tok tok_extended.Token_views_cpp.t
and multi_grouped_list_comma xs =
  let rec aux acc xs =
  match xs with
  | [] ->
      if null acc
      then []
      else [Left (acc |> List.rev |> multi_grouped_list)]
  | (x::xs) ->
      (match x with
      | Token_views_cpp.Tok tok when PI.str_of_info (tokext tok) = "," ->
          let before = acc |> List.rev |> multi_grouped_list in
          if null before
          then aux [] xs
          else (Left before)::(Right (tokext tok))::aux [] xs
      | _ ->
        aux (x::acc) xs
      )
  in
  aux [] xs


(* This is similar to what I did for OPA. This is also similar
 * to what I do for parsing hacks, but this fuzzy AST can be useful
 * on its own, e.g. for a not too bad sgrep/spatch.
 * 
 * note: this is similar to what cpplint/fblint of andrei does? 
 *)
let parse_fuzzy file =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  let toks_orig = tokens file in
  let toks = 
    toks_orig |> Common.exclude (fun x ->
      Token_helpers_cpp.is_comment x || Token_helpers_cpp.is_eof x
    )
  in
  let extended = toks |> List.map Token_views_cpp.mk_token_extended in
  Parsing_hacks_cpp.find_template_inf_sup extended;
  let groups = Token_views_cpp.mk_multi extended in
  let trees = multi_grouped_list groups in
  let hooks = { Lib_ast_fuzzy.
     kind = TH.token_kind_of_tok;
     tokf = TH.info_of_tok
  } in
  trees, Lib_ast_fuzzy.mk_tokens hooks toks_orig
  )

(*****************************************************************************)
(* Extract macros *)
(*****************************************************************************)

(* It can be used to to parse the macros defined in a macro.h file. It 
 * can also be used to try to extract the macros defined in the file 
 * that we try to parse *)
let extract_macros2 file = 
  Common.save_excursion Flag.verbose_lexing false (fun () -> 
    let toks = tokens (* todo: ~profile:false *) file in
    let toks = Parsing_hacks_define.fix_tokens_define toks in
    Pp_token.extract_macros toks
  )
let extract_macros a = 
  Common.profile_code_exclusif "Parse_cpp.extract_macros" (fun () -> 
    extract_macros2 a)

(* less: pass it as a parameter to parse_program instead ? 
 * old: was a ref, but a hashtbl.t is actually already a kind of ref
 *) 
let (_defs : (string, Pp_token.define_body) Hashtbl.t)  = 
  Hashtbl.create 101


(* We used to have also a init_defs_builtins() so that we could use a
 * standard.h containing macros that were always useful, and a macros.h
 * that the user could customize for his own project.
 * But this was adding complexity so now we just have _defs and people
 * can call add_defs to add local macro definitions.
 *)
let add_defs file =
  if not (Sys.file_exists file)
  then failwith (spf "Could not find %s, have you set PFFF_HOME correctly?"
                   file);
  pr2 (spf "Using %s macro file" file);
  let xs = extract_macros file in
  xs |> List.iter (fun (k, v) -> Hashtbl.add _defs k v)

let init_defs file =     
  Hashtbl.clear _defs;
  add_defs file

(*****************************************************************************)
(* Error recovery *)
(*****************************************************************************)
(* see parsing_recovery_cpp.ml *)

(*****************************************************************************)
(* Consistency checking *)
(*****************************************************************************)
(* todo: a parsing_consistency_cpp.ml *)
      
(*****************************************************************************)
(* Helper for main entry point *)
(*****************************************************************************)

(* Hacked lex. This function use refs passed by parse. 
 * 'tr' means 'token refs'. This is used mostly to enable
 * error recovery (This used to do lots of stuff, such as
 * calling some lookahead heuristics to reclassify
 * tokens such as TIdent into TIdent_Typeded but this is
 * now done in a fix_tokens style in parsing_hacks_typedef.ml.
 *)
let rec lexer_function tr = fun lexbuf -> 
  match tr.PI.rest with
  | [] -> (pr2 "LEXER: ALREADY AT END"; tr.PI.current)
  | v::xs -> 
      tr.PI.rest <- xs;
      tr.PI.current <- v;
      tr.PI.passed <- v::tr.PI.passed;

      if !Flag.debug_lexer then pr2_gen v;

      if TH.is_comment v
      then lexer_function (*~pass*) tr lexbuf
      else v

(* was a define ? *)
let passed_a_define tr =
  let xs = tr.PI.passed |> List.rev |> Common.exclude TH.is_comment in
  if List.length xs >= 2 
   then 
      (match Common2.head_middle_tail xs with
      | T.TDefine _, _, T.TCommentNewline_DefineEndOfMacro _ -> true
      | _ -> false
      )
   else begin
     pr2 "WIERD: length list of error recovery tokens < 2 ";
     false
   end

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
(* 
 * note: as now we go in two passes, there is first all the error message of
 * the lexer, and then the error of the parser. It is not anymore
 * interwinded.
 * 
 * !!!This function use refs, and is not reentrant !!! so take care.
 * It uses the _defs global defined above!!!!
 *)
let parse_with_lang ?(lang=Flag_parsing_cpp.Cplusplus) file = 

  let stat = Parse_info.default_stat file in
  let filelines = Common2.cat_array file in

  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  let toks_orig = tokens file in

  let toks = 
    try Parsing_hacks.fix_tokens ~macro_defs:_defs lang toks_orig
    with Token_views_cpp.UnclosedSymbol s ->
      pr2 s;
      if !Flag_cpp.debug_cplusplus 
      then raise (Token_views_cpp.UnclosedSymbol s)
      else toks_orig
  in

  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  let rec loop () =

    let info = TH.info_of_tok tr.PI.current in
    (* todo?: I am not sure that it represents current_line, cos maybe
     * tr.current partipated in the previous parsing phase, so maybe tr.current
     * is not the first token of the next parsing phase. Same with checkpoint2.
     * It would be better to record when we have a } or ; in parser.mly,
     *  cos we know that they are the last symbols of external_declaration2.
     *)
    let checkpoint = PI.line_of_info info in
    (* bugfix: may not be equal to 'file' as after macro expansions we can
     * start to parse a new entity from the body of a macro, for instance
     * when parsing a define_machine() body, cf standard.h
     *)
    let checkpoint_file = PI.file_of_info info in

    tr.PI.passed <- [];
    (* for some statistics *)
    let was_define = ref false in

    let parse_toplevel tr lexbuf_fake =
      if not use_dypgen
      then Parser_cpp.toplevel (lexer_function tr) lexbuf_fake 
      else
       let (save1, save2, save3) = tr.PI.rest, tr.PI.current, tr.PI.passed in
       try 
         Parser_cpp.toplevel (lexer_function tr) lexbuf_fake 
       with _e ->
         tr.PI.rest <- save1; tr.PI.current <- save2; tr.PI.passed <- save3;
         (try 
           Parser_cpp2.toplevel (lexer_function tr) lexbuf_fake 
           |> List.hd |> fst
          with Failure "hd" -> 
            pr2 "no elements"; 
            raise Parsing.Parse_error
          )
    in

    let elem = 
      (try 
          (* -------------------------------------------------- *)
          (* Call parser *)
          (* -------------------------------------------------- *)
          parse_toplevel tr lexbuf_fake
        with e -> 
          if not !Flag.error_recovery 
          then raise (Parse_info.Parsing_error (TH.info_of_tok tr.PI.current));

          if !Flag.show_parsing_error then
            (match e with
            (* ocamlyacc *)
            | Parsing.Parse_error 
            (* dypgen *)
            | Dyp.Syntax_error
            (* menhir *)
            | Parser_cpp.Error -> 
              pr2 ("parse error \n = " ^ error_msg_tok tr.PI.current)
            | Semantic.Semantic (s, _i) -> 
              pr2 ("semantic error " ^s^ "\n ="^ error_msg_tok tr.PI.current)
            | e -> raise e
            );

          let line_error = TH.line_of_tok tr.PI.current in

          let pbline =
            tr.PI.passed
            |> List.filter (is_same_line_or_close line_error)
            |> List.filter TH.is_ident_like
          in
          let error_info =
            (pbline |> List.map (fun tok->PI.str_of_info (TH.info_of_tok tok))),
            line_error 
          in
          stat.Stat.problematic_lines <-
            error_info::stat.Stat.problematic_lines;

          (*  error recovery, go to next synchro point *)
          let (passed', rest') = 
            Parsing_recovery_cpp.find_next_synchro tr.PI.rest tr.PI.passed in
          tr.PI.rest <- rest';
          tr.PI.passed <- passed';

          tr.PI.current <- List.hd passed';

          (* <> line_error *)
          let info = TH.info_of_tok tr.PI.current in
          let checkpoint2 = PI.line_of_info info in 
          let checkpoint2_file = PI.file_of_info info in

          was_define := passed_a_define tr;
          (if !was_define && !Flag_cpp.filter_define_error
           then ()
           else 
              (* bugfix: *)
              (if (checkpoint_file = checkpoint2_file) && checkpoint_file = file
              then PI.print_bad line_error (checkpoint, checkpoint2) filelines
              else pr2 "PB: bad: but on tokens not from original file"
              )
          );

          let info_of_bads = 
            Common2.map_eff_rev TH.info_of_tok tr.PI.passed in 
          
          Some (Ast.NotParsedCorrectly info_of_bads)
      )
    in

    (* again not sure if checkpoint2 corresponds to end of bad region *)
    let info = TH.info_of_tok tr.PI.current in
    let checkpoint2 = PI.line_of_info info in
    let checkpoint2_file = PI.file_of_info info in

    let diffline = 
      if (checkpoint_file = checkpoint2_file) && (checkpoint_file = file)
      then (checkpoint2 - checkpoint) 
      else 0
        (* TODO? so if error come in middle of something ? where the
         * start token was from original file but synchro found in body
         * of macro ? then can have wrong number of lines stat.
         * Maybe simpler just to look at tr.passed and count
         * the lines in the token from the correct file ?
         *)
    in
    let info = List.rev tr.PI.passed in 

    (* some stat updates *)
    stat.Stat.commentized <- 
      stat.Stat.commentized + count_lines_commentized info;
    (match elem with
    | Some (Ast.NotParsedCorrectly _xs) -> 
        if !was_define && !Flag_cpp.filter_define_error
        then stat.Stat.commentized <- stat.Stat.commentized + diffline
        else stat.Stat.bad     <- stat.Stat.bad     + diffline

    | _ -> stat.Stat.correct <- stat.Stat.correct + diffline
    );

    (match elem with
    | None -> []
    | Some xs -> (xs, info):: loop () (* recurse *)
    )
  in
  let v = loop() in
  (v, stat)

let parse2 file =
  match File_type.file_type_of_file file with
  | FT.PL (FT.C _) ->
    (try 
      parse_with_lang ~lang:Flag_cpp.C file
    with _exn ->
      parse_with_lang ~lang:Flag_cpp.Cplusplus file
    )
  | FT.PL (FT.Cplusplus _) ->
    parse_with_lang ~lang:Flag_cpp.Cplusplus file
  | _ -> failwith (spf "not a C/C++ file: %s" file)
    

let parse file  = 
  Common.profile_code "Parse_cpp.parse" (fun () -> 
    try 
      parse2 file
    with Stack_overflow ->
      pr2 (spf "PB stack overflow in %s" file);
      [(Ast.NotParsedCorrectly [], ([]))], {Stat.
        correct = 0;
        bad = Common2.nblines_with_wc file;
        filename = file;
        have_timeout = true;
        commentized = 0;
        problematic_lines = [];
      }
  )

let parse_program file = 
  let (ast2, _stat) = parse file in
  program_of_program2 ast2

(*****************************************************************************)
(* Sub parsers *)
(*****************************************************************************)

(* for sgrep/spatch *)
let any_of_string lang s = 
  Common2.with_tmp_file ~str:s ~ext:"c" (fun file ->
  let toks_orig = tokens file in

  let toks = 
    try Parsing_hacks.fix_tokens ~macro_defs:_defs lang toks_orig
    with Token_views_cpp.UnclosedSymbol s ->
      pr2 s;
      if !Flag_cpp.debug_cplusplus 
      then raise (Token_views_cpp.UnclosedSymbol s)
      else toks_orig
  in

  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  (* -------------------------------------------------- *)
  (* Call parser *)
  (* -------------------------------------------------- *)
  Parser_cpp.sgrep_spatch_pattern (lexer_function tr) lexbuf_fake
  )

(* experimental *)
let parse_with_dypgen file =
  (* -------------------------------------------------- *)
  (* call lexer and get all the tokens *)
  (* -------------------------------------------------- *)
  let toks_orig = tokens file in
  let lang = Flag_parsing_cpp.Cplusplus in

  let toks = 
    try Parsing_hacks.fix_tokens ~macro_defs:_defs lang toks_orig
    with Token_views_cpp.UnclosedSymbol s ->
      pr2 s;
      if !Flag_cpp.debug_cplusplus 
      then raise (Token_views_cpp.UnclosedSymbol s)
      else toks_orig
  in

  let tr = Parse_info.mk_tokens_state toks in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in

  (* -------------------------------------------------- *)
  (* Call parser *)
  (* -------------------------------------------------- *)
  (* TODO: not sure why but calling main is significanctly faster
   * than calling toplevel in a loop
   *)
  try 
    Parser_cpp2.main (lexer_function tr) lexbuf_fake
    |> List.hd |> fst
  with Dyp.Syntax_error ->
     raise (Parse_info.Parsing_error (TH.info_of_tok tr.PI.current))
