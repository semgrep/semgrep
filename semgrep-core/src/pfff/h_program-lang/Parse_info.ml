(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2020 r2c
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

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Some helpers for the different lexers and parsers in pfff.
 *
 * The main types are:
 * ('token_location' < 'token_origin' < 'token_mutable') * token_kind
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type token_location = {
  str: string;
  charpos: int;
  line: int; column: int;
  file: string;
}
[@@deriving show { with_path = false}, eq ] (* with tarzan *)

let fake_token_location = {
  charpos = -1; str = ""; line = -1; column = -1; file = "FAKE TOKEN LOCATION";
}

let first_loc_of_file file = {
  charpos = 0; str = ""; line = 1; column = 0; file = file;
}

type token_origin =
  (* Present both in the AST and list of tokens *)
  | OriginTok  of token_location

  (* Present only in the AST and generated after parsing. Can be used
   * when building some extra AST elements. *)
  | FakeTokStr of string (* to help the generic pretty printer *) *
                  (* Sometimes we generate fake tokens close to existing
                   * origin tokens. This can be useful when have to give an error
                   * message that involves a fakeToken. The int is a kind of
                   * virtual position, an offset. See compare_pos below.
                  *)
                  (token_location * int) option

  (* In the case of a XHP file, we could preprocess it and incorporate
   * the tokens of the preprocessed code with the tokens from
   * the original file. We want to mark those "expanded" tokens
   * with a special tag so that if someone do some transformation on
   * those expanded tokens they will get a warning (because we may have
   * trouble back-propagating the transformation back to the original file).
  *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      token_location  *
      (* kind of virtual position. This info refers to the last token
       * before a serie of expanded tokens and the int is an offset.
       * The goal is to be able to compare the position of tokens
       * between then, even for expanded tokens. See compare_pos
       * below.
      *)
      token_location * int

  (* The Ab constructor is (ab)used to call '=' to compare
   * big AST portions. Indeed as we keep the token information in the AST,
   * if we have an expression in the code like "1+1" and want to test if
   * it's equal to another code like "1+1" located elsewhere, then
   * the Pervasives.'=' of OCaml will not return true because
   * when it recursively goes down to compare the leaf of the AST, that is
   * the token_location, there will be some differences of positions. If instead
   * all leaves use Ab, then there is no position information and we can
   * use '='. See also the 'al_info' function below.
   *
   * Ab means AbstractLineTok. I Use a short name to not
   * polluate in debug mode.
  *)
  | Ab
[@@deriving show { with_path = false}, eq ] (* with tarzan *)

type token_mutable = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
  *)
  token : token_origin;
  mutable transfo: transformation;
  (* less: mutable comments: ...; *)
}

(* poor's man refactoring *)
and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

and add =
  | AddStr of string
  | AddNewlineAndIdent

[@@deriving show { with_path = false}, eq ] (* with tarzan *)

exception NoTokenLocation of string

let mk_info_of_loc loc =
  { token = OriginTok loc; transfo = NoTransfo }

let token_location_of_info ii =
  match ii.token with
  | OriginTok pinfo -> Ok pinfo
  (* TODO ? dangerous ? *)
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> Ok pinfo_pp
  | FakeTokStr (_, (Some (pi, _))) -> Ok pi

  | FakeTokStr (_, None) -> Error "FakeTokStr"
  | Ab -> Error "Ab"

let unsafe_token_location_of_info ii =
  match token_location_of_info ii with
  | Ok pinfo -> pinfo
  | Error msg -> raise (NoTokenLocation msg)

(* Synthesize a token. *)
let unsafe_fake_info str : token_mutable =
  { token = FakeTokStr (str, None); transfo = NoTransfo; }

let fake_info_loc next_to_loc str : token_mutable =
  (* TODO: offset seems to have no use right now (?) *)
  { token = FakeTokStr (str, Some (next_to_loc, -1)); transfo = NoTransfo; }

let fake_info next_to_tok str : token_mutable =
  match token_location_of_info next_to_tok with
  | Ok loc -> fake_info_loc loc str
  | Error _ -> unsafe_fake_info str

let abstract_info =
  { token = Ab; transfo = NoTransfo }

let is_fake tok =
  match tok.token with
  | FakeTokStr _ -> true
  | _ -> false

(* used to be in AST_generic.ml *)
let unsafe_fake_bracket x = unsafe_fake_info "(", x, unsafe_fake_info ")"
let fake_bracket_loc next_to_loc x =
  fake_info_loc next_to_loc "(", x, fake_info_loc next_to_loc ")"
let fake_bracket next_to_tok x =
  fake_info next_to_tok "(", x, fake_info next_to_tok ")"

let unbracket (_, x, _) = x

let unsafe_sc = unsafe_fake_info ";"
let sc_loc next_to_loc = fake_info_loc next_to_loc ";"
let sc next_to_tok = fake_info next_to_tok ";"

type token_kind =
  (* for the fuzzy parser and sgrep/spatch fuzzy AST *)
  | LPar | RPar
  | LBrace | RBrace
  | LBracket | RBracket
  | LAngle | RAngle
  (* for the unparser helpers in spatch, and to filter
   * irrelevant tokens in the fuzzy parser
  *)
  | Esthet of esthet
  (* mostly for the lexer helpers, and for fuzzy parser *)
  (* less: want to factorize all those TH.is_eof to use that?
   * but extra cost? same for TH.is_comment?
   * todo: could maybe get rid of that now that we don't really use
   * berkeley DB and prefer Prolog, and so we don't need a sentinel
   * ast elements to associate the comments with it
  *)
  | Eof

  | Other

and esthet =
  | Comment
  | Newline
  | Space

(* shortcut *)
type t = token_mutable
[@@deriving eq]
type info_ = t


(* see -full_token_info in meta_parse_info.ml *)
let pp_full_token_info = ref false
(* for ppx_deriving *)
let pp fmt t =
  if !pp_full_token_info
  then pp_token_mutable fmt t
  else Format.fprintf fmt "()"

type parsing_stat = {
  filename: Common.filename;
  total_line_count: int;
  mutable error_line_count: int;
  mutable have_timeout: bool;
  (* used by our cpp commentizer *)
  mutable commentized: int;
  (* if want to know exactly what was passed through, uncomment:
   *
   * mutable passing_through_lines: int;
   *
   * it differs from bad by starting from the error to
   * the synchro point instead of starting from start of
   * function to end of function.
  *)

  (* for instance to report most problematic macros when parse c/c++ *)
  mutable problematic_lines:
    (string list (* ident in error line *) * int (* line_error *)) list;
}

let summary_of_stat (x : parsing_stat) =
  spf "%s lines=%i error_lines=%i timeout=%B"
    x.filename x.total_line_count x.error_line_count x.have_timeout

let default_stat file =
  let n = Common2.nblines_eff file in
  {
    filename = file;
    total_line_count = n;

    have_timeout = false;
    error_line_count = 0;
    commentized = 0;
    problematic_lines = [];
  }

let bad_stat file =
  let stat = default_stat file in
  stat.error_line_count <- stat.total_line_count;
  stat

let correct_stat file =
  default_stat file

(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The token_state type helps.
*)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
  (* if want to do some lalr(k) hacking ... cf yacfe.
   * mutable passed_clean : 'tok list;
   * mutable rest_clean :   'tok list;
  *)
}

let mk_tokens_state toks = {
  rest       = toks;
  current    = (List.hd toks);
  passed = [];
  (* passed_clean = [];
   * rest_clean = (toks +> List.filter TH.is_not_comment);
  *)
}

type ('ast, 'toks) parsing_result = {
  ast: 'ast;
  (* Note that the token list contains usually also the comment-tokens *)
  tokens: 'toks list;
  stat: parsing_stat
}

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)

let tokinfo_str_pos str pos =
  let loc =  {
    charpos = pos;
    str     = str;

    (* info filled in a post-lexing phase, see complete_token_location_large*)
    line = -1;
    column = -1;
    file = "NO FILE INFO YET";
  } in
  mk_info_of_loc loc

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

(*****************************************************************************)
(* Errors *)
(*****************************************************************************)
(* this can be used in the different lexer/parsers in pfff *)
(* coupling: see related error in Error_code and its exn_to_error *)
exception Lexical_error of string * t
exception Parsing_error of t
exception Ast_builder_error of string * t
exception Other_error of string * t

let tokinfo lexbuf  =
  tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf)

let lexical_error s lexbuf =
  let info = tokinfo lexbuf in
  if !Flag_parsing.exn_when_lexical_error
  then raise (Lexical_error (s, info))
  else
  if !Flag_parsing.verbose_lexing
  then pr2_once ("LEXER: " ^ s)
  else ()

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* for error reporting *)
let string_of_token_location x =
  spf "%s:%d:%d" x.file x.line x.column

let string_of_info x =
  match token_location_of_info x with
  | Ok loc -> string_of_token_location loc
  | Error msg -> spf "unknown location (%s)" msg

let str_of_info  ii =
  match ii.token  with
  | OriginTok x -> x.str
  | FakeTokStr (s, _) -> s
  | ExpandedTok _ | Ab ->
      raise (NoTokenLocation "str_of_info: Expanded or Ab")

let _str_of_info ii = (unsafe_token_location_of_info ii).str
let file_of_info ii = (unsafe_token_location_of_info ii).file
let line_of_info ii = (unsafe_token_location_of_info ii).line
let col_of_info  ii = (unsafe_token_location_of_info ii).column
(* todo: return a Real | Virt position ? *)
let pos_of_info  ii = (unsafe_token_location_of_info ii).charpos

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let pinfo_of_info ii = ii.token

let is_origintok ii =
  match ii.token with
  | OriginTok _ -> true
  | _ -> false


(* info about the current location *)

(* original info *)
let get_original_token_location = function
  | OriginTok pi -> pi
  | ExpandedTok (pi,_, _) -> pi
  | FakeTokStr (_,_) -> raise (NoTokenLocation "FakeTokStr")
  | Ab -> raise (NoTokenLocation "Ab")

(* used by token_helpers *)

(* not used but used to be useful in coccinelle *)
type posrv =
  | Real of token_location
  | Virt of
      token_location (* last real info before expanded tok *) *
      int (* virtual offset *)

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi -> Real pi
    (* todo? I have this for lang_php/
        | FakeTokStr (s, Some (pi_orig, offset)) ->
            Virt (pi_orig, offset)
    *)
    | FakeTokStr _ -> raise (NoTokenLocation "compare_pos: FakeTokStr")
    | Ab -> raise (NoTokenLocation "compare_pos: Ab")
    | ExpandedTok (_pi_pp, pi_orig, offset) ->
        Virt (pi_orig, offset)
  in
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1,pos2) with
  | (Real p1, Real p2) ->
      compare p1.charpos p2.charpos
  | (Virt (p1,_), Real p2) ->
      if (compare p1.charpos p2.charpos) =|= (-1)
      then (-1)
      else 1
  | (Real p1, Virt (p2,_)) ->
      if (compare p1.charpos p2.charpos) =|= 1
      then 1
      else (-1)
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.charpos in
      let poi2 = p2.charpos in
      match compare poi1 poi2 with
      |	-1 -> -1
      |	0 -> compare o1 o2
      |	1 -> 1
      | _ -> raise Impossible


let min_max_ii_by_pos xs =
  match xs with
  | [] -> raise (NoTokenLocation "Match returned an empty list with no token location information; this may be fixed by adding enclosing token information (e.g. bracket or parend tokens) to the list's enclosing node type.")
  | [x] -> (x, x)
  | x::xs ->
      let pos_leq p1 p2 = (compare_pos p1 p2) =|= (-1) in
      xs |> List.fold_left (fun (minii,maxii) e ->
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        minii', maxii'
      ) (x,x)

(*
I used to have:
 type program2 = toplevel2 list
  (* the token list contains also the comment-tokens *)
  and toplevel2 = Ast_php.toplevel * Parser_php.token list
type program_with_comments = program2

and a function below called distribute_info_items_toplevel that
would distribute the list of tokens to each toplevel entity.
This was when I was storing parts of AST in berkeley DB and when
I wanted to get some information about an entity (a function, a class)
I wanted to get the list also of tokens associated with that entity.

Now I just have
 type program_and_tokens = Ast_php.program * Parser_php.token list
because I don't use berkeley DB. I use codegraph and an entity_finder
we just focus on use/def and does not store huge asts on disk.


let rec distribute_info_items_toplevel2 xs toks filename =
  match xs with
  | [] -> raise Impossible
  | [Ast_php.FinalDef e] ->
      (* assert (null toks) ??? no cos can have whitespace tokens *)
      let info_item = toks in
      [Ast_php.FinalDef e, info_item]
  | ast::xs ->

      (match ast with
      | Ast_js.St (Ast_js.Nop None) ->
          distribute_info_items_toplevel2 xs toks filename
      | _ ->


      let ii = Lib_parsing_php.ii_of_any (Ast.Toplevel ast) in
      (* ugly: I use a fakeInfo for lambda f_name, so I have
       * have to filter the abstract info here
       *)
      let ii = List.filter PI.is_origintok ii in
      let (min, max) = PI.min_max_ii_by_pos ii in

      let toks_before_max, toks_after =
(* on very huge file, this function was previously segmentation fault
 * in native mode because span was not tail call
 *)
        Common.profile_code "spanning tokens" (fun () ->
        toks +> Common2.span_tail_call (fun tok ->
          match PI.compare_pos (TH.info_of_tok tok) max with
          | -1 | 0 -> true
          | 1 -> false
          | _ -> raise Impossible
        ))
      in
      let info_item = toks_before_max in
      (ast, info_item)::distribute_info_items_toplevel2 xs toks_after filename

let distribute_info_items_toplevel a b c =
  Common.profile_code "distribute_info_items" (fun () ->
    distribute_info_items_toplevel2 a b c
  )

*)

let rewrap_str s ii =
  {ii with token =
             (match ii.token with
              | OriginTok pi -> OriginTok { pi with str = s;}
              | FakeTokStr (s, info) -> FakeTokStr (s, info)
              | Ab -> Ab
              | ExpandedTok _ ->
                  (* ExpandedTok ({ pi with Common.str = s;},vpi) *)
                  failwith "rewrap_str: ExpandedTok not allowed here"
             )
  }

(* less: should use Buffer and not ^ so we should not need that *)
let tok_add_s s ii  =
  rewrap_str ((str_of_info ii) ^ s) ii

let str_of_info_fake_ok ii =
  match ii.token with
  | OriginTok pinfo -> pinfo.str
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> pinfo_pp.str
  | FakeTokStr (_, (Some (pi, _))) -> pi.str
  | FakeTokStr (s, None) -> s
  | Ab -> raise (NoTokenLocation "Ab")

let combine_infos x xs =
  let str = xs |> List.map str_of_info_fake_ok |> String.concat "" in
  tok_add_s str x

let split_info_at_pos pos ii =
  let loc = unsafe_token_location_of_info ii in
  let str = loc.str in
  let loc1_str =
    String.sub str 0 pos in
  let loc2_str =
    String.sub str pos (String.length str - pos) in
  let loc1 = { loc with str = loc1_str } in
  let loc2 = { loc with
               str = loc2_str;
               charpos = loc.charpos + pos;
               column = loc.column + pos;
             } in
  mk_info_of_loc loc1, mk_info_of_loc loc2

(*****************************************************************************)
(* Adjust file pos *)
(*****************************************************************************)

let full_charpos_to_pos_large2 = fun file ->

  let chan = open_in_bin file in
  let size = Common2.filesize file + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos   = ref 0 in
  let line  = ref 0 in

  let full_charpos_to_pos_aux () =
    try
      while true do begin
        let s = (input_line chan) in
        incr line;
        let len = String.length s in

        (* '... +1 do'  cos input_line does not return the trailing \n *)
        let col = ref 0 in
        for i = 0 to (len - 1) + 1 do

          (* old: arr.(!charpos + i) <- (!line, i); *)
          arr1.{!charpos + i} <- (!line);
          arr2.{!charpos + i} <- !col;
          (* ugly: hack for weird windows files containing a single
           * carriage return (\r) instead of a carriage return + newline
           * (\r\n) to delimit newlines. Not recognizing those single
           * \r as a newline marker prevents Javascript ASI to correctly
           * insert semicolons.
           * note: we could fix info_from_charpos() too, but it's not
           * used for ASI so simpler to leave it as is.
          *)
          if i < len - 1 && String.get s i = '\r'
          then begin incr line; col := -1 end;
          incr col
        done;
        charpos := !charpos + len + 1;
      end done
    with End_of_file ->
      for i = !charpos to (* old: Array.length arr *)
          Bigarray.Array1.dim arr1 - 1 do
        (* old: arr.(i) <- (!line, 0); *)
        arr1.{i} <- !line;
        arr2.{i} <- 0;
      done;
      ();
  in
  begin
    full_charpos_to_pos_aux ();
    close_in chan;
    (fun i -> arr1.{i}, arr2.{i})
  end

let full_charpos_to_pos_large a =
  profile_code "Common.full_charpos_to_pos_large"
    (fun () -> full_charpos_to_pos_large2 a)

(* Currently, lexing.ml, in the standard OCaml libray, does not handle
 * the line number position.
 * Even if there are certain fields in the lexing structure, they are not
 * maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence those types and functions below to overcome the previous limitation,
 * (see especially complete_token_location_large()).
 * alt:
 *   - in each lexer you need to take care of newlines and update manually
 *     the field.
*)
let complete_token_location_large filename table x =
  { x with
    file = filename;
    line   = fst (table (x.charpos));
    column = snd (table (x.charpos));
  }

(* Why is it better to first get all the tokens?
 * Why not lex on-demand as yacc requires more tokens?
 * There are a few reasons:
 *  - for parsing hacks, it's easier to work on the full list
 *  - for error recovery strategy, it's easier to work on the full list
 *  - we do not need to care about line/col in the lexer and do that here
 *  - we can have comments as tokens (useful for codemap/efuns) and
 *    skip them easily with one Common.exclude
*)
let tokenize_all_and_adjust_pos file tokenizer visitor_tok is_eof =
  Common.with_open_infile file (fun chan ->
    let lexbuf = Lexing.from_channel chan in
    let table = full_charpos_to_pos_large file in
    let adjust_info ii =
      { ii with token =
                  (* could assert pinfo.filename = file ? *)
                  match ii.token with
                  | OriginTok pi ->
                      OriginTok(complete_token_location_large file table pi)
                  | ExpandedTok (pi,vpi, off) ->
                      ExpandedTok(complete_token_location_large file table pi,vpi,  off)
                  | FakeTokStr (s,vpi_opt) ->
                      FakeTokStr (s,vpi_opt)
                  | Ab -> raise Impossible
      }
    in
    let rec tokens_aux acc =
      let tok =
        try
          tokenizer lexbuf
        with Lexical_error (s, info) ->
          raise (Lexical_error (s, adjust_info info))
      in
      if !Flag_parsing.debug_lexer
      then Common.pr2_gen tok;
      let tok = tok |> visitor_tok adjust_info in
      if is_eof tok
      then List.rev (tok::acc)
      else tokens_aux (tok::acc)
    in
    tokens_aux []
  )

(* Hacked lex. Ocamlyacc expects a function returning one token at a time
 * but we actually lex all the file so we need a wrapper to turn that
 * into a stream.
 * This function use refs passed by parse. 'tr' means 'token refs'.
 *
 * Why pass is_comment? Why not skip comments before?
 *  - for error recovery to still return comments for separate entities?
 *  - TODO?
*)
let mk_lexer_for_yacc toks is_comment =
  let tr = mk_tokens_state toks in
  let rec lexer =
    fun lexbuf ->
      match tr.rest with
      | [] -> (pr2 "LEXER: ALREADY AT END"; tr.current)
      | v::xs ->
          tr.rest <- xs;
          tr.current <- v;
          tr.passed <- v::tr.passed;
          if is_comment v
          then lexer lexbuf
          else v
  in
  let lexbuf_fake = Lexing.from_function (fun _buf _n -> raise Impossible) in
  tr, lexer, lexbuf_fake

let adjust_pinfo_wrt_base base_loc loc =
  (* Note that charpos and columns are 0-based, whereas lines are 1-based. *)
  { loc with
    charpos = base_loc.charpos + loc.charpos;
    line    = base_loc.line + loc.line - 1;
    column  =
      if loc.line = 1 then
        base_loc.column + loc.column
      else
        loc.column;
    file    = base_loc.file; }

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and charpos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and inspecting
   the contents of the token, plus the start information.
*)
let get_token_end_info loc =
  let line, col =
    Stdcompat.String.fold_left (fun (line, col) c ->
      match c with
      | '\n' -> (line + 1, 0)
      | _ -> (line, col + 1)
    ) (loc.line, loc.column) loc.str in
  line, col, loc.charpos + String.length loc.str

let fix_token_location fix ii =
  { ii with token =
              match ii.token with
              | OriginTok pi ->
                  OriginTok(fix pi)
              | ExpandedTok (pi, vpi, off) ->
                  ExpandedTok(fix pi, vpi, off)
              | FakeTokStr (s, vpi_opt) ->
                  FakeTokStr (s, vpi_opt)
              | Ab -> Ab
  }

let adjust_info_wrt_base base_loc ii =
  fix_token_location (adjust_pinfo_wrt_base base_loc) ii

(*****************************************************************************)
(* Error location report *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
*)
let (info_from_charpos2: int -> filename -> (int * int * string)) =
  fun charpos filename ->

  (* Currently lexing.ml does not handle the line number position.
   * Even if there is some fields in the lexing structure, they are not
   * maintained by the lexing engine :( So the following code does not work:
   *   let pos = Lexing.lexeme_end_p lexbuf in
   *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
   *      (pos.pos_cnum - pos.pos_bol) in
   * Hence this function to overcome the previous limitation.
  *)
  let chan = open_in_bin filename in
  let linen  = ref 0 in
  let posl   = ref 0 in
  let rec charpos_to_pos_aux last_valid =
    let s =
      try Some (input_line chan)
      with End_of_file when charpos =|= last_valid -> None in
    incr linen;
    match s with
      Some s ->
        let s = s ^ "\n" in
        if (!posl + String.length s > charpos)
        then begin
          close_in chan;
          (!linen, charpos - !posl, s)
        end
        else begin
          posl := !posl + String.length s;
          charpos_to_pos_aux !posl;
        end
    | None -> (!linen, charpos - !posl, "\n")
  in
  let res = charpos_to_pos_aux 0 in
  close_in chan;
  res

let info_from_charpos a b =
  profile_code "Common.info_from_charpos" (fun () -> info_from_charpos2 a b)


(* Decalage is here to handle stuff such as cpp which include file and who
 * can make shift.
*)
let (error_messagebis: filename -> (string * int) -> int -> string)=
  fun filename (lexeme, lexstart) decalage ->

  let charpos = lexstart      + decalage in
  let tok = lexeme in
  let (line, pos, linecontent) =  info_from_charpos charpos filename in
  let s = Common2.chop linecontent in
  let s =
    (* this happens in Javascript for minified files *)
    if String.length s > 200
    then (String.sub s 0 100)  ^ " (TOO LONG, SHORTEN!)..."
    else s
  in
  spf "File \"%s\", line %d, column %d,  charpos = %d
    around = '%s', whole content = %s"
    filename line pos charpos tok s


let error_message = fun filename (lexeme, lexstart) ->
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_token_location = fun info ->
  let filename = info.file in
  let lexeme = info.str in
  let lexstart = info.charpos in
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_info info =
  let pinfo = unsafe_token_location_of_info info in
  error_message_token_location pinfo


let print_bad line_error (start_line, end_line) filelines  =
  begin
    pr2 ("badcount: " ^ i_to_s (end_line - start_line));

    for i = start_line to end_line do
      let s = filelines.(i) in
      let line =
        (* this happens in Javascript for minified files *)
        if String.length s > 200
        then (String.sub s 0 100)  ^ " (TOO LONG, SHORTEN!)..."
        else s
      in

      if i =|= line_error
      then  pr2 ("BAD:!!!!!" ^ " " ^ line)
      else  pr2 ("bad:" ^ " " ^      line)
    done
  end

(*****************************************************************************)
(* Parsing statistics *)
(*****************************************************************************)

let aggregate_stats statxs =
  let total_lines =
    statxs |> List.fold_left (fun acc {total_line_count = x; _} -> acc+x) 0 in
  let bad  =
    statxs |> List.fold_left (fun acc {error_line_count = x; _} -> acc+x) 0 in
  total_lines, bad

(* todo: stat per dir ?  give in terms of func_or_decl numbers:
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura
 * aucune def  et donc aucune couverture en fait.
 * ==> TODO evaluer les parties non parsé ?
*)

let print_parsing_stat_list ?(verbose=false) statxs =
  let total = (List.length statxs) in
  let perfect =
    statxs
    |> List.filter (function
        {have_timeout = false; error_line_count = 0; _} -> true | _ -> false)
    |> List.length
  in

  if verbose then begin
    pr "\n\n\n---------------------------------------------------------------";
    pr "pbs with files:";
    statxs
    |> List.filter (function
      | {have_timeout = true; _} -> true
      | {error_line_count = n; _} when n > 0 -> true
      | _ -> false)
    |> List.iter (function
        {filename = file; have_timeout = timeout; error_line_count = n; _} ->
          pr (file ^ "  " ^ (if timeout then "TIMEOUT" else i_to_s n));
    );

    pr "\n\n\n";
    pr "files with lots of tokens passed/commentized:";
    let threshold_passed = 100 in
    statxs
    |> List.filter (function
      | {commentized = n; _} when n > threshold_passed -> true
      | _ -> false)
    |> List.iter (function
        {filename = file; commentized = n; _} ->
          pr (file ^ "  " ^ (i_to_s n));
    );

    pr "\n\n\n";
  end;

  let total_lines =
    statxs |> List.fold_left (fun acc {total_line_count = x; _} -> acc+x) 0 in
  let bad  =
    statxs |> List.fold_left (fun acc {error_line_count = x; _} -> acc+x) 0  in
  let passed =
    statxs |> List.fold_left (fun acc {commentized = x; _} -> acc+x) 0
  in
  let good = total_lines - bad in

  pr "---------------------------------------------------------------";
  pr (
    (spf "NB total files = %d; " total) ^
    (spf "NB total lines = %d; " total_lines) ^
    (spf "perfect = %d; " perfect) ^
    (spf "pbs = %d; "     (statxs |> List.filter (function
         {error_line_count = n; _} when n > 0 -> true | _ -> false)
                           |> List.length)) ^
    (spf "timeout = %d; " (statxs |> List.filter (function
         {have_timeout = true; _} -> true | _ -> false)
                           |> List.length)) ^
    (spf "=========> %d" ((100 * perfect) / total)) ^ "%"

  );
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  pr (
    (spf "nb good = %d,  nb passed = %d " good passed) ^
    (spf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
  );
  pr (
    (spf "nb good = %d,  nb bad = %d " good bad) ^
    (spf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
    )
  )


(*****************************************************************************)
(* Regression stats *)
(*****************************************************************************)

let print_regression_information ~ext xs newscore =
  let dirname_opt =
    match xs with
    | [x] when Common2.is_directory x -> Some (Common.fullpath x)
    | _ -> None
  in
  let score_path = Config_pfff.regression_data_dir in
  dirname_opt |> Option.iter (fun dirname ->
    pr2 "------------------------------";
    pr2 "regression testing information";
    pr2 "------------------------------";
    let str = Str.global_replace (Str.regexp "/") "__" dirname in
    let file = (Filename.concat score_path
                  ("score_parsing__" ^str ^ ext ^ ".marshalled")) in
    logger#debug "saving regression info in %s" file;
    Common2.regression_testing newscore file
  );
  ()

(*****************************************************************************)
(* Most problematic tokens *)
(*****************************************************************************)

(* inspired by a comment by a reviewer of my CC'09 paper *)
let lines_around_error_line ~context (file, line) =
  let arr = Common2.cat_array file in

  let startl = max 0 (line - context) in
  let endl   = min (Array.length arr) (line + context) in
  let res = ref [] in

  for i = startl to endl -1 do
    Common.push arr.(i) res
  done;
  List.rev !res

let print_recurring_problematic_tokens xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun x ->
    let file = x.filename in
    x.problematic_lines |> List.iter (fun (xs, line_error) ->
      xs |> List.iter (fun s ->
        Common2.hupdate_default s
          (fun (old, example)  -> old + 1, example)
          (fun() -> 0, (file, line_error)) h;
      )));
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  pr2 ("maybe 10 most problematic tokens");
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  Common.hash_to_list h
  |> List.sort (fun (_k1,(v1,_)) (_k2,(v2,_)) -> compare v2 v1)
  |> Common.take_safe 10
  |> List.iter (fun (k,(i, (file_ex, line_ex))) ->
    pr2 (spf "%s: present in %d parsing errors" k i);
    pr2 ("example: ");
    let lines = lines_around_error_line ~context:2 (file_ex, line_ex) in
    lines |> List.iter (fun s -> pr2 ("       " ^ s));
  );
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  ()

(****************************************************************************)
(* Exception printers for Printexc.to_string *)
(****************************************************************************)

let shorten_string s =
  if String.length s > 200 then
    String.sub s 0 200 ^ " ... (truncated)"
  else
    s

(*
   For error messages.
   - should be useful to a human reader
   - should not raise an exception
*)
let show_token_value (x : token_origin) : string =
  match x with
  | OriginTok loc -> spf "%S" (shorten_string loc.str)
  | FakeTokStr (fake, _opt_loc) -> spf "fake %S" (shorten_string fake)
  | ExpandedTok (first_loc, _, _) ->
      (* not sure about this *)
      spf "%S" (shorten_string first_loc.str)
  | Ab -> "abstract token"

let show_token_value_and_location (x : t) =
  let location = string_of_info x in
  let value = show_token_value x.token in
  spf "%s %s" location value

let string_of_exn e =
  let p = show_token_value_and_location in
  match e with
  | NoTokenLocation msg ->
      Some (spf "Parse_info.NoTokenLocation (%s)" msg)
  | Lexical_error (msg, tok) ->
      Some (spf "Parse_info.Lexical_error (%s, %s)" msg (p tok))
  | Parsing_error tok ->
      Some (spf "Parse_info.Parsing_error (%s)" (p tok))
  | Ast_builder_error (msg, tok) ->
      Some (spf "Parse_info.Ast_builder_error (%s, %s)" msg  (p tok))
  | Other_error (msg, tok) ->
      Some (spf "Parse_info.Other_error (%s, %s)" msg (p tok))
  | _ -> None

let register_exception_printer () = Printexc.register_printer string_of_exn
