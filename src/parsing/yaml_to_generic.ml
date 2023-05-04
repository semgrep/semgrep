(* Emma Jin
 *
 * Copyright (C) 2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module Y = Yaml
module S = Yaml.Stream
module E = Yaml.Stream.Event
module M = Yaml.Stream.Mark
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* YAML "AST" to AST_generic.
 *
 * The tree-sitter grammar for YAML is somewhat complicated,
 * since YAML is whitespace sensitive. Instead, we use the OCaml Yaml module,
 * which we also use to parse YAML for semgrep. To get positions, we use the
 * low-level Stream API, which we parse into our generic AST
 *
 * Before we parse a YAML file, we first need to replace ellipses with valid
 * YAML. All ellipses are replaced with
                 __sgrep_ellipses__ : __sgrep_ellipses__
 * This means - ... becomes - __sgrep_ellipses__ : __sgrep_ellipses__, which
 * is a little awkward, since this looks like a dictionary. However, we will
 * recognize this as a single ellipsis in parse
 *
 * It is unfortunately impossible to simply replace every ellipsis with that
 * because we need to add dashes to match other list items
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* right now we just pass down the filename in the environment, but
 * we could need to pass more information later.
 *)
type env = {
  (* when we parse a pattern, the filename is fake ("<pattern_file>") *)
  file : Common.filename;
  (* This is the literal text of the program.
     We will use this to help us find the proper content of a metavariable matching
     a "folded" or "literal" style string.
     Essentially, a string using "|" or ">".
  *)
  text : string;
  (* function *)
  charpos_to_pos : (int -> int * int) option;
  (* From yaml.mli: "[parser] tracks the state of generating {!Event.t}
   * values" *)
  parser : S.parser;
  anchors : (string, G.expr * E.pos) Hashtbl.t;
  mutable last_event : (E.t * E.pos) option;
  is_target : bool;
}

(*****************************************************************************)
(* Helper functions *)
(*****************************************************************************)

exception UnrecognizedAlias of Tok.t

let sgrep_ellipses_inline = "__sgrep_ellipses__"
let sgrep_ellipses = "__sgrep_ellipses__: __sgrep_ellipses__"

let p_token = function
  | E.Stream_start _ -> "stream start"
  | E.Stream_end -> "stream end"
  | E.Document_start _ -> "document start"
  | E.Document_end _ -> "document end"
  | E.Sequence_start _ -> "sequence start"
  | E.Sequence_end -> "sequence end"
  | E.Mapping_start _ -> "mapping start"
  | E.Mapping_end -> "mapping end"
  | E.Alias _ -> "alias"
  | E.Scalar { value; _ } -> "scalar " ^ value
  | E.Nothing -> "nothing"

let _pos_str
    {
      E.start_mark = { M.index = s_index; M.line = s_line; M.column = s_column };
      E.end_mark = { M.index = e_index; M.line = e_line; M.column = e_column };
    } =
  Printf.sprintf "index: %d line: %d col %d -> index: %d line: %d col %d"
    s_index (s_line + 1) s_column e_index (e_line + 1) e_column

let tok (index, line, column) str env =
  Tok.OriginTok
    { str; pos = { charpos = index; line = line + 1; column; file = env.file } }

let mk_tok ?(style = `Plain)
    {
      E.start_mark = { M.index; M.line; M.column };
      E.end_mark = { M.index = e_index; _ };
      _;
    } str env =
  let str =
    match style with
    (* This is for strings that use `|`, `>`, or quotes.
     *)
    | `Literal
    | `Folded
    | `Double_quoted
    | `Single_quoted ->
        String.sub env.text index (e_index - index)
    | __else__ -> str
  in
  (* their tokens are 0 indexed for line and column, AST_generic's are 1
   * indexed for line, 0 for column *)
  tok (index, line, column) str env

let mk_bracket
    ( {
        E.start_mark =
          { M.index = s_index; M.line = s_line; M.column = s_column };
        _;
      },
      {
        E.end_mark = { M.index = e_index; M.line = e_line; M.column = e_column };
        _;
      } ) v env =
  (* The end index needs to be adjusted by one because the token is off *)
  let e_index' = e_index - 1 in
  let e_line, e_column =
    match env.charpos_to_pos with
    | None -> (e_line, e_column)
    | Some f ->
        let e_line, e_column = f e_index' in
        (* e_line - 1 because tok expects e_line 0-indexed *)
        (e_line - 1, e_column)
  in
  ( tok (s_index, s_line, s_column) "(" env,
    v,
    tok (e_index', e_line, e_column) ")" env )

let mk_id str pos env = G.Id ((str, mk_tok pos "" env), G.empty_id_info ())
let fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

exception ImpossibleDots
exception UnreachableList

let error str v pos env =
  let t = mk_tok pos (p_token v) env in
  raise (Parsing_error.Other_error (str, t))

let get_res file = function
  | Result.Error (`Msg str) ->
      let loc = Tok.first_loc_of_file file in
      let tok = Tok.tok_of_loc loc in
      raise (Parsing_error.Other_error (str, tok))
  | Result.Ok v -> v

let do_parse env =
  match S.do_parse env.parser with
  (* like get_res, but with better error location *)
  | Result.Error (`Msg str) ->
      let prefix, tok =
        match env.last_event with
        | None ->
            let loc = Tok.first_loc_of_file env.file in
            ("(incorrect error location) ", Tok.tok_of_loc loc)
        | Some (v, pos) ->
            ( "(approximate error location; error nearby after) ",
              mk_tok pos (p_token v) env )
      in
      raise (Parsing_error.Other_error (prefix ^ str, tok))
  | Result.Ok (ev, pos) ->
      env.last_event <- Some (ev, pos);
      (ev, pos)

(*****************************************************************************)
(* Parser functions *)
(*****************************************************************************)

let make_node f anchor args env =
  let node_expr = f args env in
  (match anchor with
  | Some anchor -> Hashtbl.add env.anchors anchor node_expr
  | None -> ());
  node_expr

let make_alias anchor pos env =
  let t = mk_tok pos anchor env in
  match Hashtbl.find_opt env.anchors anchor with
  | Some (expr, _p) -> (G.e (G.Alias ((anchor, t), expr)), pos)
  | None -> raise (UnrecognizedAlias t)

(* Scalars must first be checked for sgrep patterns *)
(* Then, they may need to be converted from a string to a value *)
let scalar (_tag, pos, value, style) env : G.expr * E.pos =
  (* If it's a target, then we don't want to parse it like a metavariable,
     or else matching will mess up when it attempts to match a pattern
     metavariable to target YAML code which looks like a metavariable
     (but ought to be interpreted as a string)
  *)
  let quoted =
    match style with
    | `Double_quoted
    | `Single_quoted ->
        true
    | __else__ -> false
  in
  if AST_generic.is_metavar_name value && (not env.is_target) && not quoted then
    (G.N (mk_id value pos env) |> G.e, pos)
  else
    let token = mk_tok ~style pos value env in
    let expr =
      (match value with
      | "__sgrep_ellipses__" -> G.Ellipsis (Tok.fake_tok token "...")
      (* TODO: emma: I will put "" back to Null and have either a warning or
       * an error when we try to parse a string and get Null in another PR.
       *)
      | "null"
      | "NULL"
      | "Null"
      | "~" ->
          G.L (G.Null token)
      | "y"
      | "Y"
      | "yes"
      | "Yes"
      | "YES"
      | "true"
      | "True"
      | "TRUE"
      | "on"
      | "On"
      | "ON" ->
          G.L (G.Bool (true, token))
      | "n"
      | "N"
      | "no"
      | "No"
      | "NO"
      | "false"
      | "False"
      | "FALSE"
      | "off"
      | "Off"
      | "OFF" ->
          G.L (G.Bool (false, token))
      | "-.inf" -> G.L (G.Float (Some neg_infinity, token))
      | ".inf" -> G.L (G.Float (Some neg_infinity, token))
      | ".nan"
      | ".NaN"
      | ".NAN" ->
          G.L (G.Float (Some nan, token))
      | _ -> (
          try G.L (G.Float (Some (float_of_string value), token)) with
          | _ -> G.L (G.String (fb (value, token)))))
      |> G.e
    in
    (expr, pos)

(* Sequences are arrays in the generic AST *)
let sequence (_tag, start_pos, (es, end_pos)) env =
  (G.Container (G.Array, mk_bracket (start_pos, end_pos) es env) |> G.e, end_pos)

(* Mappings are dictionaries in the generic AST *)
let mappings (_tag, start_pos, (es, end_pos)) env =
  match es with
  | [ { G.e = G.Ellipsis e; _ } ] -> (G.Ellipsis e |> G.e, end_pos)
  | _ ->
      ( G.Container (G.Dict, mk_bracket (start_pos, end_pos) es env) |> G.e,
        end_pos )

let make_mapping (pos1, pos2) ((key, value) : G.expr * G.expr) env =
  match (key.G.e, value.G.e) with
  | G.Ellipsis _, G.Ellipsis _ ->
      let tok = mk_tok pos1 "..." env in
      (G.Ellipsis (Tok.fake_tok tok "...") |> G.e, pos2)
  (* less: use G.keyval? *)
  | _ ->
      ( G.Container (G.Tuple, mk_bracket (pos1, pos2) [ key; value ] env) |> G.e,
        pos2 )

let make_doc start_pos (doc, end_pos) env : G.expr list =
  match doc with
  | [] -> []
  | [ x ] -> [ x ]
  | xs ->
      [ G.Container (G.Array, mk_bracket (start_pos, end_pos) xs env) |> G.e ]

let parse (env : env) : G.expr list =
  (* Parse states *)
  let rec read_stream () : G.expr list =
    match do_parse env with
    | E.Stream_start _, pos -> make_doc pos (read_documents []) env
    | v, pos -> error "Expected start of string, got" v pos env
  and read_documents acc : G.expr list * E.pos =
    match do_parse env with
    | E.Document_start _, _pos ->
        let docs, _ = read_document () in
        let rest, end_pos = read_documents acc in
        (docs :: rest, end_pos)
    | E.Stream_end, pos -> (acc, pos)
    | v, pos ->
        error "Expected start of document or end of stream, got" v pos env
  and read_document () : G.expr * E.pos =
    let node_ast, _ = read_node () in
    match do_parse env with
    | E.Document_end _, pos -> (node_ast, pos)
    | v, pos -> error "Expected end of document, got" v pos env
  and read_node ?node_val:(res = None) () : G.expr * E.pos =
    let res =
      match res with
      | None -> do_parse env
      | Some r -> r
    in
    match res with
    | E.Alias { anchor }, pos -> (
        try make_alias anchor pos env with
        | UnrecognizedAlias _ ->
            (error "Unrecognized alias" (E.Alias { anchor }) pos env, pos))
    | E.Scalar { anchor; tag; value; style; _ }, pos ->
        make_node scalar anchor (tag, pos, value, style) env
    | E.Sequence_start { anchor; tag; _ }, pos ->
        make_node sequence anchor (tag, pos, read_sequence []) env
    | E.Mapping_start { anchor; tag; _ }, pos ->
        make_node mappings anchor (tag, pos, read_mappings []) env
    | v, pos ->
        error "Expected a valid YAML element or end of sequence, got" v pos env
  and read_sequence acc : G.expr list * E.pos =
    match do_parse env with
    | E.Sequence_end, pos -> (acc, pos)
    | v, pos ->
        let seq = fst (read_node ~node_val:(Some (v, pos)) ()) in
        let rest, end_pos = read_sequence acc in
        (seq :: rest, end_pos)
  and read_mappings acc : G.expr list * E.pos =
    (* Capture the mutable `last_event` field prior to parsing, as the *)
    (* `Mapping_end` event needs to make use of it. *)
    let last_ev = env.last_event in
    match do_parse env with
    | E.Mapping_end, pos ->
        (* The `Mapping_end` event position is inaccurate: it specifies the *)
        (* position immediately prior to the next token. This causes comments *)
        (* and the leading whitespace of the next yaml element to be chomped on *)
        (* autofix. *)
        let pos' = Option.fold ~none:pos ~some:(fun (_, pos) -> pos) last_ev in
        (acc, pos')
    | v, pos ->
        let map = read_mapping (v, pos) in
        let rest, end_pos = read_mappings acc in
        (map :: rest, end_pos)
  and read_mapping first_node : G.expr =
    let key, pos1 =
      match first_node with
      | E.Scalar { anchor; tag; value; style; _ }, pos ->
          make_node scalar anchor (tag, pos, value, style) env
      | E.Mapping_start _, start_pos ->
          let _mappings, end_pos = read_mappings [] in
          ( G.OtherExpr
              ( ("Mapping", mk_tok start_pos "" env),
                [ G.Str (fb ("", mk_tok end_pos "" env)) ] )
            |> G.e,
            end_pos )
      | v, pos -> error "Expected a valid scalar, got" v pos env
    in
    let value, pos2 = read_node () in
    let value, _ = make_mapping (pos1, pos2) (key, value) env in
    value
  in
  read_stream ()

let make_pattern_expr e =
  match e with
  (* If the user creates a pattern with a single field, assume they just want *
   * to match the field, not the whole enclosing container *)
  | [ { G.e = G.Container (G.Dict, (_lp, [ x ], _rp)); _ } ] -> G.E x
  | [ { G.e = G.Container (G.Array, (_lp, [ x ], _rp)); _ } ] -> G.E x
  | [ x ] -> G.E x
  | _ -> raise UnreachableList

(*****************************************************************************)
(* Preprocess the file to replace ellipses with a standin value *)
(*****************************************************************************)

let substring str first last = String.sub str first (last - first)

(* Match ellipses to previous line with same indentation *)
let last_same_whitespace whitespace context =
  let target_len = String.length whitespace in
  let rec find_last_whitespace context =
    match context with
    | [] -> None
    | (ws_len, ws) :: xs ->
        if ws_len = target_len then Some ws else find_last_whitespace xs
  in
  find_last_whitespace context

(* Used for '- ...' when the '...' is being replaced by     *
 * '- __sgrep_ellipses__' and we want to keep both dashes.  *
 * We expect that |e_sp| <= |w_sp| because we match ' ...'  *
 * with ' - foo', but in that case would take ' ' as e_sp   *
   ' and ' - ' as w_sp *)
let union_whitespace e_sp w_sp =
  let esp_len = String.length e_sp in
  let check_esp i c =
    if i < esp_len then
      match e_sp.[i] with
      | ' ' -> c
      | c1 -> c1
    else c
  in
  String.mapi check_esp w_sp

(* Ellipses will not be immediately replaced in the case of  *
   * ...                                                      *
   * - foo                                                    *
   * so when '- foo' has been read we check for whether there *
   * is an ellipsis that can now be resolved *)
let convert_leftover_ellipses (prev_space_len, prev_space) ~is_line ellipses =
  let rec convert_ellipses ellipses =
    match ellipses with
    | [] -> ([], ellipses)
    | (ellipses_space_len, ellipses_space) :: rest ->
        let converted_ellipses, _ = convert_ellipses rest in
        (* Don't convert when you see *
         *    ...                     *
         *    ... *)
        if is_line && ellipses_space_len = prev_space_len then
          ( (union_whitespace ellipses_space prev_space ^ sgrep_ellipses)
            :: converted_ellipses,
            rest )
          (* Do convert when you see *
           *    ...                     *
           *  ... *)
        else if ellipses_space_len > prev_space_len then
          ((ellipses_space ^ sgrep_ellipses) :: converted_ellipses, rest)
        else ([], ellipses)
  in
  convert_ellipses ellipses

(* Given "  - foo", return 2, "  - ", so that if you have    *
 *   ...                                                     *
 *   - foo                                                   *
 * This converts to                                          *
 *   - __sgrep_ellipses__                                    *
 *   - foo                                                   *
 * recognizing the ... as aligned with - foo but using its - *)
let split_whitespace line =
  let line_len = String.length line in
  let rec read_string i =
    if i = line_len then (line, "")
    else
      match line.[i] with
      | ' '
      | '\t'
      | '-' ->
          read_string (i + 1)
      | _ -> (substring line 0 i, substring line i line_len)
  in
  let whitespace, line = read_string 0 in
  if String.contains whitespace '-' then
    (String.index whitespace '-', whitespace, line)
  else (String.length whitespace, whitespace, line)

(* Split a line by the ellipses *)
let split_on_ellipses line =
  let rec split line i ellipses_start num_dots =
    let line_length = String.length line in
    if i = line_length then [ line ]
    else
      match line.[i] with
      | '.' -> (
          match num_dots with
          | 0 -> split line (i + 1) i 1
          | 1 -> split line (i + 1) ellipses_start 2
          | 2 ->
              substring line 0 ellipses_start
              :: split (substring line (i + 1) line_length) 0 0 0
          | _ -> raise ImpossibleDots)
      | _ -> split line (i + 1) 0 0
  in
  let pieces = split line 0 0 0 in
  pieces

(* Pop child lines when the pattern exits *)
let rec exit_context whitespace_len context =
  match context with
  | [] -> []
  | (prev_len, _) :: rest ->
      if prev_len > whitespace_len then exit_context whitespace_len rest
      else context

(***** Preprocess the yaml *****)

let preprocess_yaml str =
  let lines = String.split_on_char '\n' str in
  (* Main processing function *)
  let rec process_lines lines context ellipses =
    match lines with
    | [] -> []
    | line :: rest ->
        let hyphen_loc, ws, str = split_whitespace line in
        let context', line', ellipses' =
          match String.trim str with
          | "..." -> (
              let ws_len = String.length ws in
              let conv, ellipses' =
                convert_leftover_ellipses (ws_len, ws) ~is_line:false ellipses
              in
              match last_same_whitespace ws context with
              | Some ws -> (context, conv @ [ ws ^ sgrep_ellipses ], ellipses')
              | None -> (context, conv, (ws_len, ws) :: ellipses'))
          | _ ->
              let conv, ellipses' =
                convert_leftover_ellipses (hyphen_loc, ws) ~is_line:true
                  ellipses
              in
              ( (hyphen_loc, ws) :: exit_context hyphen_loc context,
                conv
                @ [
                    String.concat sgrep_ellipses_inline (split_on_ellipses line);
                  ],
                ellipses' )
        in
        line' @ process_lines rest context' ellipses'
  in
  String.concat "\n" (process_lines lines [] [])

let mask_unicode str =
  (* The YAML parser returns the charpos as the number of unicode (not 8-bit)
     characters. However, Parse_info.full_charpos_to_pos expects the charpos
     to be the number of 8-bit characters. This difference causes incorrect
     line/col to be assigned to the end tokens of mappings after unicode
     characters. *)
  (* Note that the YAML parser does return a correct line and col, which we
     use everywhere else. However, it gives an exclusive end when returning
     the token that ends a mapping/sequence/other bracket, whereas Semgrep
     expects an inclusive end. To adjust this, we currently need the charpos *)
  let char_range = 128 in
  let control_char_start_range = 32 in
  let control_char_end_range = 1 in
  let available_range =
    char_range - (control_char_end_range + control_char_start_range)
  in
  String.of_seq
    (Seq.map
       (fun c ->
         let code = Char.code c in
         if code < char_range then c
         else Char.chr ((code mod available_range) + control_char_start_range))
       (String.to_seq str))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse_yaml_file ~is_target file str =
  (* we do not preprocess the yaml here; ellipsis should be transformed
   * only in the pattern *)
  let charpos_to_pos = Some (Pos.full_charpos_to_pos_large file) in
  let parser = get_res file (S.parser str) in
  let env =
    {
      file;
      text = str;
      charpos_to_pos;
      parser;
      anchors = Hashtbl.create 1;
      last_event = None;
      is_target;
    }
  in
  let xs = parse env in
  Common.map G.exprstmt xs

(* The entry points for yaml-language parsing *)

let any str =
  let file = "<pattern_file>" in
  let str = preprocess_yaml (mask_unicode str) in
  let parser = get_res file (S.parser str) in
  let env =
    {
      file;
      text = str;
      charpos_to_pos = None;
      parser;
      anchors = Hashtbl.create 1;
      last_event = None;
      is_target = false;
    }
  in
  let xs = parse env in
  make_pattern_expr xs

let program file =
  let str = mask_unicode (Common.read_file file) in
  parse_yaml_file ~is_target:true file str
