(* Emma Jin
 *
 * Copyright (C) 2021 r2c
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

module Y = Yaml
module S = Yaml.Stream
module E = Yaml.Stream.Event
module M = Yaml.Stream.Mark
module A = AST_generic

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

exception ParseError of string

exception ImpossibleDots

exception UnreachableList

(* right now we just pass down the filename in the environment, but
 * we could need to pass more information later.
 *)
type env = {
  (* when we parse a pattern, the filename is fake ("<pattern_file>") *)
  file : Common.filename;
  (* function *)
  charpos_to_pos : (int -> int * int) option;
}

(*****************************************************************************)
(* Helper functions *)
(*****************************************************************************)

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

let get_res = function
  | Result.Error (`Msg s) -> raise (ParseError s)
  | Result.Ok v -> v

let _pos_str
    {
      E.start_mark = { M.index = s_index; M.line = s_line; M.column = s_column };
      E.end_mark = { M.index = e_index; M.line = e_line; M.column = e_column };
    } =
  Printf.sprintf "index: %d line: %d col %d -> index: %d line: %d col %d"
    s_index (s_line + 1) s_column e_index (e_line + 1) e_column

let tok (index, line, column) str env =
  {
    Parse_info.token =
      Parse_info.OriginTok
        { str; charpos = index; line = line + 1; column; file = env.file };
    Parse_info.transfo = NoTransfo;
  }

let mk_tok { E.start_mark = { M.index; M.line; M.column }; _ } str env =
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
  let e_line, e_column =
    match env.charpos_to_pos with
    | None -> (e_line, e_column)
    | Some f ->
        let e_line, e_column = f (e_index - 1) in
        (e_line - 1, e_column)
  in
  ( tok (s_index, s_line, s_column) "(" env,
    v,
    tok (e_index, e_line, e_column) ")" env )

let mk_err err v ({ E.start_mark = { M.line; M.column; _ }; _ } as pos) env =
  A.error
    (mk_tok pos (p_token v) env)
    (Printf.sprintf "%s %s at line %d column %d" err (p_token v) line column)

let mk_id str pos env = A.Id ((str, mk_tok pos "" env), A.empty_id_info ())

(*****************************************************************************)
(* Parser functions *)
(*****************************************************************************)

let make_alias anchor pos env : A.name = mk_id anchor pos env

(* Scalars must first be checked for sgrep patterns *)
(* Then, they may need to be converted from a string to a value *)
let make_scalar _anchor _tag pos value env : A.expr =
  if AST_generic_.is_metavar_name value then A.N (mk_id value pos env)
  else
    let token = mk_tok pos value env in
    match value with
    | "__sgrep_ellipses__" -> A.Ellipsis (Parse_info.fake_info "...")
    | "null" | "NULL" | "" | "Null" | "~" -> A.L (A.Null token)
    | "y" | "Y" | "yes" | "Yes" | "YES" | "true" | "True" | "TRUE" | "on" | "On"
    | "ON" ->
        A.L (A.Bool (true, token))
    | "n" | "N" | "no" | "No" | "NO" | "false" | "False" | "FALSE" | "off"
    | "Off" | "OFF" ->
        A.L (A.Bool (false, token))
    | "-.inf" -> A.L (A.Float (Some neg_infinity, token))
    | ".inf" -> A.L (A.Float (Some neg_infinity, token))
    | ".nan" | ".NaN" | ".NAN" -> A.L (A.Float (Some nan, token))
    | _ -> (
        try A.L (A.Float (Some (float_of_string value), token))
        with _ -> A.L (A.String (value, token)) )

(* Sequences are arrays in the generic AST *)
let make_sequence _anchor _tag start_pos (es, end_pos) env =
  (A.Container (A.Array, mk_bracket (start_pos, end_pos) es env), end_pos)

(* Mappings are dictionaries in the generic AST *)
let make_mappings _anchor _tag start_pos (es, end_pos) env =
  match es with
  | [ A.Ellipsis e ] -> (A.Ellipsis e, end_pos)
  | _ -> (A.Container (A.Dict, mk_bracket (start_pos, end_pos) es env), end_pos)

let make_mapping (pos1, pos2) ((key, value) : A.expr * A.expr) env =
  match (key, value) with
  | A.Ellipsis _, A.Ellipsis _ -> (A.Ellipsis (Parse_info.fake_info "..."), pos2)
  | _ -> (A.Tuple (mk_bracket (pos1, pos2) [ key; value ] env), pos2)

let make_doc start_pos (doc, end_pos) env : A.expr list =
  match doc with
  | [] -> []
  | [ x ] -> [ x ]
  | xs -> [ A.Container (A.Array, mk_bracket (start_pos, end_pos) xs env) ]

let parse env parser : A.expr list =
  (* Parse states *)
  let rec read_stream () : A.expr list =
    match get_res (S.do_parse parser) with
    | E.Stream_start _, pos -> make_doc pos (read_documents []) env
    | v, pos -> mk_err "Expected start of string, got" v pos env
  and read_documents acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Document_start _, _pos ->
        let docs, _ = read_document () in
        let rest, end_pos = read_documents acc in
        (docs :: rest, end_pos)
    | E.Stream_end, pos -> (acc, pos)
    | v, pos ->
        mk_err "Expected start of document or end of stream, got" v pos env
  and read_document () : A.expr * E.pos =
    let node_ast, _ = read_node () in
    match get_res (S.do_parse parser) with
    | E.Document_end _, pos -> (node_ast, pos)
    | v, pos -> mk_err "Expected end of document, got" v pos env
  and read_node ?node_val:(res = None) () : A.expr * E.pos =
    let res =
      match res with None -> get_res (S.do_parse parser) | Some r -> r
    in
    match res with
    | E.Alias { anchor }, pos -> (A.N (make_alias anchor pos env), pos)
    | E.Scalar { anchor; tag; value; _ }, pos ->
        (make_scalar anchor tag pos value env, pos)
    | E.Sequence_start { anchor; tag; _ }, pos ->
        make_sequence anchor tag pos (read_sequence []) env
    | E.Mapping_start { anchor; tag; _ }, pos ->
        make_mappings anchor tag pos (read_mappings []) env
    | v, pos ->
        mk_err "Expected a valid YAML element or end of sequence, got" v pos env
  and read_sequence acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Sequence_end, pos -> (acc, pos)
    | v, pos ->
        let seq = fst (read_node ~node_val:(Some (v, pos)) ()) in
        let rest, end_pos = read_sequence acc in
        (seq :: rest, end_pos)
  and read_mappings acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Mapping_end, pos -> (acc, pos)
    | v, pos ->
        let map = read_mapping (v, pos) in
        let rest, end_pos = read_mappings acc in
        (map :: rest, end_pos)
  and read_mapping first_node : A.expr =
    let key, pos1 =
      match first_node with
      | E.Scalar { anchor; tag; value; _ }, pos ->
          (make_scalar anchor tag pos value env, pos)
      | v, pos -> mk_err "Expected a valid scalar, got" v pos env
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
  | [ A.Container (A.Dict, (_lp, [ x ], _rp)) ] -> A.E x
  | [ A.Container (A.Array, (_lp, [ x ], _rp)) ] -> A.E x
  | [ x ] -> A.E x
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
    if i < esp_len then match e_sp.[i] with ' ' -> c | c1 -> c1 else c
  in
  String.mapi check_esp w_sp

(* Ellipses will not be immediately replaced in the case of  *
  * ...                                                      *
  * - foo                                                    *
  * so when '- foo' has been read we check for whether there *
  * is an ellipsis that can now be resolved                  *)
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
      | ' ' | '\t' | '-' -> read_string (i + 1)
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
          | _ -> raise ImpossibleDots )
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
              | None -> (context, conv, (ws_len, ws) :: ellipses') )
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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let program file =
  (* we do not preprocess the yaml here; ellipsis should be transformed
   * only in the pattern *)
  let str = Common.read_file file in
  let charpos_to_pos = Some (Parse_info.full_charpos_to_pos_large file) in
  let env = { file; charpos_to_pos } in
  List.map A.exprstmt (get_res (S.parser str) |> parse env)

let any str =
  let env = { file = "<pattern_file>"; charpos_to_pos = None } in
  let str = preprocess_yaml str in
  (* Common.pr2 str; *)
  get_res (S.parser str) |> parse env |> make_pattern_expr
