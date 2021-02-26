module Y = Yaml
module S = Yaml.Stream
module E = Yaml.Stream.Event
module M = Yaml.Stream.Mark
module A = AST_generic

exception ParseError of string

(* Parser for YAML. The tree-sitter grammar for YAML is somewhat complicated,
 * since YAML is whitespace sensitive. Instead, we use the OCaml Yaml module,
 * which we also use to parse YAML for semgrep. To get positions, we use the
 * low-level Stream API, which we parse into a generic AST
*)

type env = {
  str: string;
  file: Common.filename
}

(* Helper functions *)

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

let tok (index, line, column) str file =
  {Parse_info.token = Parse_info.OriginTok {str; charpos = index; line = line + 1; column; file};
   Parse_info.transfo = NoTransfo}

let mk_tok {E.start_mark = {M.index; M.line; M.column}; _} str file =
  (* their tokens are 0 indexed for line and column, AST_generic's are 1 indexed for line, 0 for column *)
  tok (index, line, column) str file

let mk_bracket ({E.start_mark = {M.index = s_index; M.line = s_line; M.column = s_column}; _},
                {E.end_mark = {M.index = e_index; M.line = e_line; M.column = e_column}; _}) v file =
  tok (s_index, s_line, s_column) "(" file, v, tok (e_index, e_line, e_column) ")" file

let mk_err err v ({E.start_mark = {M.line; M.column; _}; _} as pos) file =
  A.error (mk_tok pos (p_token v) file) (Printf.sprintf "%s %s at line %d column %d" err (p_token v) line column)

let mk_id str pos file =
  A.Id ((str, (mk_tok pos "" file)), A.empty_id_info ())

(* Parser functions *)

let make_alias anchor pos file : A.name =
  mk_id anchor pos file

(* Scalars must first be checked for sgrep patterns *)
(* Then, they may need to be converted from a string to a value *)
let make_scalar _anchor _tag pos value file : A.expr =
  if AST_generic_.is_metavar_name value then A.N (mk_id value pos file) else
    let token = mk_tok pos value file in
    match value with
    | "__sgrep_ellipses__" -> A.Ellipsis (Parse_info.fake_info "...")
    | "null" | "NULL" | "" | "Null" | "~" -> A.L (A.Null token)
    | "y"|"Y"|"yes"|"Yes"|"YES"
    | "true"|"True"|"TRUE"
    | "on"|"On"|"ON" -> A.L (A.Bool (true, token))
    | "n"|"N"|"no"|"No"|"NO"
    | "false"|"False"|"FALSE"
    | "off"|"Off"|"OFF" -> A.L (A.Bool (false, token))
    | "-.inf" -> A.L (A.Float (Some neg_infinity, token))
    | ".inf" -> A.L (A.Float (Some neg_infinity, token))
    | ".nan"|".NaN"|".NAN" -> A.L (A.Float (Some nan, token))
    | _ ->
        try A.L (A.Float (Some (float_of_string value), token))
        with _ -> A.L (A.String (value, token))

(* Sequences are arrays in the generic AST *)
let make_sequence _anchor _tag start_pos (es, end_pos) file =
  A.Container(A.Array, mk_bracket (start_pos, end_pos) es file)

(* Mappings are dictionaries in the generic AST *)
let make_mappings _anchor _tag start_pos (es, end_pos) file =
  match es with
  | [A.Ellipsis e] -> A.Ellipsis e
  | _ -> A.Container(A.Dict, mk_bracket (start_pos, end_pos) es file)

let make_mapping (pos1, pos2) ((key, value) : A.expr * A.expr) file =
  match key, value with
  | A.Ellipsis _, A.Ellipsis _ -> A.Ellipsis (Parse_info.fake_info "...")
  | _ -> A.Tuple (mk_bracket (pos1, pos2) [key; value] file)

let make_doc start_pos (doc, end_pos) file =
  match doc with
  | [x] -> x
  | xs -> A.Container(A.Array, mk_bracket (start_pos, end_pos) xs file)

let parse file parser : A.expr =
  (* Parse states *)
  let rec read_stream () : A.expr =
    match get_res (S.do_parse parser) with
    | E.Stream_start _, pos -> make_doc pos (read_documents []) file
    | v, pos -> mk_err "Expected start of string, got" v pos file
  and read_documents acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Document_start _, pos -> let docs = read_document () in (fst docs) :: (fst (read_documents acc)), pos
    | E.Stream_end, pos -> acc, pos
    | v, pos -> mk_err "Expected start of document or end of stream, got" v pos file
  and read_document () : A.expr * E.pos =
    let node_ast, _ = read_node () in
    match get_res (S.do_parse parser) with
    | E.Document_end _, pos -> node_ast, pos
    | v, pos -> mk_err "Expected end of document, got" v pos file
  and read_node ?node_val:(res = None) () : A.expr * E.pos =
    let res = match res with None -> get_res (S.do_parse parser) | Some r -> r in
    match res with
    | E.Alias { anchor }, pos -> A.N (make_alias anchor pos file), pos
    | E.Scalar { anchor; tag; value; _ }, pos -> make_scalar anchor tag pos value file, pos
    | E.Sequence_start { anchor; tag; _ }, pos -> make_sequence anchor tag pos (read_sequence []) file, pos
    | E.Mapping_start { anchor; tag; _ }, pos -> make_mappings anchor tag pos (read_mappings []) file, pos
    | v, pos -> mk_err "Expected a valid YAML element or end of sequence, got" v pos file
  and read_sequence acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Sequence_end, pos -> acc, pos
    | v, pos -> let seq = (fst (read_node ~node_val:(Some(v, pos)) ())) in seq :: (fst (read_sequence acc)), pos
  and read_mappings acc : A.expr list * E.pos =
    match get_res (S.do_parse parser) with
    | E.Mapping_end, pos -> acc, pos
    | v, pos -> let map = read_mapping (v, pos) in map :: (fst (read_mappings acc)), pos
  and read_mapping first_node : A.expr =
    let key, pos1 =
      match first_node with
      | E.Scalar { anchor; tag; value; _ }, pos -> make_scalar anchor tag pos value file, pos
      | v, pos -> mk_err "Expected a valid scalar, got" v pos file
    in
    let value, pos2 = read_node () in
    make_mapping (pos1, pos2) (key, value) file
  in
  read_stream ()

(* Entry point *)

let program { str; file } = [A.exprstmt (get_res (S.parser str) |> parse file)]

let any str = A.E (get_res (S.parser str) |> parse "<pattern_file>")
