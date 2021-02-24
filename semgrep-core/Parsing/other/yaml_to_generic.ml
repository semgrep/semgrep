module Y = Yaml
module S = Yaml.Stream
module E = Yaml.Stream.Event
module M = Yaml.Stream.Mark
module A = AST_generic

exception Unimplemented
exception ParseError of string

(* Parser for YAML. The tree-sitter grammar for YAML is somewhat complicated,
 * since YAML is whitespace sensitive. Instead, we use the OCaml Yaml module,
 * which we also use to parse YAML for semgrep. To get positions, we use the
 * low-level Stream API, which we parse into a generic AST
*)

(* Helper functions *)

let fst (a, _) = a

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

let mk_err err v {E.start_mark = {M.line; M.column; _}; _} =
  Printf.sprintf "%s %s at line %d column %d" err (p_token v) line column

let get_res = function
  | Result.Error (`Msg s) -> raise (ParseError s)
  | Result.Ok v -> v

let any_of_list = function
  | [x] -> A.E x
  | xs -> A.E (A.Seq xs) (* Change to array? *)

let tok (index, line, column) str file =
  {Parse_info.token = Parse_info.FakeTokStr (str, Some ({str; charpos = index; line; column; file}, 0));
   Parse_info.transfo = NoTransfo}

let mk_tok {E.start_mark = {M.index; M.line; M.column}; _} str file =
  tok (index, line, column) str file

let mk_bracket ({E.start_mark = {M.index = s_index; M.line = s_line; M.column = s_column}; _},
                {E.end_mark = {M.index = e_index; M.line = e_line; M.column = e_column}; _}) v file =
  tok (s_index, s_line, s_column) "(" file, v, tok (e_index, e_line, e_column) ")" file

(* Parser functions *)

let make_alias anchor pos file : A.name =
  A.Id ((anchor, (mk_tok pos "" file)), {id_resolved = ref None; id_type = ref None; id_constness = ref None})

let make_scalar _anchor _tag pos value file : A.expr =
  A.L (A.String (value, mk_tok pos value file))

(* Sequences are arrays in the generic AST *)
let make_sequence _anchor _tag start_pos (es, end_pos) file =
  A.Container(A.Array, mk_bracket (start_pos, end_pos) es file)

(* Mappings are dictionaries in the generic AST *)
let make_mappings _anchor _tag start_pos (es, end_pos) file =
  A.Container(A.Dict, mk_bracket (start_pos, end_pos) es file)

let parse file parser : A.any =
  (* Parse states *)
  let rec read_stream () : A.any =
    match get_res (S.do_parse parser) with
    | E.Stream_start _, _pos -> any_of_list (read_documents [])
    | v, pos -> raise (ParseError (mk_err "Expected start of string, got" v pos))
  and read_documents acc : A.expr list =
    match get_res (S.do_parse parser) with
    | E.Document_start _, _pos -> let docs = read_document () in docs :: (read_documents acc)
    | E.Stream_end, _ -> acc
    | v, pos -> raise (ParseError (mk_err "Expected start of document or end of stream, got" v pos))
  and read_document () : A.expr =
    let node_ast, _ = read_node () in
    match get_res (S.do_parse parser) with
    | E.Document_end _, _pos -> node_ast
    | v, pos -> raise (ParseError (mk_err "Expected end of document, got" v pos))
  and read_node ?node_val:(res = None) () : A.expr * E.pos =
    let res = match res with None -> get_res (S.do_parse parser) | Some r -> r in
    match res with
    | E.Alias { anchor }, pos -> A.N (make_alias anchor pos file), pos
    | E.Scalar { anchor; tag; value; _ }, pos -> make_scalar anchor tag pos value file, pos
    | E.Sequence_start { anchor; tag; _ }, pos -> make_sequence anchor tag pos (read_sequence []) file, pos
    | E.Mapping_start { anchor; tag; _ }, pos -> make_mappings anchor tag pos (read_mappings []) file, pos
    | v, pos -> raise (ParseError (mk_err "Expected a valid YAML element or end of sequence, got" v pos))
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
      | v, pos -> raise (ParseError (mk_err "Expected a valid scalar, got" v pos))
    in
    let value, pos2 = read_node () in
    A.Tuple (mk_bracket (pos1, pos2) [key; value] file)
  in
  read_stream ()

let parse_yaml str =
  get_res (S.parser str) |> parse "dummy_file_name"

(* Entry point *)

let program _file = (* parse_yaml file *) raise Unimplemented

let any file = parse_yaml file
