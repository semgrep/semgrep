(*
   Parse a source file or load its json representation.
*)

open Tree_sitter_bindings

type t = {
  src: Src_file.t;
  root: Tree_sitter_output_t.node
}

let src x = x.src
let root x = x.root

let set_missing_fields root_node =
  let open Tree_sitter_output_t in
  let counter = ref (-1) in
  let create_id () = incr counter; !counter in
  let rec map node =
    let id = create_id () in
    assert (id >= 0);
    let children = Option.map (List.map map) node.children in
    let kind =
      match children with
      | None -> Literal node.type_
      | Some _ -> Name node.type_
    in
    { node with id; children; kind }
  in
  map root_node

let parse_json_file json_file =
  Atdgen_runtime.Util.Json.from_file
    Tree_sitter_output_j.read_node
    json_file
  |> set_missing_fields

let load_json_file ~src_file ~json_file =
  let root = parse_json_file json_file in
  let src = Src_file.load_file src_file in
  { src; root }

let parse_source_string ?src_file ts_parser src_data =
  let src = Src_file.load_string ?src_file src_data in
  let root =
    Tree_sitter_API.Parser.parse_string ts_parser src_data
    |> Tree_sitter_output.of_ts_tree
  in
  { src; root }

let parse_source_file ts_parser src_file =
  let src_data = Util_file.read_file src_file in
  parse_source_string ~src_file ts_parser src_data

let print src =
  Tree_sitter_dump.to_stdout [src.root]
