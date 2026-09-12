(*
   Simple utilities to work on the types defined in Tree_sitter_output.atd.
*)

open Tree_sitter_API
open Tree_sitter_output_t

let rec of_ts_node get_node_id ts_node =
  let id = get_node_id () in
  let type_ = Node.type_ ts_node in
  let start_pos = Node.start_point ts_node in
  let end_pos = Node.end_point ts_node in
  let children = read_children get_node_id ts_node in
  let kind =
    match type_ with
    | "ERROR" -> Error
    | _ ->
        match children with
        | None -> Literal type_
        | Some _ -> Name type_
  in
  let is_missing = Node.is_missing ts_node in
  {
    type_;
    start_pos;
    end_pos;
    children;
    kind;
    is_missing;
    id;
  }

and read_children get_node_id ts_node =
  if Node.is_named ts_node then
    match Node.child_count ts_node with
    | 0 -> Some []
    | child_count ->
        Some (
          List.init child_count (fun i ->
            of_ts_node get_node_id (Node.child ts_node i)
          )
        )
  else
    None

let of_ts_tree ts_tree =
  let root = Tree.root_node ts_tree in
  let counter = ref (-1) in
  let get_node_id () =
    incr counter;
    !counter
  in
  let res = of_ts_node get_node_id root in
  (* This manually frees the tree. Do not use the tree in any
     way after this. If this is changed, also update the comment
     in Tree_sitter_API *)
  Tree.delete ts_tree;
  res

let to_json ?(pretty = false) node =
  let compact_json = Tree_sitter_output_j.string_of_node node in
  if pretty then
    Yojson.Safe.prettify compact_json
  else
    compact_json
