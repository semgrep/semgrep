(*
   OCaml interface to the tree-sitter C functions.

   This is a minimum low-level, imperative interface.
*)

(*
   These types match the types defined by tree-sitter, with suitable wrapping
   for compatibility with the OCaml runtime.
*)
type ts_tree
type ts_node
type ts_parser

type ts_point = Tree_sitter_output_t.position = {
  row: int;
  column: int;
}

module Parser = struct
  type t = ts_parser

  external parse_string :
    ts_parser -> string -> ts_tree = "octs_parser_parse_string"

  type read_function = int -> int -> int -> string option

  external parse :
    ts_parser -> ts_tree option -> read_function -> ts_tree
    = "octs_parser_parse"

  let parse_read_function : read_function Domain.DLS.key =
    Domain.DLS.new_key (fun () -> fun _ _ _ -> None)

  let parse_read (byte_offset : int) (row : int) (col : int) =
    (Domain.DLS.get parse_read_function) byte_offset row col

  let parse (parser : t) (tree : ts_tree option) read_function =
    Domain.DLS.set parse_read_function read_function;
    parse parser tree parse_read

  let () = Callback.register "octs__parse_read" parse_read
end

module Tree = struct
  type t = ts_tree

  external root_node : ts_tree -> ts_node = "octs_tree_root_node"

  external edit : ts_tree -> int -> int -> int -> int -> int -> int -> ts_tree
    = "octs_tree_edit_bytecode" "octs_tree_edit_native"

  (* This exposes ts_tree_delete, the C function to free
     the tree-sitter tree. We call this manually as a hack
     because the OCaml garbage collector is not aware of
     the tree. Ideally, we would instead give the garbage
     collector the information it needed about the tree.
     Currently this function is used solely in of_ts_tree
     in Tree_sitter_output *)
  external delete : ts_tree -> unit = "octs_tree_delete"
end

module Point = struct
  type t = ts_point
end

module Node = struct
  external string : ts_node -> string = "octs_node_string"
  external child_count : ts_node -> int = "octs_node_child_count"
  external child : ts_node -> int -> ts_node = "octs_node_child"
  external parent : ts_node -> ts_node = "octs_node_parent"
  external named_child_count : ts_node -> int = "octs_node_named_child_count"
  external named_child : ts_node -> int -> ts_node = "octs_node_named_child"

  external bounded_named_index :
    ts_node -> int = "octs_node_bounded_named_index"

  external named_index : ts_node -> int = "octs_node_named_index"
  external index : ts_node -> int = "octs_node_index"

  external descendant_for_point_range :
    ts_node -> int -> int -> int -> int -> ts_node
    = "octs_node_descendant_for_point_range"

  external start_byte : ts_node -> int = "octs_node_start_byte"
  external end_byte : ts_node -> int = "octs_node_end_byte"
  external start_point : ts_node -> ts_point = "octs_node_start_point"
  external end_point : ts_node -> ts_point = "octs_node_end_point"
  external has_changes : ts_node -> bool = "octs_node_has_changes"
  external has_error : ts_node -> bool = "octs_node_has_error"
  external is_missing : ts_node -> bool = "octs_node_is_missing"
  external is_null : ts_node -> bool = "octs_node_is_null"
  external is_named : ts_node -> bool = "octs_node_is_named"
  external is_error : ts_node -> bool = "octs_node_is_error"
  external is_extra : ts_node -> bool = "octs_node_is_extra"
  external symbol : ts_node -> int = "octs_node_symbol"
  external type_ : ts_node -> string = "octs_node_type"
end
