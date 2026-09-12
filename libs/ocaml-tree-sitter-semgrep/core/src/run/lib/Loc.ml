(*
   Locations in a source file.
*)

type pos = Tree_sitter_bindings.Tree_sitter_output_t.position = {
  row : int;
  column : int;
}

type t = {
  start: pos;
  end_: pos;
}
