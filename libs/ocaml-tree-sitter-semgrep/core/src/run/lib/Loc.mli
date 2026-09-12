(*
   A location in a source file, expressed as pair of positions.
*)

type pos = Tree_sitter_bindings.Tree_sitter_output_t.position = {
  row : int; (* 0-based *)
  column : int; (* 0-based *)
}

(* TODO: include filename as a field? *)
type t = {
  start: pos; (* inclusive *)
  end_: pos; (* exclusive i.e. start = end_ gives the empty string. *)
}
