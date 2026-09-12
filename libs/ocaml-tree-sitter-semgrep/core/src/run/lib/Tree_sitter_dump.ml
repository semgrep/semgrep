(*
   Compact pretty-printing of the tree-sitter output.
*)

open Printf
open Tree_sitter_bindings.Tree_sitter_output_t

let extend_indent s =
  if String.length s mod 4 = 0 then
    s ^ "| "
  else
    s ^ "  "

let string_of_node_kind (kind : node_kind) =
  match kind with
  | Name s -> s
  | Literal s -> sprintf "%S" s
  | Error -> "!ERROR!"

let to_buf buf nodes =
  let rec print indent nodes =
    List.iter (print_node indent) nodes
  and print_node indent node =
    match node.children with
    | None | Some [] ->
        bprintf buf "%s%s\n" indent (string_of_node_kind node.kind);
    | Some children ->
        bprintf buf "%s%s:\n" indent (string_of_node_kind node.kind);
        print (extend_indent indent) children;
  in
  print "" nodes

let to_string nodes =
  let buf = Buffer.create 1000 in
  to_buf buf nodes;
  Buffer.contents buf

let to_stdout nodes =
  print_string (to_string nodes);
  flush stdout
