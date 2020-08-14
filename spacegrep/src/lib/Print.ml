(*
   A pretty-printer for an AST.

   It's meant both for debugging the parser and for showing a user how
   a given program is interpreted.
*)

open Printf
open AST

let print_atom buf indent atom =
  match atom with
  | Word s -> bprintf buf "%s%s\n" indent s
  | Punct c -> bprintf buf "%s%c\n" indent c
  | Byte c -> bprintf buf "%s0x%02x\n" indent (Char.code c)

let rec print_node buf indent node =
  match node with
  | Atom atom -> print_atom buf indent atom
  | List nodes -> List.iter (print_node buf (indent ^ "  ")) nodes

let print_root buf node =
  match node with
  | Atom _ -> print_node buf "" node
  | List nodes -> List.iter (print_node buf "") nodes

let to_buffer buf node = print_root buf node

let to_string node =
  let buf = Buffer.create 1000 in
  to_buffer buf node;
  Buffer.contents buf

let to_channel oc node =
  output_string oc (to_string node)

let to_stdout node =
  to_channel stdout node

let to_file file node =
  let oc = open_out file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> to_channel oc)
