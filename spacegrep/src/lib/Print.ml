(*
   A pretty-printer for an AST.

   It's meant both for debugging the parser and for showing a user how
   a given program is interpreted.
*)

open Printf
open Pattern_AST

let print_atom buf indent atom =
  match atom with
  | Word s -> bprintf buf "%s%s\n" indent s
  | Punct c -> bprintf buf "%s%c\n" indent c
  | Byte c -> bprintf buf "%s0x%02x\n" indent (Char.code c)
  | Metavar s -> bprintf buf "%s$%s\n" indent s

let rec print_node buf indent node =
  match node with
  | Atom atom -> print_atom buf indent atom
  | List nodes -> print_nodes buf (indent ^ "  ") nodes
  | Dots -> bprintf buf "%s...\n" indent

and print_nodes buf indent nodes =
  List.iter (print_node buf indent) nodes

let print_root buf nodes =
  print_nodes buf "" nodes

let to_buffer buf nodes = print_root buf nodes

let to_string nodes =
  let buf = Buffer.create 1000 in
  to_buffer buf nodes;
  Buffer.contents buf

let to_channel oc nodes =
  output_string oc (to_string nodes)

let to_stdout nodes =
  to_channel stdout nodes

let to_file file nodes =
  let oc = open_out file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> to_channel oc nodes)

(* Same but using unambiguous output format. *)
module Debug = struct
  let print_atom buf indent atom =
    match atom with
    | Word s -> bprintf buf "%sWord '%s'\n" indent (String.escaped s)
    | Punct c -> bprintf buf "%sPunct %C\n" indent c
    | Byte c -> bprintf buf "%sByte 0x%02x\n" indent (Char.code c)
    | Metavar s -> bprintf buf "%sMetavar %s\n" indent s

  let rec print_node buf indent node =
    match node with
    | Atom atom -> print_atom buf indent atom
    | List nodes ->
        bprintf buf "%sList (\n" indent;
        print_nodes buf (indent ^ "  ") nodes;
        bprintf buf "%s)\n" indent
    | Dots -> bprintf buf "%sDots\n" indent

  and print_nodes buf indent nodes =
    List.iter (print_node buf indent) nodes

  let print_root buf nodes =
    print_nodes buf "" nodes

  let to_buffer buf nodes = print_root buf nodes

  let to_string nodes =
    let buf = Buffer.create 1000 in
    to_buffer buf nodes;
    Buffer.contents buf

  let to_channel oc nodes =
    output_string oc (to_string nodes)

  let to_stdout nodes =
    to_channel stdout nodes

  let to_file file nodes =
    let oc = open_out file in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> to_channel oc nodes)
end
