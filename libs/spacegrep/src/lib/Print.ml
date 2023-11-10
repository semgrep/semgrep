(*
   A pretty-printer for an AST.

   It's meant both for debugging the parser and for showing a user how
   a given program is interpreted.
*)

open Printf
open Pattern_AST

let string_of_atom atom =
  match atom with
  | Word s -> s
  | Punct c -> sprintf "%c" c
  | Byte c -> sprintf "0x%02x" (Char.code c)
  | Metavar s -> sprintf "$%s" s

let print_atom buf _loc indent atom =
  match atom with
  | Word s -> bprintf buf "%s%s\n" indent s
  | Punct c -> bprintf buf "%s%c\n" indent c
  | Byte c -> bprintf buf "%s0x%02x\n" indent (Char.code c)
  | Metavar s -> bprintf buf "%s$%s\n" indent s

let rec print_node buf indent node =
  match node with
  | Atom (loc, atom) -> print_atom buf loc indent atom
  | List nodes -> print_nodes buf (indent ^ "  ") nodes
  | Dots (_loc, None) -> bprintf buf "%s...\n" indent
  | Dots (_loc, Some s) -> bprintf buf "%s$...%s\n" indent s
  | End -> ()

and print_nodes buf indent nodes = List.iter (print_node buf indent) nodes

let print_root buf nodes = print_nodes buf "" nodes
let to_buffer buf nodes = print_root buf nodes

let to_string nodes =
  let buf = Buffer.create 1000 in
  to_buffer buf nodes;
  Buffer.contents buf

let to_channel oc nodes = output_string oc (to_string nodes)
let to_stdout nodes = to_channel stdout nodes

let to_file file nodes =
  let oc = open_out_bin file in
  Common.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> to_channel oc nodes)

(* Same but using unambiguous output format. *)
module Debug = struct
  let show_loc = false

  let print_loc buf (start, _end_) =
    if show_loc then bprintf buf "%3i: " start.Lexing.pos_lnum

  let print_atom buf loc indent atom =
    print_loc buf loc;
    match atom with
    | Word s -> bprintf buf "%sWord '%s'\n" indent (String.escaped s)
    | Punct c -> bprintf buf "%sPunct %C\n" indent c
    | Byte c -> bprintf buf "%sByte 0x%02x\n" indent (Char.code c)
    | Metavar s -> bprintf buf "%sMetavar %s\n" indent s

  let rec print_node buf indent node =
    match node with
    | Atom (loc, atom) -> print_atom buf loc indent atom
    | List nodes ->
        bprintf buf "%sList (\n" indent;
        print_nodes buf (indent ^ "  ") nodes;
        bprintf buf "%s)\n" indent
    | Dots (loc, None) -> bprintf buf "%a%sDots\n" print_loc loc indent
    | Dots (loc, Some s) -> bprintf buf "%a%sDots %s\n" print_loc loc indent s
    | End -> bprintf buf "%sEnd\n" indent

  and print_nodes buf indent nodes = List.iter (print_node buf indent) nodes

  let print_root buf nodes = print_nodes buf "" nodes
  let to_buffer buf nodes = print_root buf nodes

  let to_string nodes =
    let buf = Buffer.create 1000 in
    to_buffer buf nodes;
    Buffer.contents buf

  let to_channel oc nodes = output_string oc (to_string nodes)
  let to_stdout nodes = to_channel stdout nodes

  let to_file file nodes =
    let oc = open_out_bin file in
    Common.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> to_channel oc nodes)
end
