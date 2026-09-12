(*
   Simple, functional pretty-printer.
*)

open Printf

module Types = struct
  type node =
    | Line of string
    | Block of node list
    | Inline of node list
    | Space
    | Group of node list
end

open Types

type t = node list

(*
   Expand Inline nodes, remove empty blocks.
*)
let rec simplify l =
  List.map simplify_node l
  |> List.flatten

and simplify_node = function
  | Line _ as x -> [x]
  | Block l ->
      (match simplify l with
       | [] -> []
       | l -> [Block l]
      )
  | Inline l ->
      (match simplify l with
       | [] -> []
       | l -> l
      )
  | Space -> [Space]
  | Group l ->
      (match simplify l with
       | [] -> []
       | l -> [Group l]
      )

let really_collapse nodes =
  let buf = Buffer.create 100 in
  let rec add nodes =
    List.iter add_node nodes
  and add_node = function
    | Line s -> Buffer.add_string buf s
    | Block nodes -> add nodes
    | Inline nodes -> add nodes
    | Space -> Buffer.add_char buf ' '
    | Group nodes -> add nodes
  in
  add nodes;
  Line (Buffer.contents buf)

module Size = struct
  (* Length of a collapsible group. If None, the group is not collapsible. *)
  type t = int option

  let max_len = 60

  let add a b : t =
    match a, b with
    | None, _
    | _, None -> None
    | Some a, Some b ->
        let len = a + b in
        if len <= max_len then
          Some len
        else
          None
end

let collapse nodes =
  let rec collapse nodes =
    let l = List.map collapse_node nodes in
    let size =
      List.fold_left (fun acc (size, _node) -> Size.add acc size) (Some 0) l
    in
    size, List.map snd l

  and collapse_node = function
    | Line s as x ->
        Some (String.length s), x
    | Block [node] ->
        let size, node = collapse_node node in
        size, Block [node]
    | Block nodes ->
        let _size, nodes = collapse nodes in
        None, Block nodes
    | Inline _ -> assert false (* removed by 'simplify' *)
    | Space ->
        Some 1, Space
    | Group nodes ->
        let size, nodes = collapse nodes in
        if size <> None then
          size, really_collapse nodes
        else
          size, Group nodes
  in
  snd (collapse nodes)

let rec print_node buf indent (x : node) =
  match x with
  | Line s -> bprintf buf "%s%s\n" (String.make indent ' ') s
  | Block nodes -> print buf (indent + 2) nodes
  | Inline _ -> assert false (* removed by 'simplify' *)
  | Space -> ()
  | Group nodes -> print buf indent nodes

and print buf indent nodes =
  List.iter (print_node buf indent) nodes

let to_string nodes =
  let buf = Buffer.create 1000 in
  print buf 0 (simplify nodes |> collapse);
  Buffer.contents buf

let to_channel oc nodes =
  let data = to_string nodes in
  output_string oc data;
  flush oc

let to_file output_file nodes =
  let data = to_string nodes in
  let oc = open_out_bin output_file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc data)
