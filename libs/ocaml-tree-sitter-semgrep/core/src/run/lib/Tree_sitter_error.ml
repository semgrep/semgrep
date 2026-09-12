(*
   Simple utilities to work on the types defined in Tree_sitter_output.atd.
*)

open Printf
open Tree_sitter_bindings.Tree_sitter_output_t

type error_class = {
  parent: node_kind;
  left_sibling: node_kind option;
  first_child: node_kind option;
}

type t = {
  kind: Tree_sitter_error_t.error_kind;
  msg: string;
  file: Src_file.info;
  start_pos: position;
  end_pos: position;
  substring: string;
  snippet: Snippet.t;
  error_class: error_class option;
}

exception Error of t

let string_of_node_kind (x : node_kind) =
  match x with
  | Literal s -> sprintf "%S" s
  | Name s -> s
  | Error -> "ERROR"

let string_of_node (x : node) =
  let node_kind = string_of_node_kind x.kind in
  match x.children with
  | None ->
      sprintf "node type: %s" node_kind
  | Some children ->
      sprintf "\
node type: %s
children: [
%s]"
        node_kind
        (List.map
           (fun (x : node) -> sprintf "  %s\n" (string_of_node_kind x.kind))
           children
         |> String.concat "")

let string_of_error_class (x : error_class) =
  let parent =
    sprintf "parent: %s" (string_of_node_kind x.parent) in
  let left_sibling =
    match x.left_sibling with
    | None -> ", no left sibling"
    | Some x ->
        sprintf ", left sibling: %s" (string_of_node_kind x)
  in
  let first_child =
    match x.first_child with
    | None -> ", no children"
    | Some x ->
        sprintf ", first child: %s" (string_of_node_kind x)
  in
  parent ^ left_sibling ^ first_child

let rec find_left_sibling (children : node list) target_node =
  match children with
  | left :: node :: _ when node == target_node -> Some left.kind
  | node :: _ when node == target_node -> None
  | _ :: xs -> find_left_sibling xs target_node
  | [] -> assert false (* target_node must exist in 'children' list *)

let error_class_of_node ~(parent : node option) node =
  match parent with
  | None -> None
  | Some { children = None; _ } -> assert false (* parent must have children *)
  | Some { kind; children = Some children; _ } ->
      let left_sibling = find_left_sibling children node in
      let first_child =
        match node.children with
        | None | Some [] -> None
        | Some (x :: _) -> Some x.kind
      in
      Some {
        parent = kind;
        left_sibling;
        first_child;
      }

let create kind src ?parent node msg =
  let msg = sprintf "\
%s
%s"
      (string_of_node node)
      msg
  in
  let start_pos = node.start_pos in
  let end_pos = node.end_pos in
  {
    kind;
    msg;
    file = Src_file.info src;
    start_pos;
    end_pos;
    substring = Src_file.get_region src start_pos end_pos;
    snippet = Snippet.extract src ~start_pos ~end_pos;
    error_class = error_class_of_node ~parent node;
  }

let fail kind src node msg =
  raise (Error (create kind src node msg))

let internal_error src node msg =
  fail Internal src node msg

let string_of_file_info (src_info : Src_file.info) =
  match src_info.path with
  | Some path -> sprintf "File %s" path
  | None -> src_info.name

(* Take an error message and prepend the location information,
   in a human-readable and possibly computer-readable format (TODO check with
   emacs etc.)
*)
let to_string ?(style = Snippet.Text) (err : t) =
  let start = err.start_pos in
  let end_ = err.end_pos in
  let loc =
    let src_name = string_of_file_info err.file in
    if start.row = end_.row then
      sprintf "%s, line %i, characters %i-%i:"
        src_name
        (start.row + 1) start.column end_.column
    else
      sprintf "%s, line %i, character %i to line %i, character %i:"
        src_name
        (start.row + 1) start.column (end_.row + 1) end_.column
  in
  let error_class =
    match err.error_class with
    | None -> ""
    | Some x ->
        sprintf "%s\n" (string_of_error_class x)
  in
  sprintf "\
%s
%s%s%s"
    loc
    (Snippet.format ~style err.snippet)
    error_class
    err.msg

let to_json_error (x : t): Tree_sitter_error_t.json_error = {
  kind = x.kind;
  msg = x.msg;
  file = x.file.name;
  start_pos = x.start_pos;
  end_pos = x.end_pos;
  substring = x.substring;
  error_class = Option.map string_of_error_class x.error_class;
}

let log_json_errors out_file errors =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 out_file in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
       List.iter (fun err ->
         let simplified_err = to_json_error err in
         let json = Tree_sitter_error_j.string_of_json_error simplified_err in
         fprintf oc "%s\n" json
       ) errors
    )
