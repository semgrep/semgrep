(*
   A generic tree type meant to accommodate all tree types derived
   from tree-sitter grammars.
*)

open Printf
open Tree_sitter_gen.Indent.Types

type 'a t =
  | Token of Token.t
  | List of 'a t list
  | Tuple of 'a t list
  | Case of string * 'a t
  | Option of 'a t option
  | Any of 'a

let format ?(format_any = fun _ -> [Line "??"]) x =
  let rec format x : Tree_sitter_gen.Indent.t =
    match x with
    | Token (_loc, str) -> [ Line (sprintf "%S" str) ]
    | List [] -> [ Line "[]" ]
    | List [Token (_loc, str)] -> [ Line (sprintf "[%S]" str) ]
    | List xs -> [
        Line "[";
        Block (List.map format_inline xs);
        Line "]"
      ]
    | Tuple [] -> [ Line "()" ]
    | Tuple xs -> [
        Line "(";
        Block (List.map format_inline xs);
        Line ")"
      ]
    | Case (name, (Token (_loc, str))) -> [ Line (sprintf "%s %S" name str) ]
    | Case (name, x) -> [
        Line (sprintf "%s (" name);
        Block (format x);
        Line ")";
      ]
    | Option None -> [ Line "None" ]
    | Option (Some (Token (_loc, str))) -> [ Line (sprintf "Some %S" str) ]
    | Option (Some (List [])) -> [ Line "Some []" ]
    | Option (Some (List xs)) -> [
        Line "Some [";
        Block (List.map format_inline xs);
        Line "]";
      ]
    | Option (Some (Tuple [])) -> [ Line "Some ()" ]
    | Option (Some (Tuple xs)) -> [
        Line "Some (";
        Block (List.map format_inline xs);
        Line ")";
      ]
    | Option (Some x) -> [
        Line "Some (";
        Block (format x);
        Line ")";
      ]
    | Any x -> format_any x
  and format_inline x = Inline (format x)
  in
  format x

let to_string ?format_any x =
  format ?format_any x
  |> Tree_sitter_gen.Indent.to_string

let to_channel ?format_any oc x =
  to_string ?format_any x
  |> output_string oc
