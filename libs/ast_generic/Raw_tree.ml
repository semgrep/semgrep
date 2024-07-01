(*
   Utilities for working with AST_generic.raw_tree
*)

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

type 'any t =
  (* Tok.t_always_equal = Tok.t with special equal and hash functions *)
  | Token of (string * Tok.t_always_equal)
  | List of 'any t list
  | Tuple of 'any t list
  | Case of string * 'any t
  | Option of 'any t option
  | Any of 'any
[@@deriving show { with_path = false }, eq, ord, hash]

(* Find the left-most token in the tree *)
let rec first_tok (x : _ t) =
  match x with
  | Token (_str, tok) -> Some tok
  | List xs -> first_tok_in_list xs
  | Tuple xs -> first_tok_in_list xs
  | Case (_cons, x) -> first_tok x
  | Option opt -> (
      match opt with
      | None -> None
      | Some x -> first_tok x)
  | Any _ ->
      (* We could continue the search in the generic AST but that would
         be a big endeavor. *)
      None

and first_tok_in_list xs =
  match xs with
  | x :: xs -> (
      match first_tok x with
      | None -> first_tok_in_list xs
      | Some _ as res -> res)
  | [] -> None

(* Find the right-most token in the tree *)
let rec last_tok (x : _ t) =
  match x with
  | Token (_str, tok) -> Some tok
  | List xs -> last_tok_in_list xs
  | Tuple xs -> last_tok_in_list xs
  | Case (_cons, x) -> last_tok x
  | Option opt -> (
      match opt with
      | None -> None
      | Some x -> last_tok x)
  | Any _ -> None

and last_tok_in_list xs =
  match xs with
  | [] -> None
  | x :: xs -> (
      match last_tok_in_list xs with
      | None -> last_tok x
      | Some _ as res -> res)

let loc x =
  match (first_tok x, last_tok x) with
  | None, None -> None
  | Some first, Some last -> Some (first, last)
  | None, Some _
  | Some _, None ->
      assert false

let unsafe_loc x =
  match loc x with
  | Some res -> res
  | None ->
      let tok = Tok.unsafe_fake_tok "" in
      (tok, tok)

let anys x =
  let rec anys acc (x : _ t) =
    match x with
    | Token _tok -> acc
    | List xs -> List.fold_left anys acc xs
    | Tuple xs -> List.fold_left anys acc xs
    | Case (_cons, x) -> anys acc x
    | Option opt -> (
        match opt with
        | None -> acc
        | Some x -> anys acc x)
    | Any any -> any :: acc
  in
  anys [] x |> List.rev

let map ~map_any x =
  let rec map x =
    match x with
    | Token tok -> Token tok
    | List xs -> List (List_.map map xs)
    | Tuple xs -> List (List_.map map xs)
    | Case (cons, x) -> Case (cons, map x)
    | Option opt ->
        Option
          (match opt with
          | None -> None
          | Some x -> Some (map x))
    | Any any -> Any (map_any any)
  in
  map x

let visit ~v_raw_tree ~v_token ~v_any = function
  | Token tok -> v_token tok
  | List xs -> List.iter v_raw_tree xs
  | Tuple xs -> List.iter v_raw_tree xs
  | Case (_cons, x) -> v_raw_tree x
  | Option opt -> (
      match opt with
      | None -> ()
      | Some x -> v_raw_tree x)
  | Any any -> v_any any
