(*
   Utilities for working with AST_generic.raw_tree
*)

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

type ('tok, 'any) t =
  | Token of 'tok
  | List of ('tok, 'any) t list
  | Tuple of ('tok, 'any) t list
  | Case of string * ('tok, 'any) t
  | Option of ('tok, 'any) t option
  | Any of 'any
[@@deriving show { with_path = false }, eq, hash]

(* Find the left-most token in the tree *)
let rec first_tok (x : (_, _) t) =
  match x with
  | Token tok -> Some tok
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
let rec last_tok (x : (_, _) t) =
  match x with
  | Token tok -> Some tok
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
      let tok = Parse_info.unsafe_fake_info "" in
      (tok, tok)

let anys x =
  let rec anys acc (x : (_, _) t) =
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
