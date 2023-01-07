(*
   Utilities for working with AST_generic.raw_tree
*)

type t = AST_generic.raw_tree

module G = AST_generic

(* Find the left-most token in the tree *)
let rec first_tok (x : t) =
  match x with
  | RawToken tok -> Some tok
  | RawList xs -> first_tok_in_list xs
  | RawTuple xs -> first_tok_in_list xs
  | RawCase (_cons, x) -> first_tok x
  | RawOption opt -> (
      match opt with
      | None -> None
      | Some x -> first_tok x)
  | RawAny _ ->
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
let rec last_tok (x : t) =
  match x with
  | RawToken tok -> Some tok
  | RawList xs -> last_tok_in_list xs
  | RawTuple xs -> last_tok_in_list xs
  | RawCase (_cons, x) -> last_tok x
  | RawOption opt -> (
      match opt with
      | None -> None
      | Some x -> last_tok x)
  | RawAny _ -> None

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
  let rec anys acc (x : t) =
    match x with
    | RawToken _tok -> acc
    | RawList xs -> List.fold_left anys acc xs
    | RawTuple xs -> List.fold_left anys acc xs
    | RawCase (_cons, x) -> anys acc x
    | RawOption opt -> (
        match opt with
        | None -> acc
        | Some x -> anys acc x)
    | RawAny any -> any :: acc
  in
  anys [] x |> List.rev
