open Common
open Tok

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* TODO: remove this file, spread its content in separate files *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)

(* for error reporting *)
let string_of_token_location x = Pos.string_of_pos x.pos

let string_of_info x =
  match Tok.loc_of_tok x with
  | Ok loc -> string_of_token_location loc
  | Error msg -> spf "unknown location (%s)" msg

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* TODO: we should filter with is_origintok() first, to avoid having
 * the caller to do it.
 *)
let min_max_ii_by_pos xs =
  match xs with
  | [] ->
      raise
        (Tok.NoTokenLocation
           "Match returned an empty list with no token location information; \
            this may be fixed by adding enclosing token information (e.g. \
            bracket or parend tokens) to the list's enclosing node type.")
  | [ x ] -> (x, x)
  | x :: xs ->
      let pos_leq p1 p2 = Tok.compare_pos p1 p2 =|= -1 in
      xs
      |> List.fold_left
           (fun (minii, maxii) e ->
             let maxii' = if pos_leq maxii e then e else maxii in
             let minii' = if pos_leq e minii then e else minii in
             (minii', maxii'))
           (x, x)
