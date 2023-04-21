open Common
open Tok

(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(* for error reporting *)
let string_of_token_location x = Pos.string_of_pos x.pos

let string_of_info x =
  match Tok.loc_of_tok x with
  | Ok loc -> string_of_token_location loc
  | Error msg -> spf "unknown location (%s)" msg
