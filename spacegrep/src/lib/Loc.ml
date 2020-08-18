(*
   The location of a substring within a document.
*)

type t = Lexing.position * Lexing.position

open Lexing

module Pos = struct
  type t = Lexing.position

  let dummy = Lexing.dummy_pos

  (* Shift a position within the same line. *)
  let shift (pos : Lexing.position) offset =
    { pos with pos_cnum = pos.pos_cnum + offset }
end

let length (a, b) =
  let start = a.pos_bol + a.pos_cnum in
  let end_ = b.pos_bol + a.pos_cnum in
  end_ - start

(* Same usage as String.sub. Both positions must be on the same line. *)
let sub (a, b) pos len =
  if a.pos_bol <> b.pos_bol then
    invalid_arg "Loc.sub: only valid on a single-line location.";
  (Pos.shift a pos, Pos.shift a (pos + len))

let dummy = (Pos.dummy, Pos.dummy)
