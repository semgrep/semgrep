(*
   The location of a substring within a document.
*)

open Printf
open Lexing

type t = Lexing.position * Lexing.position

let pp fmt _t =
  (*if !pp_full_token_info
    then pp_token_mutable fmt t*)
  Format.fprintf fmt "()"

(* we don't care about position information in spacegrep/semgrep *)
let equal _ _ = true

module Pos = struct
  type t = Lexing.position

  (*
     Position that can be used in place of a valid position, where using
     an option is too expensive or inconvenient.
     It is guaranteed to be strictly less than any real position in a lexbuf.
  *)
  let dummy =
    { Lexing.pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 }

  let absolute pos = pos.pos_bol + pos.pos_cnum

  let compare a b = Int.compare (absolute a) (absolute b)

  (* Shift a position within the same line. *)
  let shift (pos : Lexing.position) offset =
    { pos with pos_cnum = pos.pos_cnum + offset }

  let show pos =
    sprintf "{lnum=%i, bol=%i, cnum=%i}" pos.pos_lnum pos.pos_bol pos.pos_cnum
end

let eq a b = a = b

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

let show (start, end_) = sprintf "(%s, %s)" (Pos.show start) (Pos.show end_)
