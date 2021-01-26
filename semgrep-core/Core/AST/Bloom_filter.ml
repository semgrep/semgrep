
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple wrapper over a third-party Bloom filter library.
 *
 * TODO: fill in the blank and choose one OCaml Bloom filter library:
 *  - https://github.com/mirage/bloomf/ available in OPAM, seems pretty good
 *  - https://github.com/sergezloto/ocaml-bloom-filter
 *  - Martin has one?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO *)
type t = {
  mutable added: bool;
}
[@@deriving show]

type elt = string

type bbool = No | Maybe

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let create () =
  { added = false }

let is_empty bf = not bf.added

let add _elt bf =
  bf.added <- true

let mem _elt _bf =
  Maybe

let is_subset _bf1 _bf2 =
  Maybe
