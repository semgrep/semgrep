module B = Bloomf

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
type elt = string

type t = {
  mutable added: bool;
  filter: elt B.t;
}

type bbool = No | Maybe

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let pp _formatter _bf = ()

let create () =
  { added = false;
    filter = B.create ~error_rate:0.01 10 }

let is_empty bf = not bf.added

let add elt bf =
  bf.added <- true;
  B.add bf.filter elt

let mem elt bf =
  if B.mem bf.filter elt then Maybe
  else No

(* TODO I think this should be is_subset pattern_set bf *)
let is_subset _bf1 _bf2 =
  Maybe
