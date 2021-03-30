module B = Bloomf
module Set = Set_

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

type filter = Bloom of elt B.t | Set of elt Set.t

type t = {
  mutable added: bool;
  filter: filter;
}

type bbool = No | Maybe

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let pp _formatter _bf = ()

(* This gives a bloom filter with an expected false positive rate of 0.01
 * when 2500 elements are added. The numbers are chosen by experimentation on
 * a file with 10000 lines of javascript *)
let create set_or_bloom =
  { added = false;
    filter = if set_or_bloom then Bloom (B.create ~error_rate:0.01 2500)
      else Set Set.empty
  }

let is_empty bf = not bf.added

let add elt bf =
  bf.added <- true;
  B.add bf.filter elt

let mem elt bf =
  if B.mem bf.filter elt then Maybe
  else No

let is_subset pattern_list bf =
  let patterns_in_bf b x =
    match b with
    | No -> No
    | Maybe -> mem x bf
  in
  List.fold_left patterns_in_bf Maybe pattern_list
