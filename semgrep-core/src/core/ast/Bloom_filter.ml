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

type filter = Bloom of elt B.t | Set of elt Set.t ref

type t = {
  mutable added: bool;
  filter: filter;
}

type bbool = No | Maybe

exception Impossible

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let pp _formatter _bf = ()

(* This gives a bloom filter with an expected false positive rate of 0.01
 * when 2500 elements are added. The numbers are chosen by experimentation on
 * a file with 10000 lines of javascript *)
let create set_instead_of_bloom =
  { added = false;
    filter = if set_instead_of_bloom then Set (ref Set.empty)
      else Bloom (B.create ~error_rate:0.01 2500)
  }

let is_empty bf = not bf.added

let add elt bf =
  bf.added <- true;
  match bf.filter with
  | Bloom filter -> B.add filter elt
  | Set s -> s := Set.add elt !s

let mem elt bf =
  let is_in =
    match bf.filter with
    | Bloom filter -> B.mem filter elt
    | Set s -> Set.mem elt !s
  in
  if is_in then Maybe
  else No

let is_subset pattern_list bf =
  let patterns_in_bf b x =
    match b with
    | No -> No
    | Maybe -> mem x bf
  in
  List.fold_left patterns_in_bf Maybe pattern_list

let make_bloom_from_set set_instead_of_bloom ids =
  if set_instead_of_bloom then
    { added = true; filter = Set (ref ids) }
  else
    let bf = create set_instead_of_bloom in
    let () =
      match bf.filter with
      | Bloom _ -> Set.iter (fun id -> add id bf) ids
      | Set _ -> raise Impossible
    in
    bf
