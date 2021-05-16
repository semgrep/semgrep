module B = Bloomf
module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Simple wrapper over a third-party Bloom filter library.
 *
 * Some OCaml Bloom filter library options:
 *  - https://github.com/mirage/bloomf/ available in OPAM, seems pretty good
 *  - https://github.com/sergezloto/ocaml-bloom-filter
 *  - Martin has one?
 * We choose Bloomf because it was available in OPAM, so far no problems,
 * though it would be nice if it had a bloom filter intersection function
 *
 * As a second option, we can also use a set instead of a bloom filter. The
 * two possible filter types is a temporary state. Eventually, based on our
 * benchmarking results, we will settle on one.
 *
 * In theory, sets ought to be less efficient than bloom filters, since bloom
 * filters use bits and are constant size. However, in practice, the
 * implementation of sets and strings are designed to reuse memory as much as
 * possible. Since every string in the set is already in the AST, and a child
 * node's set is a subset of its parent, there is a great deal of opportunity
 * for reusing. Sets will also have no probability of failing, which should
 * lead to a speedup.
 *
 * Other than memory, the main drawback is that bloom filter comparison may be
 * slightly faster than set comparison (especially if bloom filters are
 * implemented as bitsets that can be intersected). However, it does not appear
 * to be a significant factor, and the set comparison can be improved over the
 * POC implementation.
 * *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO *)
type elt = string

(* Since Bloomf.t is imperative, the API for Bloom_filter is as well.
 * Set.t is immutable, so we use a ref to integrate it with the current
 * API. If we switch to using Set instead of Bloom, we should change
 * the API *)
type filter = Bloom of elt B.t | Set of elt Set.t ref

type t = { mutable added : bool; filter : filter }

type bbool = No | Maybe

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let pp _formatter _bf = ()

(* This gives a bloom filter with an expected false positive rate of 0.01
 * when 2500 elements are added. The numbers are chosen by experimentation on
 * a file with 10000 lines of javascript *)
let create set_instead_of_bloom =
  {
    added = false;
    filter =
      ( if set_instead_of_bloom then Set (ref Set.empty)
      else Bloom (B.create ~error_rate:0.01 2500) );
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
  if is_in then Maybe else No

let is_subset pattern_list bf =
  let patterns_in_bf b x = match b with No -> No | Maybe -> mem x bf in
  List.fold_left patterns_in_bf Maybe pattern_list

let make_bloom_from_set set_instead_of_bloom ids =
  if set_instead_of_bloom then { added = true; filter = Set (ref ids) }
  else
    let bf = create set_instead_of_bloom in
    let () =
      match bf.filter with
      | Bloom _ -> Set.iter (fun id -> add id bf) ids
      | Set _ -> raise Common.Impossible
    in
    bf
