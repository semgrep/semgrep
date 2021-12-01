module Set = Set_

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is just a wrapper around the Set_ module. The name is a legacy from
 * when we intended to use bloom filters to save space
 *
 * In theory, sets ought to be less efficient than bloom filters, since bloom
 * filters use bits and are constant size. However, in practice, the
 * implementation of sets and strings are designed to reuse memory as much as
 * possible. Since every string in the set is already in the AST, and a child
 * node's set is a subset of its parent, there is a great deal of opportunity
 * for reusing. From experimental spotchecking, there does not seem to be a
 * significant difference between bloom and set filters in memory usage
 *
 * Using sets also ensures that we will never have a false positive, which from
 * comparing benchmarks on https://dashboard.semgrep.dev/metrics appears to make
 * a small difference.
 *
 * We keep this hidden behind an interface so that we can make changes easily to
 * it in the future, if we want.
 * *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO *)
type elt = string

type filter = elt Set.t

type t = filter

(*****************************************************************************)
(* API entry points *)
(*****************************************************************************)

let pp _formatter _bf = ()

let create = Set.empty

let is_empty bf = Set.is_empty bf

let add elt bf = Set.add elt bf

let mem elt bf = Set.mem elt bf

let is_subset patterns bf = Set.subset patterns bf

let make_bloom_from_set ids = ids

let set_of_filter bf = bf
