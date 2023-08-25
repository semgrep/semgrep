(* This module is used mostly to decide between pysemgrep and osemgrep.
 * It could be used for different things later.
 *)

type t =
  (* mix of pysemgrep and osemgrep, depending on CLI arguments (see CLI.ml) *)
  | Default
  (* for forcing pysemgrep *)
  | Legacy
  (* for forcing osemgrep *)
  | Experimental
  (* Leaving on the edge, using osemgrep with osemgrep-only features enabled *)
  | Develop
[@@deriving show]

(* --experimental/--legacy/--develop CLI processing *)
val o_maturity : t Cmdliner.Term.t
