module Arg = Cmdliner.Arg
module Term = Cmdliner.Term

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This module is used mostly to decide between pysemgrep and osemgrep.
 * It could be used for different things later.
 *)

(*************************************************************************)
(* Types *)
(*************************************************************************)

type t =
  (* alt: we could use an option type also *)
  | Default
  (* Currently used to force the use of pysemgrep *)
  | Legacy
  (* Currently used to force the use of osemgrep *)
  | Experimental
  (* Leaving on the edge! This is used to specify whether to get rid of
   * pysemgrep behavior/limitations/errors or to keep how things were done
   * before (even if they were bad, but just to remain backward compatible).
   *)
  | Develop
[@@deriving show]

(*************************************************************************)
(* Maturity Cmdliner *)
(*************************************************************************)

(* We could remove some of the flags below and handle them manually in
 * cli/bin/semgrep or in ../cli/CLI.ml and remove them from Sys.argv
 * before going further in the individual cli_xxx/
 * (especially because they're mostly used for the pysemgrep/osemgrep
 * dispatch for now), but it's useful anyway to have them as explicit flags
 * so they show up in the man pages (e.g., in 'semgrep scan --help').
 *)

(* osemgrep-only:  *)
let o_experimental : bool Term.t =
  let info =
    Arg.info [ "experimental" ] ~doc:{|Enable experimental features.|}
  in
  Arg.value (Arg.flag info)

(* osemgrep-only: (well it is also supported by pysemgrep but
 * by handling (and filtering it) in cli/bin/semgrep *)
let o_legacy : bool Term.t =
  let info = Arg.info [ "legacy" ] ~doc:{|Prefer old (legacy) behavior.|} in
  Arg.value (Arg.flag info)

(* osemgrep-only: *)
let o_develop : bool Term.t =
  let info =
    (* alt: get rid  of the pysemgrep behaviors/limitations/errors *)
    Arg.info [ "develop" ] ~doc:{|Living on the edge.|}
  in
  Arg.value (Arg.flag info)

let o_maturity : t Term.t =
  let combine experimental legacy develop =
    match (experimental, legacy, develop) with
    | false, false, false -> Default
    | true, false, false -> Experimental
    | false, true, false -> Legacy
    | false, false, true -> Develop
    | _else_ ->
        Error.abort
          "mutually exclusive options --experimental/--legacy/--develop"
  in
  Term.(const combine $ o_experimental $ o_legacy $ o_develop)
