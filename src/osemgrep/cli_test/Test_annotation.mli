type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Those should *not* be reported (TN) *)
  | Ok
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
[@@deriving show]

type engine = OSS | Pro | Deep [@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map" *)
type t = kind * engine * Rule_ID.t [@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

val annotations : Fpath.t -> annotations
val group_positive_annotations : annotations -> (Rule_ID.t, linenb list) Assoc.t
val filter_todook : annotations -> linenb list -> linenb list
