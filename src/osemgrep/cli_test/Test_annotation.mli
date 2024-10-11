type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
  (* Those should *not* be reported (TN) *)
  | Ok
[@@deriving show]

type engine = OSS | Pro | Deep [@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map"
 * but also "ruleid: prook: lang.ocaml.do-not-use-lisp-map".
 *
 * Note that 'ruleid:' implies 'proruleid:' and 'deepruleid:' so you don't need
 * to repeat those annotations. You usually need multiple kind/engine
 * prefix when one engine TP would be another engine FP (e.g., 'ruleid: prook:')
 *)
type t = {
  kind : kind;
  engine : engine;
  (* e.g., to deal with multiple annots as in 'ruleid: prook: x' *)
  others : (kind * engine) list;
  id : Rule_ID.t;
}
[@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

val annotations : Fpath.t -> annotations
val group_by_rule_id : annotations -> (Rule_ID.t, linenb list) Assoc.t
val filter_todook : annotations -> linenb list -> linenb list
