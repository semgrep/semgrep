(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.
*)
(* External expectations replace annotated finding lines for a target, rule,
   and engine. Paths in the JSON file are relative to its directory. *)
type t

val empty : t
(** No overrides. *)

val load : Fpath.t -> (t, [ `Msg of string ]) result
(** Read a JSON array of objects with path, rule_id, engine (OSS/Pro/Deep),
    and lines (positive, one-based finding lines). Reject invalid or duplicate
    entries, rather than silently ignoring a typo. File read, JSON decoding,
    and validation errors are returned with the expectations file's path. *)

val for_rules : t -> Rule_ID.t list -> t
(** Limit overrides to the rules currently being tested. *)

val for_file :
  t -> Test_annotation.engine -> Fpath.t -> (Rule_ID.t * int list) list
(** Return the complete replacement line lists for this target and engine.
    An empty line list explicitly expects no findings. *)
