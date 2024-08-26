(* Heejong Lee
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

let default_depth = 2

(* This option is designed for the differential scan within the Pro
   engine. It involves two distinct configuration options: one to
   specify the set of changed files and the other to determine the
   depth of dependencies to be jointly analyzed. When operating in
   differential scan mode, the deep preprocessors, which include
   constant propagation and taint signature extraction, exclusively
   process the files contained within the changed input set, as
   configured by the `targets` parameter, along with their associated
   dependencies. This targeted approach substantially reduces the
   overall analysis time while maintaining a high level of
   accuracy. The dependencies are identified through a file-level
   dependency graph with a maximum distance of k edges, as configured
   by the `depth` parameter. Note that, when the --baseline-commit
   option is used, `targets` are automatically calculated based on the
   output from the `git diff` command between the baseline commit and
   the head commit. *)
type t =
  | BaseLine of Fpath.t list (* implies depth 0 *)
  | Depth of Fpath.t list (* starting point *) * int (* depth *)
  | WholeScan
[@@deriving show]

let map_targets f = function
  | BaseLine targets -> BaseLine (f targets)
  | Depth (targets, depth) -> Depth (f targets, depth)
  | WholeScan -> WholeScan

let apply_targets f = function
  | BaseLine targets -> Some (f targets)
  | Depth (targets, _) -> Some (f targets)
  | WholeScan -> None
