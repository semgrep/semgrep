(*s: pfff/lang_GENERIC/analyze/CFG.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

type nodei = Ograph_extended.nodei

(* We include the entry point because the Dataflow engine uses this, and works using an abstract node type,
   which prevent it from searching for an entry node.
*)
type ('node, 'edge) t = {
  graph : ('node, 'edge) Ograph_extended.ograph_mutable;
  entry : nodei;
}

type ('node, 'edge) cfg = ('node, 'edge) t

(*e: pad/r2c copyright *)

(*e: pfff/lang_GENERIC/analyze/CFG.ml *)
