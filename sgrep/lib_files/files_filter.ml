(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

module Glob = Dune_glob__Glob

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* see https://dune.readthedocs.io/en/stable/concepts.html#glob *)
type glob = Glob.t

type filters = {
  excludes: glob list;
  includes: glob list;
  exclude_dirs: glob list;
}

exception GlobSyntaxError of string

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)
let mk_filters ~excludes ~includes ~exclude_dirs =
 try 
  { excludes = excludes |> List.map Glob.of_string;
    includes = 
      if includes = []
      then [Glob.universal]
      else includes |> List.map Glob.of_string;
    exclude_dirs = exclude_dirs |> List.map Glob.of_string;
  } 
 with Invalid_argument s -> raise (GlobSyntaxError s)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let filter filters xs =
  xs |> List.filter (fun file ->
    let base = Filename.basename file in
    let dir = Filename.dirname file in
    (* todo? includes have priority over excludes? *)
    (filters.excludes |> List.for_all (fun glob -> not (Glob.test glob base)))
    &&
    (filters.includes |> List.exists (fun glob -> Glob.test glob base))
    &&
    (filters.exclude_dirs |> List.for_all 
            (fun glob -> not (Glob.test glob dir)))
    
 )
  

