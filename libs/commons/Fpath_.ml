(* Martin Jambon, Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extensions to Fpath.ml
 *
 * old: this used to be in File.ml as a submodule, but cleaner and
 * more consistent to use the Fpath_ scheme
 *)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let of_strings strings = List_.map Fpath.v strings
let to_strings paths = List_.map Fpath.to_string paths
let ( !! ) = Fpath.to_string

module Operators = struct
  let ( / ) = Fpath.( / )
  let ( // ) = Fpath.( // )
  let ( !! ) = ( !! )
end

let readable ~root path = Filename_.readable ~root:!!root !!path |> Fpath.v

(* Make the fpath "forget" whether the last element is a directory or not. *)
let to_string_no_trailing_slash x =
  let dir, file = Fpath.split_base x in
  Fpath.(append dir file) |> Fpath.to_string
