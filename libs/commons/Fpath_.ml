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

let of_relative_segments segs =
  match segs with
  | "" :: _
  | [] ->
      invalid_arg ("Fpath_.of_relative_segments: " ^ String.concat "/" segs)
  | seg :: segs -> List.fold_left Fpath.add_seg (Fpath.v seg) segs

let append_no_dot a b = if Fpath.is_current_dir a then b else Fpath.append a b

module Operators = struct
  let ( / ) = Fpath.( / )
  let ( // ) = Fpath.( // )
  let ( !! ) = ( !! )
end

let readable ~root path = Filename_.readable ~root:!!root !!path |> Fpath.v
