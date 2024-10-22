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
open Eq.Operators

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
let current_dir = Fpath.v "."

(* TODO: get rid of! *)
let fake_file : Fpath.t = Fpath.v "_NOT_A_FILE_"
let is_fake_file (f : Fpath.t) : bool = Fpath.equal f fake_file
let to_yojson file = `String (Fpath.to_string file)

let of_yojson = function
  | `String path ->
      Fpath.of_string path
      |> Result.map_error (fun (`Msg error_str) -> error_str)
  | other ->
      Error
        (Printf.sprintf "Expected `String, received %s"
           (Yojson.Safe.pretty_to_string other))

let exts (p : Fpath.t) : string list =
  (* ex: ".tar.gz" *)
  let ext = Fpath.get_ext ~multi:true p in
  String.split_on_char '.' ext |> List_.exclude (fun s -> s = "")

let split_ext ?multi (p : Fpath.t) : Fpath.t * string =
  (Fpath.rem_ext ?multi p, Fpath.get_ext ?multi p)

let () =
  Testo.test "Fpath_.exts" (fun () ->
      assert (exts (Fpath.v "foo.tar.gz") =*= [ "tar"; "gz" ]));
  Testo.test "Fpath_.split_ext" (fun () ->
      assert (
        split_ext ~multi:true (Fpath.v "a/foo.tar.gz")
        =*= (Fpath.v "a/foo", ".tar.gz")));
  ()

(*****************************************************************************)
(* Alias *)
(*****************************************************************************)
type t = Fpath.t [@@deriving show, eq, ord]

let t_of_sexp (sexp : Sexplib.Sexp.t) : t =
  Fpath.v (Sexplib.Std.string_of_sexp sexp)

let sexp_of_t (t : t) : Sexplib.Sexp.t = Sexplib.Std.sexp_of_string !!t
