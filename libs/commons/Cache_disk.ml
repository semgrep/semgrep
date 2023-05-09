(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
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
open Common

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Caching computation (e.g., parsed ASTs, network data, big result) on disk.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let get_value filename =
  let chan = open_in_bin filename in
  let x = input_value chan in
  (* <=> Marshal.from_channel  *)
  close_in chan;
  x

let write_value valu filename =
  let chan = open_out_bin filename in
  output_value chan valu;
  (* <=> Marshal.to_channel *)
  (* Marshal.to_channel chan valu [Marshal.Closures]; *)
  close_out chan

let filemtime file =
  if !Common.jsoo then failwith "JSOO: Common.filemtime"
  else (Unix.stat file).Unix.st_mtime

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Why a use_cache argument ? because sometimes want disable it but dont
 * want put the cache_computation funcall in comment, so just easier to
 * pass this extra option.
 *)
let cache_computation ?(use_cache = true) file ext_cache f =
  if not use_cache then f ()
  else if not (Sys.file_exists file) then (
    logger#error "WARNING: cache_computation: can't find %s" file;
    logger#error "defaulting to calling the function";
    f ())
  else
    let file_cache = file ^ ext_cache in
    if Sys.file_exists file_cache && filemtime file_cache >= filemtime file then (
      logger#info "using cache: %s" file_cache;
      get_value file_cache)
    else
      let res = f () in
      write_value res file_cache;
      res

let cache_computation_robust file ext_cache
    (need_no_changed_files, need_no_changed_variables) ext_depend f =
  if not (Sys.file_exists file) then failwith ("can't find: " ^ file);

  let file_cache = file ^ ext_cache in
  let dependencies_cache = file ^ ext_depend in

  let dependencies =
    (* could do md5sum too *)
    ( file :: need_no_changed_files |> List.map (fun f -> (f, filemtime f)),
      need_no_changed_variables )
  in

  if
    Sys.file_exists dependencies_cache
    && get_value dependencies_cache =*= dependencies
  then get_value file_cache
  else (
    pr2 ("cache computation recompute " ^ file);
    let res = f () in
    write_value dependencies dependencies_cache;
    write_value res file_cache;
    res)
