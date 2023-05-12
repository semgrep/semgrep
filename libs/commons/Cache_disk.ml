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
open File.Operators

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Caching computation (e.g., parsed ASTs, network data, big result) on disk.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* this record is marshalled on disk *)
type ('v, 'x) cached_value_on_disk = {
  (* Extra data useful for checking the cache (e.g., a version).
   * This is useful because the Marshall module is not type-safe;
   * if the OCaml data structures change between the moment the
   * value was cached and the moment it was retrieved, you can
   * get a segmentation fault after when starting to use the value.
   *)
  extra : 'x;
  (* the actual cached value *)
  value : 'v;
}

(* alt:  use a functor, but meh *)
type ('input, 'value, 'extra) cache_methods = {
  (* the containing directory will be automatically created if
   * not existing already *)
  cache_file_for_input : 'input -> Fpath.t;
  (* add some extra information with the value to enable certain checks
   * when getting back the value from the cache (e.g., versioning, timestamp).
   *)
  cache_extra_for_input : 'input -> 'extra;
  (* returns true if everything is fine and the cached value is valid.
   * alt: return a string option to explain the failure
   *)
  check_extra : 'extra -> bool;
  (* for debugging purpose, to add the input in the logs *)
  input_to_string : 'input -> string;
}

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

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* flexible cache *)
let (cache :
      ('input -> 'value) ->
      ('input, 'value, 'extra) cache_methods ->
      'input ->
      'value) =
 fun compute_value methods input ->
  let cache_file = methods.cache_file_for_input input in
  let input_str = methods.input_to_string input in
  let compute_and_save_in_cache () =
    (* TODO? assert Fpath.is_file cache_file ? *)
    let dir, _b = Fpath.split_base cache_file in
    (match Bos.OS.Dir.create ~path:true dir with
    (* old: we used to use Unix.mkdir, but we could race with another thread
     * to create the directory, leading to a crash if the
     * Unix.Unix_error (Unix.EEXIST, _, _) was unhandled.
     * Now Bos.OS.Dir.Create simply returns Ok false if the dir
     * already existed.
     *)
    | Ok _ -> ()
    | Error (`Msg err) -> failwith err);
    let value = compute_value input in
    let extra = methods.cache_extra_for_input input in
    let (v : ('value, 'extra) cached_value_on_disk) = { extra; value } in
    Logs.debug (fun m -> m "saving %s content in %s" input_str !!cache_file);
    (try write_value v !!cache_file with
    | Sys_error err ->
        (* We must Signal_ignore SIGXFSZ to get this exn; See the comment
         * on "SIGXFSZ (file size limit exceeded)" in Semgrep CLI.ml
         *)
        Logs.debug (fun m ->
            m "could not write cache file for %s (err = %s)" input_str err);
        (* Make sure we don't leave corrupt cache files behind us *)
        if Sys.file_exists !!cache_file then Sys.remove !!cache_file);
    value
  in
  (* TODO? in theory we could use the filemtime of cache_file and
   * if the input is a file, we could check whether the cache is
   * up-to-date without even reading it
   *)
  if Sys.file_exists !!cache_file then
    let (v : ('value, 'extra) cached_value_on_disk) =
      Common2.get_value !!cache_file
    in
    let { extra; value } = v in
    if methods.check_extra extra then (
      Logs.debug (fun m ->
          m "using the cache file %s for %s" !!cache_file input_str);
      value)
    else (
      Logs.debug (fun m ->
          m "invalid cache %s for %s (or too old)" !!cache_file input_str);

      compute_and_save_in_cache ())
  else compute_and_save_in_cache ()

(*****************************************************************************)
(* Deprecated *)
(*****************************************************************************)

(* Why a [use_cache] parameter? because sometimes we want disable it but we
 * don't want to put the cache_computation funcall in comment, so it's
 * just easier to pass this extra option.
 *)
let cache_computation ?(use_cache = true) file ext_cache f =
  if not use_cache then f ()
  else if not (Sys.file_exists file) then (
    logger#error "WARNING: cache_computation: can't find %s" file;
    logger#error "defaulting to calling the function";
    f ())
  else
    let file_cache = file ^ ext_cache in
    if
      Sys.file_exists file_cache
      && File.filemtime (Fpath.v file_cache) >= File.filemtime (Fpath.v file)
    then (
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
    ( file :: need_no_changed_files
      |> List.map (fun f -> (f, File.filemtime (Fpath.v f))),
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
