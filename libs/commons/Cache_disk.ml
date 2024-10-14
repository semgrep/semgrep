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
open Fpath_.Operators
module Log = Log_commons.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Caching computation (e.g., parsed ASTs, network data, big result) on disk *)

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

(* input_value <=> Marshal.from_channel  *)
let get_value filename =
  UFile.Legacy.with_open_infile filename UStdlib.input_value

(* output_value <=> Marshal.to_channel chan valu [Marshal.Closures] *)
let write_value valu filename =
  UFile.Legacy.with_open_outfile filename (fun (_pr, chan) ->
      UStdlib.output_value chan valu)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (cache_monad :
      ('value -> 'wrapped_value) ->
      ('wrapped_value -> ('value -> 'wrapped_value) -> 'wrapped_value) ->
      ('input -> 'wrapped_value) ->
      ('input, 'value, 'extra) cache_methods ->
      'input ->
      'wrapped_value) =
 fun return bind compute_value methods input ->
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
    bind (compute_value input) (fun value ->
        let extra = methods.cache_extra_for_input input in
        let (v : ('value, 'extra) cached_value_on_disk) = { extra; value } in
        Log.info (fun m -> m "saving %s content in %s" input_str !!cache_file);
        (try write_value v !!cache_file with
        | Sys_error err ->
            (* We must Signal_ignore SIGXFSZ to get this exn; See the comment
             * on "SIGXFSZ (file size limit exceeded)" in Semgrep CLI.ml
             *)
            Log.warn (fun m ->
                m "could not write cache file for %s (err = %s)" input_str err);
            (* Make sure we don't leave corrupt cache files behind us *)
            if USys.file_exists !!cache_file then USys.remove !!cache_file);
        return value)
  in
  (* TODO? in theory we could use the filemtime of cache_file and
   * if the input is a file, we could check whether the cache is
   * up-to-date without even reading it
   *)
  if USys.file_exists !!cache_file then
    let (v : ('value, 'extra) cached_value_on_disk) = get_value !!cache_file in
    let { extra; value } = v in
    if methods.check_extra extra then (
      Log.info (fun m ->
          m "using the cache file %s for %s" !!cache_file input_str);
      return value)
    else (
      Log.warn (fun m ->
          m "invalid cache %s for %s (or too old)" !!cache_file input_str);

      compute_and_save_in_cache ())
  else compute_and_save_in_cache ()

(* flexible cache *)
let cache compute_value methods input =
  cache_monad Fun.id ( |> ) compute_value methods input

let cache_lwt compute_value methods input =
  cache_monad Lwt.return Lwt.bind compute_value methods input

(*****************************************************************************)
(* Deprecated *)
(*****************************************************************************)

(* Why a [use_cache] parameter? because sometimes we want disable it but we
 * don't want to put the cache_computation funcall in comment, so it's
 * just easier to pass this extra option.
 *)
let cache_computation ?(use_cache = true) file ext_cache f =
  if not use_cache then f ()
  else if not (USys.file_exists file) then (
    Log.warn (fun m -> m "cache_computation: can't find %s" file);
    Log.warn (fun m -> m "defaulting to calling the function");
    f ())
  else
    let file_cache = file ^ ext_cache in
    if
      USys.file_exists file_cache
      && UFile.filemtime (Fpath.v file_cache) >= UFile.filemtime (Fpath.v file)
    then (
      Log.info (fun m -> m "using cache: %s" file_cache);
      get_value file_cache)
    else
      let res = f () in
      write_value res file_cache;
      res

let cache_computation_robust file ext_cache
    (need_no_changed_files, need_no_changed_variables) ext_depend f =
  if not (USys.file_exists file) then failwith ("can't find: " ^ file);

  let file_cache = file ^ ext_cache in
  let dependencies_cache = file ^ ext_depend in

  let dependencies =
    (* could do md5sum too *)
    ( file :: need_no_changed_files
      |> List_.map (fun f -> (f, UFile.filemtime (Fpath.v f))),
      need_no_changed_variables )
  in

  if
    USys.file_exists dependencies_cache
    && get_value dependencies_cache =*= dependencies
  then get_value file_cache
  else (
    Log.info (fun m -> m "cache computation recompute %s" file);
    let res = f () in
    write_value dependencies dependencies_cache;
    write_value res file_cache;
    res)
