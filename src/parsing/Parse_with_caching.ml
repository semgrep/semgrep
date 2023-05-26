(* Yoann Padioleau
 *
 * Copyright (C) 2022-2023 r2c
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
open Common
open File.Operators

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Cache parsed generic ASTs between two runs of Semgrep to speedup things.
 *
 * On some big repositories (e.g., elasticsearch), the speedup can be
 * very important (e.g., 10x). Indeed:
 *  $ time semgrep-core -l java -rules ~/eqeq.yaml ~/elasticsearch/
 *  real: 1m22s
 *  $ time semgrep-core -use_parsing_cache ~/.semgrep/cache -l java -rules ~/eqeq.yaml ~/elasticsearch/
 *  real: 0.11s
 *
 * history: the cache used to be necessary because pysemgrep was
 * making multiple calls to semgrep-core and we didn't want to reparse again
 * the same file for each rule. After migrating the processing of all
 * the rules at once to semgrep-core, this was not needed anymore. However,
 * it is now useful again to speedup semgrep when running semgrep
 * multiple times on the same repository. This can also be useful
 * for semgrep LSP.
 *
 * This file could also be under optimizing/, but that would require to add
 * semgrep_parsing as a dependency in optimizing/dune.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* We store the parsed AST and the skipped tokens in the cache. If parsing
 * was resulting in an exception, we store the exn instead.
 * The extra data is to double check the cached AST correspond
 * to the file, to check it has the right AST_generic.version, and
 * to check if the cache is stale.
 * We use Rpath below instead of Fpath which reduces cache miss if
 * some people are running Semgrep from different places or through
 * symlinks.
 *)
type ast_cached_value =
  ( (AST_generic.program * Tok.location list, Exception.t) Common.either,
    string (* AST_generic.version *)
    * Lang.t
    * Rpath.t (* original file *)
    * float (* mtime of the original file *) )
  Cache_disk.cached_value_on_disk

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ast_or_exn_of_file (lang, file) =
  logger#trace "parsing %s" !!file;
  try
    (* finally calling the actual function *)
    let { Parsing_result2.ast; skipped_tokens; _ } =
      Parse_target.parse_and_resolve_name lang !!file
    in
    Left (ast, skipped_tokens)
  with
  (* We store also in the cache whether we had an exception on this file,
   * especially Timeout. This avoids trying to parse the same file
   * again and again (which actually timeout, so this saves lots of time).
   * Then, at the end of parse_and_resolve_name() we
   * reraise the exn if we got it from the cache.
   * TODO: right now we just capture Timeout, but we should capture
   * any exn. But doing so introduced some weird regressions in CI
   * so we focus on just Timeout for now.
   *)
  | Time_limit.Timeout _ as exn ->
      let e = Exception.catch exn in
      Right e

(* Cache_disk methods reused in a few places *)
let cache_extra_for_input version (lang, file) =
  (version, lang, Rpath.of_fpath file, File.filemtime file)

(* this is used for semgrep-core -generate_ast_binary done for Snowflake *)
let binary_suffix : Fpath.ext = ".ast.binary"

(* coupling: with binary_suffix definition above *)
let is_binary_ast_filename file =
  (* TODO: use Fpath.ext functions *)
  !!file =~ ".*\\.ast\\.binary$"

(* for semgrep-core -generate_ast_binary *)
let ast_cached_value_of_file version lang (file : Fpath.t) : ast_cached_value =
  let value = ast_or_exn_of_file (lang, file) in
  let extra = cache_extra_for_input version (lang, file) in
  { Cache_disk.extra; value }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* A wrapper around Parse_target.parse_and_resolve_name *)
let parse_and_resolve_name ?(parsing_cache_dir = None) version lang
    (file : Fpath.t) =
  if is_binary_ast_filename file then (
    logger#info "%s is already an AST binary file, unmarshalling its value"
      !!file;
    let (v : ast_cached_value) = Common2.get_value !!file in
    let { Cache_disk.value = either; extra = version2, _lang2, _file2, _mtime2 }
        =
      v
    in
    (* coupling: similar to check_extra() method below, but we're not
     * checking it's the same file here
     *)
    if version <> version2 then
      failwith (spf "Version mismatch! Clean the cache file %s" !!file);
    match either with
    | Left v -> v
    | Right exn -> Exception.reraise exn)
  else
    match parsing_cache_dir with
    | None ->
        (* simply calling the wrapped function *)
        let { Parsing_result2.ast; skipped_tokens; _ } =
          Parse_target.parse_and_resolve_name lang !!file
        in
        (ast, skipped_tokens)
    | Some parsing_cache_dir -> (
        let cache_methods =
          {
            Cache_disk.cache_file_for_input =
              (fun (lang, file) ->
                (* canonicalize to reduce cache misses *)
                let file = Rpath.of_fpath file in
                (* we may use different parsers for the same file
                 * (e.g., in Python3 or Python2 mode), so we put the lang as
                 * part of the cache "dependency".
                 * We also add version here so bumping the version will not
                 * try to use the old cache file.
                 * TODO? do we need version here since anyway we would
                 * generate an exn when reading the cache and regenerate it.
                 *)
                let str =
                  spf "%s__%s__%s" (Rpath.to_string file) (Lang.to_string lang)
                    version
                in
                (* Better to obfuscate the cache files, like in Unison, to
                 * discourage people to play with it. Also simple way to escape
                 * special chars in a file path.
                 * hopefully there will be no collision
                 *)
                let md5_hex = Digest.string str |> Digest.to_hex in
                (* TODO: create parsing_cache_dir in Cache_disk.ml, factorize *)
                parsing_cache_dir // Fpath.(v md5_hex + binary_suffix));
            cache_extra_for_input = cache_extra_for_input version;
            check_extra =
              (fun (version2, lang2, file2, mtime2) ->
                (* TODO: better logging for the different reason the cache is
                 * invalid?
                 * - "Version mismatch! Clean the cache file"
                 * - "Not the same file! Md5sum collision! Clean the cache file"
                 *)
                version = version2
                && Rpath.equal (Rpath.of_fpath file) file2
                && Lang.equal lang lang2
                && File.filemtime file =*= mtime2);
            input_to_string =
              (fun (lang, file) ->
                spf "Lang = %s, target = %s" (Lang.show lang) !!file);
          }
        in
        let either =
          Cache_disk.cache ast_or_exn_of_file cache_methods (lang, file)
        in
        match either with
        | Left x -> x
        | Right exn -> Exception.reraise exn)
  [@@profiling]
