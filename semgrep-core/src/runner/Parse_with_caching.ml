open Common
open Runner_common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* Cache parsed ASTs between runs. This is necessary because the python
 * wrapper makes multiple calls to semgrep-core. Previous was in main
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Caching *)
(*****************************************************************************)

let filemtime file = (Unix.stat file).Unix.st_mtime

(* The function below is mostly a copy-paste of Common.cache_computation.
 * This function is slightly more flexible because we can put the cache file
 * anywhere thanks to the argument 'cache_file_of_file'.
 * We also try to be a bit more type-safe by using the version tag above.
 * TODO: merge in pfff/commons/Common.ml at some point
 *)
let cache_computation ~parsing_cache_exists version_cur file cache_file_of_file
    f =
  if not parsing_cache_exists then f ()
  else if not (Sys.file_exists file) then (
    pr2 ("WARNING: cache_computation: can't find file " ^ file);
    pr2 "defaulting to calling the function";
    f ())
  else
    Common.profile_code "Main.cache_computation" (fun () ->
        let file_cache = cache_file_of_file file in
        if Sys.file_exists file_cache && filemtime file_cache >= filemtime file
        then (
          logger#trace "using cache: %s" file_cache;
          let version, file2, res = Common2.get_value file_cache in
          if version <> version_cur then
            failwith
              (spf "Version mismatch! Clean the cache file %s" file_cache);
          if file <> file2 then
            failwith
              (spf
                 "Not the same file! Md5sum collision! Clean the cache file %s"
                 file_cache);

          res)
        else
          let res = f () in
          (try Common2.write_value (version_cur, file, res) file_cache
           with Sys_error err ->
             (* We must ignore SIGXFSZ to get this exception, see
              * note "SIGXFSZ (file size limit exceeded)". *)
             logger#error "Could not write cache file for %s (%s): %s" file
               file_cache err;
             (* Make sure we don't leave corrupt cache files behind us. *)
             if Sys.file_exists file_cache then Sys.remove file_cache);
          res)

let cache_file_of_file parsing_cache filename =
  let dir = parsing_cache in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  (* hopefully there will be no collision *)
  let md5 = Digest.string filename in
  Filename.concat dir (spf "%s.ast_cache" (Digest.to_hex md5))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* It should really be just a call to Parse_target.parse_and_resolve...
 * but the semgrep python wrapper calls semgrep-core separately for each
 * rule, so we need to cache parsed AST to avoid extra work.
 *)
let parse_generic parsing_cache version lang file =
  if lang = Lang.C && Sys.file_exists !Flag_parsing_cpp.macros_h then
    Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;

  let v =
    cache_computation ~parsing_cache_exists:(parsing_cache <> "") version file
      (fun file ->
        (* we may use different parsers for the same file (e.g., in Python3 or
         * Python2 mode), so put the lang as part of the cache "dependency".
         * We also add ast_version here so bumping the version will not
         * try to use the old cache file (which should generate an exception).
         *)
        let full_filename =
          spf "%s__%s__%s" file (Lang.string_of_lang lang) version
        in
        cache_file_of_file parsing_cache full_filename)
      (fun () ->
        try
          logger#trace "parsing %s" file;
          (* finally calling the actual function *)
          let { Parse_target.ast; errors; _ } =
            Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
          in
          Left (ast, errors)
          (* This is a bit subtle, but we now store in the cache whether we had
           * an exception on this file, especially Timeout. Indeed, semgrep now calls
           * semgrep-core per rule, and if one file timeout during parsing, it would
           * timeout for each rule, but we don't want to wait each time 5sec for each
           * rule. So here we store the exn in the cache, and below we reraise it
           * after we got it back from the cache.
           *
           * TODO: right now we just capture Timeout, but we should capture any exn.
           *  However this introduces some weird regressions in CI so we focus on
           *  just Timeout for now.
           *)
        with Main_timeout _ as e -> Right e)
  in
  match v with
  | Left x -> x
  | Right exn -> raise exn
  [@@profiling]
