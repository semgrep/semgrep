open Common

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Cache parsed generic ASTs between runs to speedup things.
 *
 * On some big repositories (e.g., Java elasticsearch), the speedup can be
 * very important (e.g., 10x).
 * Indeed:
 *  $ time semgrep-core -l java -rules ~/eqeq.yaml ~/elasticsearch/
 *  real: 1m22s
 *  $ time semgrep-core -use_parsing_cache ~/.semgrep/cache -l java -rules ~/eqeq.yaml ~/elasticsearch/
 *  real: 0.11s
 *
 * history: this used to be necessary because the Semgrep Python wrapper was
 * making multiple calls to semgrep-core and we didn't want to reparse again
 * again the same file for each rule. After migrating the processing of
 * a whole rule to semgrep-core, this was not anymore necessary, but
 * it is now useful to speedup semgrep when running semgrep
 * multiple times on the same reposotiry.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let filemtime file = (Unix.stat file).Unix.st_mtime

(* The function below is mostly a copy-paste of Common.cache_computation.
 * This function is slightly more flexible because we can put the cache file
 * anywhere thanks to the argument 'cache_file_of_file'.
 * We also try to be a bit more type-safe by using the version tag above.
 * TODO: merge in pfff/commons/Common.ml at some point
 *)
let cache_computation use_parsing_cache version_cur file cache_file_of_file f =
  if not use_parsing_cache then f ()
  else if not (Sys.file_exists file) then (
    logger#warning "cache_computation: can't find file %s " file;
    logger#warning "defaulting to calling the function";
    f ())
  else
    let file_cache = cache_file_of_file file in
    if Sys.file_exists file_cache && filemtime file_cache >= filemtime file then (
      logger#trace "using cache: %s" file_cache;
      let version, file2, res = Common2.get_value file_cache in
      if version <> version_cur then
        failwith (spf "Version mismatch! Clean the cache file %s" file_cache);
      if file <> file2 then
        failwith
          (spf "Not the same file! Md5sum collision! Clean the cache file %s"
             file_cache);

      res)
    else
      let res = f () in
      (try Common2.write_value (version_cur, file, res) file_cache with
      | Sys_error err ->
          (* We must ignore SIGXFSZ to get this exception, see
           * note "SIGXFSZ (file size limit exceeded)". *)
          logger#error "Could not write cache file for %s (%s): %s" file
            file_cache err;
          (* Make sure we don't leave corrupt cache files behind us. *)
          if Sys.file_exists file_cache then Sys.remove file_cache);
      res
  [@@profiling]

let cache_file_of_file parsing_cache_dir ext filename =
  let dir = parsing_cache_dir in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  (* hopefully there will be no collision *)
  let md5 = Digest.string filename in
  Filename.concat dir (spf "%s.%s" (Digest.to_hex md5) ext)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* A wrapper around Parse_target.parse_and_resolve_name *)
let parse_and_resolve_name ?(parsing_cache_dir = "") version lang file =
  let v =
    cache_computation (parsing_cache_dir <> "") version file
      (fun file ->
        (* we may use different parsers for the same file (e.g., in Python3 or
         * Python2 mode), so put the lang as part of the cache "dependency".
         * We also add version here so bumping the version will not
         * try to use the old cache file (which should generate an exception).
         *)
        let full_filename =
          spf "%s__%s__%s" file (Lang.to_string lang) version
        in
        cache_file_of_file parsing_cache_dir "ast_cache" full_filename)
      (fun () ->
        try
          logger#trace "parsing %s" file;
          (* finally calling the actual function *)
          let { Parse_target.ast; skipped_tokens; _ } =
            Parse_target.parse_and_resolve_name lang file
          in
          Left (ast, skipped_tokens)
          (* We stores also in the cache whether we had
           * an exception on this file, especially Timeout. We store the exn
           * in the cache, and below we reraise it after we got it back
           * from the cache.
           * TODO: right now we just capture Timeout, but we should capture
           * any exn. But doing so introduced some weird regressions in CI
           * so we focus on just Timeout for now.
           *)
        with
        | Match_rules.File_timeout as e -> Right e)
  in
  match v with
  | Left x -> x
  | Right exn -> raise exn
