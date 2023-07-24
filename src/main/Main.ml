(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* A SEMantic GREP.
 * See https://semgrep.dev/ for more information.
 *
 * related:
 *  - Structural Search and Replace (SSR) in Jetbrains IDE
 *    http://www.jetbrains.com/idea/documentation/ssr.html
 *    http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *  - gogrep: https://github.com/mvdan/gogrep/
 *  - ruleguard: https://github.com/quasilyte/go-ruleguard
 *    (use gogrep internally)
 *  - phpgrep: https://github.com/quasilyte/phpgrep
 *    https://github.com/VKCOM/noverify/blob/master/docs/dynamic-rules.md
 *    https://speakerdeck.com/quasilyte/phpgrep-syntax-aware-code-search
 *  - rubocop pattern
 *    https://github.com/marcandre/rubocop/blob/master/manual/node_pattern.md
 *  - astpath, using XPATH on ASTs https://github.com/hchasestevens/astpath
 *  - ack http://beyondgrep.com/
 *  - cgrep http://awgn.github.io/cgrep/
 *  - hound https://codeascraft.com/2015/01/27/announcing-hound-a-lightning-fast-code-search-tool/
 *  - many grep-based linters (in Zulip, autodesk, bento, etc.)
 *
 * See also codequery for more structural queries.
 * See also old information at https://github.com/facebook/pfff/wiki/Sgrep.
 *)

(*****************************************************************************)
(* Exn pretty printers *)
(*****************************************************************************)

(* TODO: maybe we should update our dependencies to use a more recent
 * Base so we don't need those hacks and every module can independentely
 * register exn printers.
 * TODO? should we just call/reuse Core_CLI.override_janestreet_exn_printers
 * instead?
 *)
let register_stdlib_exception_printers () =
  (* Needs to take place after JaneStreet Base does its own registration.
   * See https://github.com/janestreet/base/issues/146 for more info
   *)
  Printexc.register_printer (function
    | Failure msg ->
        (* Avoid unnecessary quoting of the error message *)
        Some ("Failure: " ^ msg)
    | _else_ -> None)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* We currently use a single binary for semgrep-core and osemgrep. This
 * binary is called 'semgrep-core' and is accompanied by a symlink
 * to semgrep-core called 'osemgrep' (see 'make core').
 * We use the argv[0] trick below to decide whether the user wants the
 * semgrep-core or osemgrep behavior.
 * alt: we could have provided a separate binary for osemgrep, which
 * would be cleaner, but that would double the size of our Docker image.
 * LATER: when osemgrep is fully done we can just get rid of semgrep-core
 * and rename this binary to simply 'semgrep'.
 *)
let () =
  match Filename.basename Sys.argv.(0) with
  (* osemgrep!! *)
  | "osemgrep" ->
      (* TODO? What about the Core_CLI.register_exception_printers? *)
      register_stdlib_exception_printers ();
      let exit_code = CLI.main Sys.argv |> Exit_code.to_int in
      (* TODO: remove or make debug-only *)
      if exit_code <> 0 then
        Printf.eprintf "exiting with error status %i: %s\n%!" exit_code
          (String.concat " " (Array.to_list Sys.argv));
      exit exit_code
  (* legacy semgrep-core *)
  | _else_ -> Core_CLI.main Sys.argv
