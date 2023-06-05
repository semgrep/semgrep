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
(* Semgrep-core *)
(*****************************************************************************)

let semgrep_core () = Core_CLI.main Sys.argv

(*****************************************************************************)
(* Osemgrep *)
(*****************************************************************************)
(* Translated from __main__.py *)

let register_stdlib_exception_printers () =
  (* Needs to take place after JaneStreet Base does its own registration.
     https://github.com/janestreet/base/issues/146 *)
  Printexc.register_printer (function
    | Failure msg ->
        (* Avoid unnecessary quoting of the error message *)
        Some ("Failure: " ^ msg)
    | __ -> None)

let osemgrep () =
  register_stdlib_exception_printers ();
  let exit_code = CLI.main Sys.argv |> Exit_code.to_int in
  (* TODO: remove or make debug-only *)
  if exit_code <> 0 then
    Printf.eprintf "exiting with error status %i: %s\n%!" exit_code
      (String.concat " " (Array.to_list Sys.argv));
  exit exit_code

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let () =
  match Filename.basename Sys.argv.(0) with
  | "osemgrep" -> osemgrep ()
  | _else_ -> semgrep_core ()
