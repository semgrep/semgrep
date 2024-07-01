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
(* Entry point *)
(*****************************************************************************)

(* We currently use the same binary for semgrep-core and osemgrep (and now
 * also for semgrep for windows). See 'make core'.
 * We use the argv[0] trick below to decide whether the user wants the
 * semgrep-core or osemgrep (or semgrep) behavior.
 * LATER: when osemgrep is fully done we can just get rid of semgrep-core
 * and have a single binary called 'semgrep'.
 *)
let () =
  Cap.main (fun (caps : Cap.all_caps) ->
      let argv = CapSys.argv caps#argv in
      let argv0 =
        (* remove the possible ".exe" extension for Windows and ".bc" *)
        Fpath.v argv.(0) |> Fpath.base |> Fpath.rem_ext |> Fpath.to_string
      in
      match argv0 with
      (* osemgrep!! *)
      | "osemgrep"
      (* in the long term (and in the short term on windows) we want to ship
       * osemgrep as the default "semgrep" binary, without any
       * wrapper script such as cli/bin/semgrep around it.
       *)
      | "semgrep" ->
          let exit_code =
            match argv0 with
            | "semgrep" ->
                (* nosemgrep: no-pr2 *)
                UCommon.pr2
                  "!!!This is an experimental version of semgrep for \
                   Windows.!!!";
                (* nosemgrep: no-pr2 *)
                UCommon.pr2
                  "!!!Not all features may work. In case of problems, report \
                   here:!!!";
                (* nosemgrep: no-pr2 *)
                UCommon.pr2
                  "!!!https://github.com/semgrep/semgrep/issues/1330!!!";
                (* adding --experimemtal so we don't default back to pysemgrep *)
                CLI.main
                  (caps :> CLI.caps)
                  (Array.append argv [| "--experimental" |])
            | _else_ -> CLI.main (caps :> CLI.caps) argv
          in
          if not (Exit_code.Equal.ok exit_code) then
            Logs.info (fun m ->
                m "Error: %s\nExiting with error status %i: %s\n%!"
                  exit_code.description exit_code.code
                  (String.concat " " (Array.to_list argv)));
          CapStdlib.exit caps#exit exit_code.code
      (* legacy semgrep-core *)
      | _ -> Core_CLI.main caps argv)
