(*
 * The author disclaims copyright to this source file.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A SEMantic GREP.
 * See https://semgrep.dev/ for more information.
 *
 * This is the entry point of the semgrep-core program used internally
 * by pysemgrep, the entry point of osemgrep, and currently also the entry point
 * of semgrep for windows. See also the ../../cli/bin/semgrep python wrapper
 * script which is currently the real entry point of semgrep.
 * LATER: when osemgrep is fully done we can just get rid of semgrep-core,
 * osemgrep, the wrapper script, and have a single binary called 'semgrep'.
 *
 * Related work using code patterns (from oldest to newest):
 *  - Structural Search and Replace (SSR) in Jetbrains IDEs
 *    http://www.jetbrains.com/idea/documentation/ssr.html
 *    http://tv.jetbrains.net/videocontent/intellij-idea-static-analysis-custom-rules-with-structural-search-replace
 *  - Coccinelle (the precursor of Semgrep) for C
 *    https://coccinelle.gitlabpages.inria.fr/website/
 *  - Sgrep (Syntactical GREP, another precursor of Semgrep) for PHP
 *    https://github.com/facebook/pfff/wiki/Sgrep
 *  - gogrep and ruleguard for Go
 *    https://github.com/mvdan/gogrep/
 *    https://github.com/quasilyte/go-ruleguard
 *  - phpgrep for PHP
 *    https://github.com/quasilyte/phpgrep
 *    https://speakerdeck.com/quasilyte/phpgrep-syntax-aware-code-search
 *    https://github.com/VKCOM/noverify/blob/master/docs/dynamic-rules.md
 *  - cgrep for C
 *    http://awgn.github.io/cgrep/
 *  - Comby for many languages
 *    https://comby.dev/
 *  - Weggli for C/C++ (inspired by Semgrep)
 *    https://github.com/weggli-rs/weggli
 *  - ASTgrep (inspired by Semgrep)
 *    https://ast-grep.github.io/
 *
 * related AST search tools:
 *  - "ASTLOG: A Language for Examining Abstract Syntax Trees"
 *     https://www.usenix.org/legacy/publications/library/proceedings/dsl97/full_papers/crew/crew.pdf
 *  - rubocop pattern
 *    https://docs.rubocop.org/rubocop-ast/node_pattern.html
 *  - astpath, using XPATH on ASTs
 *    https://github.com/hchasestevens/astpath
 *
 * related code search and indexing tools:
 *  - "Tutorial on the C Information Abstraction System"
 *     https://www2.eecs.berkeley.edu/Pubs/TechRpts/1987/CSD-87-327.pdf
 *  - "JQuery: Finding your way through thangled code"
 *     https://www.cs.ubc.ca/labs/spl/projects/jquery/papers.htm
 *  - Codequery (from Pfff too)
 *    https://github.com/facebookarchive/pfff/wiki/CodeQuery
 *  - CodeQL (known previously as Semmle and before CodeQuest)
 *    https://codeql.github.com/
 *  - Kythe (sucessor of Grok by Steve Yegge at Google)
 *    https://kythe.io/
 *  - LSP the Language Server protocol
 *    https://langserver.org/
 *  - SCIP and LSIF by sourcegraph
 *    https://github.com/sourcegraph/scip
 *  - Glean
 *    https://glean.software/
 *  - many more (e.g., PQL)
 *
 * related grep-like tools:
 *  - ack
 *    http://beyondgrep.com/
 *  - ripgrep
 *    https://github.com/BurntSushi/ripgrep
 *  - hound https://codeascraft.com/2015/01/27/announcing-hound-a-lightning-fast-code-search-tool/
 *  - many grep-based linters (in Zulip, autodesk, bento, etc.)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let eprint_experimental_windows (cap : Cap.Console.stderr) : unit =
  let epr = CapConsole.eprint cap in
  epr "!!!This is an experimental version of semgrep for Windows.!!!";
  epr "!!!Not all features may work. In case of problems, report here:!!!";
  epr "!!!https://github.com/semgrep/semgrep/issues/1330!!!";
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* We currently use the same binary for semgrep-core and osemgrep (and now
 * also for semgrep for windows). See 'make core' and './dune' install section.
 * We use the argv[0] trick below to decide whether the user wants the
 * semgrep-core or osemgrep (or semgrep) behavior.
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
                eprint_experimental_windows caps#stderr;
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
      | _else_ -> Core_CLI.main caps argv)
