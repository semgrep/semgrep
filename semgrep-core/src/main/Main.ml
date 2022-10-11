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
(* A semantic grep.
 * See https://semgrep.dev/ for more information.
 *
 * Right now there is:
 *  - good support for: Python, Java, C#, Go, Ruby,
 *    Javascript (and JSX), Typescript (and TSX), JSON
 *  - partial support for: C, C++, PHP, OCaml, Kotlin, Scala, Rust, Lua,
 *    YAML, HTML, Vue, Bash, Docker
 *  - almost support for: R
 *
 * opti: git grep foo | xargs semgrep -e 'foo(...)'
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
(* This is the entry point for semgrep-core stand-alone program.
 *
 * Most of the code in this file has been moved to core_cli/Core_CLI.ml,
 * which contains the main command line parsing logic. The code
 * was moved to Cli_lib, a library, so it can be used both for
 * the stand-alone semgrep-core binary as well as the semgrep_bridge.so
 * shared library.
 *)

let () = Core_CLI.main Sys.argv
