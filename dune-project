(lang dune 3.7)

;; This file is used both to configure 'dune' as usual, and
;; to autogenerate the semgrep.opam for 'opam'.

;; classic dune-project settings
(name semgrep)
(using menhir 2.1)
; disable mangling of workspace root, to preserve debugging information
; needed to support the earlybird debugger
; See https://dune.readthedocs.io/en/stable/dune-files.html#map-workspace-root
(map_workspace_root false)

;; Opam package declarations for public libraries and public executables
;; defined in various dune files.
;; See the documentation for declaring opam packages for more info:
;; https://dune.readthedocs.io/en/stable/opam.html#opam-generation
;;
;; Type 'make semgrep.opam' to regenerate semgrep.opam
;; TODO: add it as a pre-commit hook?
(generate_opam_files)

;; set here so the semgrep package below can use it and we can easily bump it
(version 1.103.0)

;; Default attributes of opam packages
(source (github semgrep/semgrep))
(homepage "https://semgrep.dev")
(maintainers "Semgrep authors")
(authors "Semgrep authors")

;; Opam packages (sorted alphabetically)

(package
  (name aliengrep)
  (synopsis "Generic Semgrep-like pattern search backed by PCRE")
  (description "Generic Semgrep-like pattern search backed by PCRE")
)

(package
  (name ast_generic)
  (version 1.8.0)
  (synopsis "Abstract Syntax Tree (AST) supporting 31 programming languages")
  (description "\
This is a library defining a generic AST to factorize similar
analysis on different programming languages
(e.g., naming, semantic code highlighting, semgrep matching).

Right now this generic AST is mostly the factorized union of the ASTs of:
 - Python, Ruby, Lua, Julia, Elixir
 - Javascript, Typescript, Vue
 - PHP, Hack
 - Java, CSharp, Kotlin
 - C, C++
 - Go
 - Swift
 - OCaml, Scala, Rust
 - Clojure, Lisp, Scheme
 - R
 - Solidity
 - Bash, Docker
 - JSON, YAML, HCL, Jsonnet

This is a core library used by Semgrep but which can be of use in other
projects. This AST is also specified using ATD and so can be leveraged
from other programming languages such as Typescript, Java, Scala, and
the other programming languages supported by atdgen."
  )

  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")
  (depends
    (ocaml (>= 4.13.0))
    (dune (>= 3.2.0))
    (commons (>= 1.8.0))
    (lib_parsing (>= 1.5.5))
    (profiling (>= 1.5.5))
    (atdgen (>= 2.8.0))
  )
)

(package
  (name commons)

  (version 1.8.0)
  (synopsis "Yet another set of common utilities")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c.
"
  )
  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")

  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (ANSITerminal (>= "0.8.4"))
    (alcotest (>= "1.5.0"))
    (cmdliner (>= "1.1.1" ))
    (logs (>= "0.7.0" ))
    (yojson (>= "1.7.0"))
    (re (>= "1.10.4"))
    (pcre (>= "7.5.0" ))
    ; we're currently "vendoring" pcre2 in libs/pcre2
    ; so no need to get opam to install it
    ;(pcre2 (>= "7.5.3" ))
    (ppxlib (>= "0.25.0"))
    (ppx_deriving (>= "5.2.1"))
    (ppx_hash (>= "v0.14.0" ))
    (digestif (>= "1.0.0"))
    uri
    bos
  )
)

(package (name commons2)
  (synopsis "XXX")
  (description "XXX")
)

(package (name concurrency)
  (synopsis "XXX")
  (description "XXX")
)
(package (name gitignore)
  (synopsis "XXX")
  (description "XXX")
)
(package (name git_wrapper)
  (synopsis "Wrappers for the git CLI")
  (description "Utility functions for interacting with git on the command-line.")
)
(package (name glob)
  (synopsis "XXX")
  (description "XXX")
)
(package (name networking)
  (synopsis "XXX")
  (description "XXX")
)
(package (name murmur3)
  (synopsis "XXX")
  (description "XXX")
)

(package (name lwt_platform)
  (synopsis "XXX")
  (description "XXX")
)
(package
  (name lib_parsing)
  (version 1.14.0)
  (synopsis "Small library to help writing parsers")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c to help writing
parsers, especially ocamlyacc/menhir based parsers.
"
  )
  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")

  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
    (profiling (>= "1.5.5"))
    (fpath (>= "0.7.3"))
  )
)

(package (name lib_parsing_tree_sitter)
  (synopsis "XXX")
  (description "XXX")
)
(package (name ograph)
  (synopsis "XXX")
  (description "XXX")
)
(package (name ojsonnet)
  (synopsis "XXX")
  (description "XXX")
)
(package (name otarzan)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parallelism)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_bash)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_cairo)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_c)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_cpp)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_circom)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_csharp)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_dart)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_dockerfile)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_go)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_hack)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_html)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_java)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_javascript)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_jsonnet)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_json)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_julia)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_kotlin)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_lisp)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_lua)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_move_on_sui)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_move_on_aptos)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_ocaml)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_php)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_promql)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_protobuf)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_python)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_ql)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_regexp)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_r)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_ruby)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_rust)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_scala)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_solidity)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_swift)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_terraform)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_typescript)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_vue)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_yaml)
  (synopsis "XXX")
  (description "XXX")
)
(package (name pfff-lang_GENERIC-analyze)
  (synopsis "XXX")
  (description "XXX")
)
(package (name pfff-lang_GENERIC-naming)
  (synopsis "XXX")
  (description "XXX")
)
(package (name paths)
  (synopsis "XXX")
  (description "XXX")
)

(package
  (name process_limits)
  (version 1.5.5)
  (synopsis "Setting time and memory limits for your program")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c to control the
time and memory used by Semgrep.
"
  )
  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")

  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
  )
)

(package
  (name profiling)
  (version "1.5.5")
  (synopsis "Small library to help profile code")
  (description "\
This is a small library of utilities used by Semgrep and
a few other projects developed at r2c to monitor and profile
functions.
"
  )
  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")

  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
    (process_limits (>= "1.5.5"))
  )
)

(package
  (name tracing)
  (version "1.5.5")
  (synopsis "Small library to help trace code")
  (description "\
This is a small library of utilities used by Semgrep to
trace functions for the purpose of reporting them in
metrics
"
  )
  (maintainers "Emma Jin <emma@semgrep.com>")
  (authors "Emma Jin <emma@semgrep.com>")
  (license "LGPL-2.1-only")

  (depends
    (ocaml (>= "4.13.0"))
    (dune (>= "3.2.0" ))
    (commons (>= "1.5.5"))
  )
)

;; coupling: if you modify this package, type 'make semgrep.opam'
(package (name semgrep)
  (synopsis "Like grep but for code: fast and syntax-aware semantic code pattern for many languages")
  (description "\
Semgrep is like grep but for searching patterns at the AST level while
leveraging semantic information (e.g., types, control flow) for powerful
matching.

For more information see https://semgrep.dev
"
  )
  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1-only")

;note: these are build dependencies. Development-only deps are in 'dev/*.opam'.
;note: we don't include here as dependencies all the libraries
; that are "vendored" inside semgrep (e.g., commons, aliengrep, pcre2)
;coupling: because of the way we need to statically link on macOS, if
; you add a library here, you'll probably need to update src/main/flags.sh
; if this library has some C stubs.
;
; coupling: if you add a dependency here and the nix workflow fails, you may
; need to manually trigger the "nix-weekly-update-flake" workflow
;
;TODO: restore  "bisect_ppx" {>= "2.5.0"} once can use ppxlib 0.22.0
  (depends
    ; core deps
    (ocaml (>= 4.13.0))
    (dune (>= 2.7.0 ))
    base
    ; parser generators
    (menhir (= 20230608))
    ; standard libraries
    ; We don't use Base directly. If it's being used indirectly
    ; (is it, though?), we need a version that doesn't register
    ; rogue exception printers.
    ; See https://github.com/janestreet/base/issues/146
    (base (>= v0.15.1))
    fpath
    bos
    fileutils
    ; unit testing (note that testo is vendored in libs/ so not listed here)
    ; TODO? still need those alcotest-xx now that we use testo?
    alcotest
    alcotest-js
    alcotest-lwt
    calendar
    ; text and terminal formating
    fmt
    ocolor
    ANSITerminal
    terminal_size
    ; Unicode - used via python-str-repr
    uucp
    uutf
    ; logging
    logs
    ; JSON/YAML/XML
    atdgen
    (yojson ( >= 2.0.0 ))
    yaml
    xmlm
    (sarif ( >= 0.3.0 ))
    ; CLI
    cmdliner
    ; PPX
    ppxlib
    (ppx_deriving ( >= 6.0.3 ))
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    (ppx_sexp_conv ( = v0.16.0))
    ppx_expect
    (visitors (= 20210608))
    ; regexps
    re
    pcre
    ; we're currently "vendoring" pcre2 in libs/pcre2
    ; so no need to get opam to install it
    ;(pcre2 (>= "7.5.3" ))
    ; misc
    ocamlgraph
    parmap
    semver
    (timedesc (>= 2.0.0)) ; used via git-wrapper
    (lsp (= 1.15.1-5.0))
    git
    ; tracing
    trace
    ppx_trace
    (opentelemetry (>= 0.9))
    ; needed by opentelemetry-client-ocurl but the latest, 0.9.2
    ; does not work under windows (see https://github.com/ygrek/ocurl/issues/77)
    ; hence the pin to 0.9.1
    (ocurl (= 0.9.1))
    opentelemetry-client-ocurl
    ambient-context-lwt
    (conf-libcurl (= 1)) ; force older version of conf-libcurl to make windows work
    ; web stuff
    uri
    (uuidm (>= 0.9.9))
    (cohttp (= 6.0.0)) ; 6.0.0 for better error handling
    cohttp-lwt-unix
    cohttp-lwt-jsoo
    tls-lwt ; needed for TLS support in the cohttp HTTP client (only TLS 1.3 seems to work)
    emile
    (digestif (>= 1.0.0))
    (tls-mirage (= 0.17.3)) ; force older version of tls-mirage to make windows work
    ; concurrency
    lwt
    lwt_ppx
    (conf-libev (<> :os win32)) ; for lwt, to get better FD perf
    ; jsoo
    (js_of_ocaml (= 5.8.2))
    (js_of_ocaml-compiler (= 5.8.2))
    (js_of_ocaml-ppx (= 5.8.2))
    (js_of_ocaml-lwt (= 5.8.2))
    integers_stubs_js
    ;TODO: this is for secrets and should be in ../dune-project but doing so
    ; slows down a lot our CI because it pulls dependencies that seem
    ; to cause the removal and recompilation of many packages (e.g., tls) and
    ; make our GHA opam cache useless. We might need to install here instead
    ; packages that would avoid this effect.
    git-unix
  )
)

(package
  (name spacegrep)
  (synopsis "Universal program parser and matcher")
  (description "\
Uses indentation and standard braces to determine a crude parse tree
"
  )
  (maintainers "martin@semgrep.com")
  (authors "Semgrep authors")
  (license "LGPL-2.1-only")

  (depends
    alcotest
    atdgen
    ANSITerminal
    cmdliner
    (dune (>= 2.1))
  )
)
; needed because lots of dune language libs use tree-sitter-lang
; as a common base as in tree-sitter-lang.typescript
(package (name tree-sitter-lang)
  (synopsis "XXX")
  (description "XXX")
  (allow_empty)
)

(package (name TCB)
  (synopsis "XXX")
  (description "XXX")
)
