(lang dune 3.20)

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

;; Default attributes of opam packages
(source (github semgrep/semgrep))
(homepage "https://semgrep.dev")
(maintainers "Semgrep authors")
(authors "Semgrep authors")

;; Semgrep package
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

; note: these are build dependencies. Development-only deps are in 'dev/*.opam'.

; note: vendored libs are listed here so we pull in their opam dependencies
; (e.g., ocaml-tree-sitter-core -> tsort). They are pinned in
; semgrep.opam.template to the local version, so we don't have to worry about
; publishing to opam. If you want to add another, add it below and pin it there
;
; coupling: because of the way we need to statically link on macOS, if you add a
; library here, you'll probably need to update src/main/flags.sh if this library
; has some C stubs.
;
; coupling: if you add a dependency here and the nix workflow fails, you may
; need to manually trigger the "nix-weekly-update-flake" workflow
  (depends
    ; core deps
    ; see ppxlib comment for why we can't support 5.4 yet
    (ocaml (>= 5.3.0))
    ; needed for opam to understand that we are using a variant of the ocaml
    ; compiler (namily semgrep/ocaml).
    ocaml-variants
    ocaml-option-flambda
    ; parser generators
    (menhir (= 20230608))
    ; vendored in libs/ocaml-tree-sitter-core
    tree-sitter
    ; vendored in libs/semgrep-interfaces
    semgrep-interfaces
    ; standard libraries
    ;
    ; We currently don't use base directly, but there are 2 issues in past
    ; versions that require us to have a min version, since we DO use it
    ; indirectly:
    ; https://github.com/janestreet/base/issues/146
    ; https://github.com/janestreet/base/issues/178
    (base (>= v0.17.3))
    (fpath (>= 0.7.3))
    (bos (>= 0.2.1))
    (fileutils (>= 0.6.6))
    ; > 0.9 fails to compile w/tsan on linux
    ;
    ; Dune doesn't pass the -fsanitize=thread flag to compile C stubs when
    ; building for native, so we have a simple wrapper that forces the flag.
    ; See:
    ; https://github.com/ocaml/dune/issues/11923
    ;
    ; I suspect this is part of the issue with uring > 0.9 so let's wait until
    ; that is resolved until filing an issue on this
    ; alt: only choose an old uring if the tsan option is enabled?
    (uring (and (= :os linux) (= 0.9)))
    ; unit testing (note that testo is pinned in semgrep.opam.template)
    testo
    testo-lwt
    (alcotest (>= 1.9.1))
    (alcotest-lwt (>= 1.9.1))
    (calendar (>= 3.0.0))
    ; text and terminal formating
    (fmt (>= 0.11.0))
    (ocolor (>= 1.3.1))
    (ANSITerminal (>= 0.8.5))
    (terminal_size (>= 0.2.0))
    ; Unicode - used via python-str-repr
    (uucp (>= 17.0.0))
    (uutf (>= 1.0.4))
    ; logging
    (logs (>= 0.9.0))
    ; JSON/YAML/XML
    ; atdgen's runtime library must be compatible with the generated code in
    ; semgrep-interfaces:
    (atdgen-runtime (>= 3.0.0))  ; requires yojson 3
    (atdgen (>= 3.0.0))  ; no longer requires -j-std
    (yojson (>= 2.2.2))
    (yaml (>= 3.2.0))
    (xmlm (>= 1.4.0))
    (sarif (>= 0.3.1 ))
    ; CLI
    (cmdliner (>= 2.0.0))
    ; PPX
    (ppxlib (= 0.37.0))
    (ppx_blob (>= 0.9.0))
    (ppx_deriving (>= 6.0.3 ))
    (ppx_deriving_yojson (>= 3.9.1))
    ; https://github.com/janestreet/ppx_hash/issues/5
    (ppx_hash (>= v0.17.0))
    ; https://github.com/janestreet/ppx_inline_test/issues/52
    (ppx_inline_test (>= v0.17.0))
    (ppx_sexp_conv (>= v0.17.1))
    ; clean-room visitors runtime + upstream PPX (replaces upstream visitors package)
    visitors-cleanroom
    ; regexps
    (re ( >= 1.14.0))
    (conf-libpcre (= 2))
    (pcre (>= 8.0.5))
    ; we're currently pinning pcre2 to semgrep/pcre2.git
    pcre2
    ; obackward, pinned to semgrep/obackward.git
    obackward
    ; misc
    (ocamlgraph (>= 2.2.0))
    (parmap (>= 1.2.5))
    (semver (>= 0.2.1))
    (timedesc (>= 3.1.0)) ; used via git-wrapper
    ; 1.24.0 and above is incompatible with the ocaml VScode extention.
    (lsp (and (>= 1.22.0) (< 1.24.0)))
    (git (>= 3.18.0))
    ; tracing
    ; pinned to janestreet/memtrace.git
    memtrace
    ; profiling
    pyro-caml-instruments
    pyro-caml-ppx
    ; these are vendored due to https://github.com/imandra-ai/ocaml-opentelemetry/issues/113
    ; TODO once these changes are released, unvendor them
    opentelemetry
    opentelemetry-client-ocurl
    opentelemetry-client-cohttp-eio
    opentelemetry-logs
    ; trace 0.11 removed the Otrace.trace_id type that our pinned
    ; opentelemetry-trace still uses; pin until we bump the opentelemetry SHA
    (trace (< 0.11))
    ; needed by opentelemetry-client-ocurl but the latest, 0.9.2
    ; does not work under windows (see https://github.com/ygrek/ocurl/issues/77)
    ; hence the pin to 0.9.1
    (ocurl (= 0.9.1))
    ; ambient-context-lwt 0.2 introduces API changes, breaking ./libs/telemetry
    (ambient-context-lwt (and (>= 0.1.0) (< 0.2)))
    (conf-libcurl (= 2))
    ; web stuff
    (uri (>= 4.4.0))
    (uuidm (>= 0.9.10))
    (cohttp (>= 6.2.1))
    (cohttp-lwt-unix (>= 6.2.1))
    (cohttp-eio (>= 6.2.1))
    (tls-lwt (>= 2.0.3)) ; needed for TLS support in the cohttp HTTP client (only TLS 1.3 seems to work)
    (emile (>= 1.1))
    (digestif (>= 1.3.0))
    ; concurrency
    (lwt (>= 5.9.2))
    (lwt_ppx (>= 5.9.1))
    (conf-libev (and (<> :os win32) (= 4-13))) ; for lwt, to get better FD perf
    ;TODO: this is for secrets and should be in ../dune-project but doing so
    ; slows down a lot our CI because it pulls dependencies that seem
    ; to cause the removal and recompilation of many packages (e.g., tls) and
    ; make our GHA opam cache useless. We might need to install here instead
    ; packages that would avoid this effect.
    (git-unix (>= 3.18.0))
    ; Eio
    ; eio 1.3 appears broken on Windows https://github.com/semgrep/semgrep-proprietary/pull/4322
    (eio (= 1.2))
    (eio_main (= 1.2)); Picks a suitable backend (i.e eio_{posix, linux, windows})
    (tls-eio (>= 2.0.3))
  )
)

;; Other opam packages (sorted alphabetically)

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
    (ANSITerminal (>= "0.8.4"))
    (alcotest (>= "1.5.0"))
    (cmdliner (>= "2.0.0"))
    (logs (>= "0.7.0" ))
    (yojson (>= "1.7.0"))
    (re (>= "1.10.4"))
    (pcre (>= "7.5.0" ))
    ; we're currently "vendoring" pcre2 in libs/pcre2
    ; so no need to get opam to install it
    ;(pcre2 (>= "7.5.3" ))
    (ppxlib (>= "0.35.0"))
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
(package (name fast_json)
  (synopsis "Fast JSON parser emitting a Yojson.Safe-shaped tree with location info")
  (description "Experimental hand-rolled JSON parser that produces a tree with the same shape as Yojson.Safe.t but with Tok.t location annotations on every value and object key.")
)
(package (name networking)
  (synopsis "XXX")
  (description "XXX")
)
(package (name language_server)
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
(package (name parser_fga)
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
    (commons (>= "1.5.5"))
    (process_limits (>= "1.5.5"))
  )
)

(package
  (name telemetry)
  (version "1.5.5")
  (synopsis "Small library to help observe code")
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
    (commons (>= "1.5.5"))
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
  )
)
; needed because lots of dune language libs use tree-sitter-lang
; as a common base as in tree-sitter-lang.typescript
(package (name tree-sitter-lang)
  (synopsis "XXX")
  (description "XXX")
  (allow_empty)
)

(package (name collections)
  (synopsis "XXX")
  (description "XXX")
)
