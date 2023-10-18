(lang dune 3.7)
(name semgrep)
(using menhir 2.1)
; disable mangling of of workspace root, to preserve debugging information
; needed to support earlybird debugger
; See https://dune.readthedocs.io/en/stable/dune-files.html#map-workspace-root
(map_workspace_root false)

; Opam package declarations for public libraries and public executables
; defined in various dune files.
;
; Documentation for declaring opam packages:
; https://dune.readthedocs.io/en/stable/opam.html#opam-generation

; Generate the opam files used to install dependencies.
;
; 'dune build' (+ early interruption) is sufficient to trigger an update
; of the opam files.
; TODO: find the minimum command for this and add it as a pre-commit hook.
;
(generate_opam_files)

; Default attributes of opam packages
(source (github returntocorp/semgrep))
(homepage "https://semgrep.dev")
(maintainers "Semgrep authors")
(authors "Semgrep authors")

; Opam packages

(package
  (name aliengrep)
  (synopsis "XXX")
  (description "XXX")
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
    (easy_logging (>= "0.8.1" ))
    (easy_logging_yojson (>= "0.8.1" ))
    (yojson (>= "1.7.0"))
    (re (>= "1.10.4"))
    (pcre (>= "7.5.0" ))
    (ppxlib (>= "0.25.0"))
    (ppx_deriving (>= "5.2.1"))
    (ppx_hash (>= "v0.14.0" ))
    (parmap (>= "1.2.4"))
    (digestif (>= "1.0.0"))
    uri
    bos
  )
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
  (synopsis "XXX")
  (description "XXX")
)
(package (name glob)
  (synopsis "XXX")
  (description "XXX")
)
(package (name graph_code)
  (synopsis "XXX")
  (description "XXX")
)
(package (name networking)
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
(package (name parser_elixir)
  (synopsis "XXX")
  (description "XXX")
)
(package (name parser_go)
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

(package (name semgrep)
  (version 1.38.0)
  (synopsis "Like grep but for code: fast and syntax-aware semantic code pattern for many languages")
  (description "\
Semgrep is like grep but for searching patterns at the AST level.

For more information see https://semgrep.dev
"
  )

  (maintainers "Yoann Padioleau <pad@semgrep.com>")
  (authors "Yoann Padioleau <pad@semgrep.com>")
  (license "LGPL-2.1")

; These are build dependencies.
; Development-only dependencies are in 'dev/dev.opam'.

;coupling: for semgrep CI to be fast, we try to pre-install these packages as
; part of of the base docker image. When you add a new package or change a version
; here, please also update the list of packages there:
;
;   https://github.com/returntocorp/ocaml-layer/blob/master/common-config.sh
;
; or ask Martin to do so.
; You may also need to update the ocaml:alpine-xxx image used in ../Dockerfile.
;TODO: restore  "bisect_ppx" {>= "2.5.0"} once can use ppxlib 0.22.0
  (depends
    ; core deps
    (ocaml (>= 4.13.0))
    (dune (>= 2.7.0 ))
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
    ; unit testing
    alcotest
    alcotest-js
    ; text and terminal formating
    fmt
    ocolor
    ANSITerminal
    terminal_size
    notty
    ; logging
    (easy_logging ( = 0.8.1 ))
    (easy_logging_yojson ( = 0.8.1 ))
    logs
    ; JSON/YAML
    atdgen
    (yojson ( >= 2.0.0 ))
    yaml
    ; CLI
    cmdliner
    ; PPX
    ppxlib
    ppx_deriving
    ppx_hash
    ppx_inline_test
    (ppx_sexp_conv ( = v0.16.0))
    ppx_expect
    (visitors (= 20210608))
    ; regexps
    re
    pcre
    ; misc
    ocamlgraph
    parmap
    semver
    uri
    uuidm
    (lsp (= 1.15.1-5.0))
    ; web stuff
    uri
    cohttp-lwt-unix
    cohttp-lwt-jsoo
    tls-lwt ; needed for TLS support in the cohttp HTTP client (only TLS 1.3 seems to work)
    emile
    (digestif (>= 1.0.0))
    ; concurrency
    lwt
    lwt_ppx
    ; jsoo
    (js_of_ocaml (= 5.4.0))
    (js_of_ocaml-compiler (= 5.4.0))
    (js_of_ocaml-ppx (= 5.4.0))
    (js_of_ocaml-lwt (= 5.4.0))
    ctypes_stubs_js
    integers_stubs_js
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
  (license "LGPL-2.1")

  (depends
    alcotest
    atdgen
    ANSITerminal
    cmdliner
    (dune (>= 2.1))
  )
)

(package (name tree-sitter-lang)
  (synopsis "XXX")
  (description "XXX")
)
(package (name tree-sitter-to-generic)
  (synopsis "XXX")
  (description "XXX")
)
