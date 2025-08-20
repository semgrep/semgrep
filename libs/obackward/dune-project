(lang dune 2.7)
(name obackward)

(generate_opam_files true)

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(package
  (name obackward)
  (synopsis "Bindings to backward-cpp")
  (description "obackward provides bindings to backward-cpp, a library for catching and pretty printing segfaults")
  (depends
    (ocaml (>= 5))
    dune-configurator
    (conf-unwind (<> :os win32))
    (conf-zstd (<> :os win32)) ; for libdwarf
    (conf-libdw (= :os linux))
    (conf-libbz2 (= :os linux)) ; for libdw
  )
)
