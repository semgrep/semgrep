(*
   Test Osemgrep's target selection on real git (or other) repos.
*)

val tests : CLI.caps -> Eio_unix.Stdenv.base -> Testo.t list
