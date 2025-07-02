(* we need CLI.caps here because we call directly CLI.main in some tests *)
val tests : CLI.caps -> Eio_unix.Stdenv.base -> Testo.t list
