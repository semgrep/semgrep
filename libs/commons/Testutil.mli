(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

(*
   Log a function call. e.g.

     Testo.run file (fun () -> Parse_java.parse file)

   will log the file name instead of letting us guess which file was being
   parsed.
*)
val run : string -> (unit -> 'a) -> 'a

(*
   Extension of Testo.mask_temp_paths that also masks the physical path
   to the temporary folder in case the original is a symlink.

   This is useful for macOS where the standard temporary directory is
   usually not /tmp but a /var/folders/... which is itself a symlink.

   This is not done in Testo because it uses Unix.realpath which
   requires ocaml >= 4.13 and for now, Testo is meant to work starting with
   ocaml 4.08.
*)
val mask_temp_paths :
  ?depth:int option -> ?replace:(string -> string) -> unit -> string -> string

val skip_on_windows : string option
(** This value is used to mark tests which are skipped because they fail on
    Windows.

    By using this value for any tests skipped due to Windows incompatibility, we
    ensure that the tests skipped for this reason are tracked by the compiler.
    Finding the references to this value (or just removing the value) will reveal
    all tests skipped for this reason.
*)
