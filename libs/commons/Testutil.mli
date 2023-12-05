(*
   Various utilities used for testing and are not considered an extension
   of Alcotest (e.g. because it depends on Semgrep-specific libraries).
*)

(*
   Log a function call. e.g.

     Alcotest_ext.run file (fun () -> Parse_java.parse file)

   will log the file name instead of letting us guess which file was being
   parsed.
*)
val run : string -> (unit -> 'a) -> 'a
