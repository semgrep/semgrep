(* Small helpers around the Logs library.

   Here are the usage conventions for the Logs library level:

      - App, this level can be used for the standard output or console
        of an application. It should never be used by libraries.

      - Error, error condition that prevent the program from running
        normally.

      - Warning, suspicious condition that does not prevent the
        program from running normally but may eventually lead to an error
        condition.

      - Info, condition that allows the program user to get a better
        understanding of what the program is doing.

      - Debug, condition that allows the program developer to get a
        better undersanding of what the program is doing.

   src: https://erratique.ch/software/logs/doc/Logs/index.html#usage
*)

(* Setup the Logs library. This call is necessary before any logging
   calls, otherwise your log will not go anywhere (not even on stderr).
*)
val setup_logging : Logs.level option -> unit
