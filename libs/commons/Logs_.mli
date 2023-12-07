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

(* Enable basic logging (level = Logs.Warning) so that
   you can use Logging calls even before a precise call
   to setup_logging.
*)
val enable_logging : unit -> unit

(* list of Logs.src we don't want to enable logging for (third-party libs) *)
val default_skip_libs : string list

(* Setup the Logs library. This call is necessary before any logging
   calls, otherwise your log will not go anywhere (not even on stderr).
*)
val setup_logging :
  ?skip_libs:string list ->
  force_color:bool ->
  level:Logs.level option ->
  unit ->
  unit

(* See Testutil_mock.with_mocked_logs(). If this global is set,
 * setup_logging() above will not call Logs.set_reporter (and so
 * leave the mock logs_reporter in place).
 *)
val in_mock_context : bool ref

(* TODO:
   Logs.Error, Logs.Warning, and friends should apply the appropriate color
   and tag prefix (e.g. ERROR) to the message. For now, those functions can
   be used to prepend the colored tag manually before the log message.
*)
val err_tag : ?tag:string -> unit -> string
val warn_tag : ?tag:string -> unit -> string
val success_tag : ?tag:string -> unit -> string
