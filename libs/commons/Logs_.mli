(* A few helpers for the Logs library.

   Here are the usage conventions for the Logs library "level"
   from https://erratique.ch/software/logs/doc/Logs/index.html#usage
   (slightly augmented).

   Attention: Any log message that can't be understood without context
   should be moved to the Debug level!

   - App: unlike the other levels, this prints ordinary messages without any
     special formatting. Note that this still prints the message on stderr,
     not stdout (use Console.ml and CapConsole.ml for printing on stdout).

   - Error ('err'): error condition that prevent the program from running
     normally.

   - Warning ('warn'): suspicious condition that does not prevent the
     program from running normally but may eventually lead to an error
     condition.

   - Info: condition that allows the program *user* to get a better
     understanding of what the program is doing.
     Log messages at this level and above may not clutter up the log
     output not should they reduce performance significantly. If that's
     the case, log at the Debug level.
     This is usually activated with a --verbose flag.

   - Debug: condition that allows the program *developer* to get a
     better understanding of what the program is doing.
     It may reduce the performance of the application or result in
     unreadable logs unless they're filtered. Use tags for filtering.
     This is usually activated with a --debug flag.

   See also Testutil_logs.mli if you need to mask logs in tests.
*)

(* Enable basic logging (default level = Logs.Warning) so you can use Logging
 * calls even before a precise call to setup().
 *)
val setup_basic : ?level:Logs.level option -> unit -> unit

(* Setup the Logs library. This call is necessary before any logging
   calls, otherwise your log will not go anywhere (not even on stderr,
   unless you used setup_basic() below).

   'highlight_setting': whether the output should be formatted with color
   and font effects. It defaults to the current setting we have for stderr
   in Console.ml. This option can be useful when redirecting the logs to
   a file with the 'log_to_file' option.

   'require_one_of_these_tags': if a list of tags is provided, at least one
   of these tags must be set for a log instruction to be printed.

   'read_level_from_env_var': environment variables that override the
   'level' argument. The default is ["LOG_LEVEL"].

   'read_srcs_from_env_vars': specifies environment variables
   from which a list of comma-separated pcre-compliant regexps will be
   read if the variable is set, in which case the list of regexps will be
   used to enable logging for third-party libraries whose src is matching
   one of the regexp.
   This variable is "LOG_SRCS" by default.

   'read_tags_from_env_vars': specifies environment variables
   from which a list of comma-separated tags will be read if the variable
   is set, in which case the list of tags will override any value set
   via 'require_one_of_these_tags'. This variable is "LOG_TAGS"
   by default.

   'additional_reporters' is a list of additonal loggers that will be called
   after the default logger. Useful for writing the logs to multiple
   destinations, e.g. an opentelemetry collector, a log file, etc.

   The following shell command shows for example how to
   make semgrep run at the debug level but only show lines tagged with
   'Match_rules' or 'Core_scan'.

     $ LOG_TAGS=Match_rules,Core_scan semgrep -e 'Obj.magic' -l ocaml --debug
     ...
     [00.45][INFO](Core_scan): Analyzing TCB/CapStdlib.ml
     [00.45][INFO](Core_scan): Analyzing tests/parsing/ocaml/attribute_type.ml
     [00.45][DEBUG](Match_rules): checking TCB/CapStdlib.ml with 1 rules
     [00.45][DEBUG](Match_rules): checking tests/parsing/ocaml/attribute_type.ml with 1 rules
     [00.45][DEBUG](Match_rules): looking for ["Pred",["Idents",["Obj","magic"]]] in TCB/CapStdlib.ml
     [00.45][DEBUG](Match_rules): looking for ["Pred",["Idents",["Obj","magic"]]] in tests/parsing/ocaml/attribute_type.ml
     [00.45][DEBUG](Match_rules): skipping rule - for tests/parsing/ocaml/attribute_type.ml
     [00.45][DEBUG](Match_rules): skipping rule - for TCB/CapStdlib.ml
     [00.45][DEBUG](Core_scan): done with tests/parsing/ocaml/attribute_type.ml
     [00.45][DEBUG](Core_scan): done with TCB/CapStdlib.ml
     [00.45][INFO](Core_scan): Analyzing tests/parsing/ocaml/basic.mli
     [00.45][INFO](Core_scan): Analyzing src/fixing/tests/Unit_autofix_printer.mli
     [00.45][DEBUG](Match_rules): checking tests/parsing/ocaml/basic.mli with 1 rules
     ...
*)

val setup :
  ?highlight_setting:Console.highlight_setting ->
  ?log_to_file:Fpath.t ->
  ?additional_reporters:Logs.reporter list ->
  ?require_one_of_these_tags:string list ->
  ?read_level_from_env_vars:string list ->
  ?read_srcs_from_env_vars:string list ->
  ?read_tags_from_env_vars:string list ->
  level:Logs.level option ->
  unit ->
  unit

(*
   String tags to be included in log messages for easy filtering.

   Filtering is done by setting 'require_one_of_these_tags' at setup time
   or by running grep on the full logs.

   Supported tag syntax: dot-separated alphanumeric identifiers.

   Suggested usage:

     let tags = Logs_.create_tag_set [__MODULE__; "autofix"]
     ...
     Logs.info (fun m -> m ~tags "Hello.");

   Note that tags are mostly deprecated. In practice you should prefer
   to use Logs src instead of tags.
*)
val create_tag : string -> string Logs.Tag.def
val create_tags : string list -> Logs.Tag.set
val create_tag_set : string Logs.Tag.def list -> Logs.Tag.set

(*
   Log a string directly.

   Those functions are useful because 'Logs.debug (fun m -> m "%s" str)' is a
   bit heavy. Note that the Logs library use closures by default for the logs
   so one can have heavy computation in the closure and this will
   not slow-down the app if the log is not shown. However, if
   the log is a constant string, there is no need for the closure
   hence the shorcuts below.
*)
val app : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val err : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val warn : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val info : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val debug : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit

(* [with_debug_trace ?src ?__FUNCTION__ ?__LOC__ ?pp_input f]
 * will use __FUNCTION__ as <str> and will
 * first log a "starting <str>" on the src, then run f,
 * then log a "finished <str>" and return the result of f.
 * If [f] is throwing an exception, this will also be logged as
 * "exception during <str>". If no src is given,
 * with_debug_trace will use the debug_trace_src below
 * and so you might need to run your program with
 * LOG_SRCS=debug_trace ...
 * Note, execeptions are always logged on the application src.
 * If pp_input is given then the function will be invoked when
 * logging at the start of the function and when an exception occurs.
 *)
val with_debug_trace :
  ?src:Logs.src ->
  __FUNCTION__:string ->
  ?pp_input:(unit -> string) ->
  (unit -> 'a) ->
  'a

val debug_trace_src : Logs.src

(*
   Formatting utilities for common containers:
*)
val list : ('a -> string) -> 'a list -> string
val option : ('a -> string) -> 'a option -> string
val array : ('a -> string) -> 'a array -> string
