(* A few helpers for the Logs library.

   Here are the usage conventions for the Logs library "level"
   from https://erratique.ch/software/logs/doc/Logs/index.html#usage
   (slightly augmented).

   Attention: Any log message that can't be understood without context
   should be moved to the Debug level!

   - App: unlike the other levels, this prints ordinary messages without any
     special formatting.
     TODO: enable normal logging formatting and/or deprecate it
     and convert the existing code to use 'Std_msg.print' or 'Std_msg.eprint'.
     The issue is that right now, redirecting logs also redirects the verbose
     stderr output because it uses 'Logs.app'.

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
*)

(* Enable basic logging (level = Logs.Warning) so you can use Logging calls
 * even before a precise call to setup().
 *)
val setup_basic : unit -> unit

(* Setup the Logs library. This call is necessary before any logging
   calls, otherwise your log will not go anywhere (not even on stderr,
   unless you used setup_basic() below).

   'highlight_setting': whether the output should be formatted with color
   and font effects. It defaults to the current setting we have for stderr
   in Stderr_msg. This option can be useful when redirecting the logs to
   a file with the 'log_to_file' option.

   'require_one_of_these_tags': if a list of tags is provided, at least one
   of these tags must be set for a log instruction to be printed.

   'read_level_from_env_var': environment variables that override the
   'level' argument. The default is ["LOG_LEVEL"].
    See also 'read_tags_from_env_vars'.

   'read_srcs_from_env_vars': specifies environment variables
   from which a list of comma-separated (PCRE compliant) regexps will be
   read if the variable is set, in which case the list of regexps will be
   used to enable logging for third-party libraries whose src is matching
   one of the regexp.
   This variable is "LOG_SRCS" by default.

   'read_tags_from_env_vars': specifies environment variables
   from which a list of comma-separated tags will be read if the variable
   is set, in which case the list of tags will override any value set
   via 'require_one_of_these_tags'. This variable is "LOG_TAGS"
   by default.

   The following shell command shows how
   to make semgrep run at the debug level but only show lines tagged with
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
  ?highlight_setting:Std_msg.highlight_setting ->
  ?log_to_file:Fpath.t ->
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
*)
val create_tags : string list -> Logs.Tag.set

(*
   Log a string directly.
*)
val sdebug : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val sinfo : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val swarn : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit
val serr : ?src:Logs.src -> ?tags:Logs.Tag.set -> string -> unit

(*
   A function that masks the timestamps in log output so that we can compare
   logs from one run to another. To be used as:

     Testo.create ~checked_output:(Testo.stderr ()) ~mask_output:[Logs_.mask_time] ...

   This is crude. Beware false positives.
*)
val mask_time : string -> string

(*
   Mask all lines that look like log lines. This won't work for multiline
   logs:

     Testo.create
        ~checked_output:(Testo.stderr ())
        ~mask_output:[Logs_.mask_log_lines]
        ...

   This is crude. Beware false positives.
*)
val mask_log_lines : string -> string

(*
   Formatting utilities for common containers:
*)
val list : ('a -> string) -> 'a list -> string
val option : ('a -> string) -> 'a option -> string
val array : ('a -> string) -> 'a array -> string
