(* Small helpers around the Logs library.

   Here are the usage conventions for the Logs library level,
   augmented from https://erratique.ch/software/logs/doc/Logs/index.html#usage


      - App: unlike the other levels, this prints ordinary messages on
        stderr without any special formatting.

      - Error ('err'): error condition that prevent the program from running
        normally.

      - Warning ('warn'): suspicious condition that does not prevent the
        program from running normally but may eventually lead to an error
        condition.

      - Info: condition that allows the program user to get a better
        understanding of what the program is doing.
        Log messages at this level and above may not clutter up the log
        output not should they reduce performance significantly. If that's
        the case, log at the Debug level.
        Semgrep: activated with --verbose

      - Debug: condition that allows the program developer to get a
        better undersanding of what the program is doing.
        It may reduce the performance of the application or result in
        unreadable logs unless they're filtered. Use tags for filtering.
        Semgrep: activated with --debug
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

   'require_one_of_these_tags': if a list of tags is provided, at least one
   these tags must be set for a log instruction to be printed.

   'read_tags_from_env_var': specifies an environment variable
   from which a list of comma-separated tags will be read if the variable
   is set, in which case the list of tags will override any value set
   via 'require_one_of_these_tags'. This variable is "LOG_TAGS" by default.
   To disable it, set it to None. The following shell command shows how
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
val setup_logging :
  ?log_to_file:Fpath.t ->
  ?skip_libs:string list ->
  ?require_one_of_these_tags:string list ->
  ?read_tags_from_env_var:string option ->
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

   Note: these have nothing to do with the tags supported by the Logs library.
   See 'create_tag' below for those.
*)
val err_tag : ?tag:string -> unit -> string
val warn_tag : ?tag:string -> unit -> string
val success_tag : ?tag:string -> unit -> string

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
