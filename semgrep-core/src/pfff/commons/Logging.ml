(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around the easy_logging library.
 *
 * My requirements for a logging library are:
 *  (1) different log levels (Debug, Info, Warning, Error)
 *  (2) easy logging calls with sprintf style formatting by default
 *  (3) ability to log on stdout/stderr or in a file
 *  (4) easy one-line logger creation
 *  (5) hierarchical loggers where you can enable/disable a whole set
 *      of loggers
 *  (6) configurable with an external log config file
 *
 * There are a few OCaml logging libraries, but they usually miss one or more
 * of the requirements above:
 *  - Logs: popular library according to OPAM metrics. However, Logs is heavily
 *    functorized (no (4)), and it requires to encapsulate your logging calls
 *    in a closure 'Log.info (fun m -> m "xxx")' (no (2)). It also lacks
 *    (5) and (6).
 *  - dolog: very basic logging library, very easy to use with (1), (2),
 *    but lacks especially (5) and (6)
 *  - Bolt: not updated since 2013
 *  - merlin/logging.ml: not available directly under OPAM and seems
 *    more limited than easy_logging
 *  - easy_logging: not really popular according to OPAM (no package depends
 *    on it), use OCaml objects, some documentation but no good examples,
 *    some unintuitive interfaces, some issues to compile without -42
 *    due to ambiguous constructors, 0.8 still not in OPAM, etc.
 *    But, it supports all the requirements if you know how to use it!
 * => I use easy_logging (actually I use easy_logging_yojson for (6))
 *
*)
open Easy_logging_yojson

type level = Easy_logging__.Logging_types.level
module Handlers = Easy_logging_yojson.Handlers

class type logger =
  object

    (** {3 Classic logging Methods}
        Each of these methods takes an optional [string list] of tags, then a set of parameters the way a printf function does. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

        Example :
        {[logger#warning "Unexpected value: %s" (to_string my_value)]}
    *)

    method flash : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
    method error : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
    method warning : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
    method info : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
    method trace : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
    method debug : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a


    (** {3 Lazy logging methods}
        Each of these methods takes a [string lazy_t] as an input (as well as the optional tags. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

        Example:
        {[logger#ldebug (lazy (heavy_calculation () ))]}
    *)

    method ldebug : ?tags:string list -> string lazy_t -> unit
    method ltrace : ?tags:string list -> string lazy_t -> unit
    method linfo : ?tags:string list -> string lazy_t -> unit
    method lwarning : ?tags:string list -> string lazy_t -> unit
    method lerror : ?tags:string list -> string lazy_t -> unit
    method lflash : ?tags:string list -> string lazy_t -> unit

    (** {3 String logging methods}
        Each of these methods takes a [string] as an input (as well as the optional tags).

        Example:
        {[logger#sdebug string_variable]}
    *)


    method sdebug : ?tags:string list -> string -> unit
    method strace : ?tags:string list -> string -> unit
    method sinfo : ?tags:string list -> string -> unit
    method swarning : ?tags:string list -> string -> unit
    method serror : ?tags:string list -> string -> unit
    method sflash : ?tags:string list -> string -> unit


    (** {3 Other methods} *)

    method name: string
    method internal_level : level

    (** Sets the log level of the logger instance. *)
    method set_level : level  -> unit

    (** Adds a handler to the logger instance. *)
    method add_handler : Handlers.t -> unit

    method get_handlers : Handlers.t list
    method set_handlers : Handlers.t list -> unit

    (** Will add a tag to each log message, resulting from the call of the supplied fonction (called each time a message is logged)*)
    method add_tag_generator: (unit -> string) -> unit

    (** Sets the propagate attribute, which decides whether messages passed to this logger are propagated to its ancestors' handlers. *)
    method set_propagate : bool -> unit

    (** {4 Internal methods} *)

    (** Returns the list of handlers of the logger, recursing with parents handlers
        if propagate is true*)
    method get_handlers_propagate : Handlers.t list

    (** Returns this logger level if it is not [None], else searches amongst ancestors for the first defined level; returns [NoLevel] if no level can be found. *)
    method effective_level : level

  end


(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let all_loggers = ref ([] : logger list)

let apply_to_all_loggers f =
  List.iter (fun logger -> f logger) !all_loggers

let get_loggers () = !all_loggers

let set_global_level level =
  apply_to_all_loggers (fun logger -> logger#set_level level)

let add_PID_tag () =
  let pid_string = Unix.getpid() |> string_of_int in
  apply_to_all_loggers (fun logger ->
    logger#add_tag_generator (fun () -> pid_string))

let get_logger xs : logger =
  let final_name = ("Main"::xs) |> String.concat "." in
  let logger = Logging.get_logger final_name in
  all_loggers := logger :: !all_loggers;
  logger

let load_config_file file =
  Logging.load_global_config_file file
