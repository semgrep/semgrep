
type level = Easy_logging__.Logging_types.level

(* Mostly a copy paste of easy_logging.mli inlined here for
 * quicker reference *)
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
    method add_handler : Easy_logging_yojson.Handlers.t -> unit

    method get_handlers : Easy_logging_yojson.Handlers.t list
    method set_handlers : Easy_logging_yojson.Handlers.t list -> unit

    (** Will add a tag to each log message, resulting from the call of the supplied fonction (called each time a message is logged)*)
    method add_tag_generator: (unit -> string) -> unit

    (** Sets the propagate attribute, which decides whether messages passed to this logger are propagated to its ancestors' handlers. *)
    method set_propagate : bool -> unit

    (** {4 Internal methods} *)

    (** Returns the list of handlers of the logger, recursing with parents handlers
        if propagate is true*)
    method get_handlers_propagate : Easy_logging_yojson.Handlers.t list

    (** Returns this logger level if it is not [None], else searches amongst ancestors for the first defined level; returns [NoLevel] if no level can be found. *)
    method effective_level : level

  end

(* Return the logger of that given name or create one if necessary.
   Typically, we create one logger per module using the name of the module
   as the single component:

     let logger = Logging.get_logger [ __MODULE__ ]

   This results in the name "Main."<module>.
   TODO: why do this concatenation and not take a plain string like the
   original Easy_logging API?
*)
val get_logger : string list -> logger

(*
   Get all the loggers created and registered with 'get_logger'.
   This is useful to check which loggers and tags exist.
*)
val get_loggers : unit -> logger list

(*
   Iterate over all the registered loggers.
*)
val apply_to_all_loggers : (logger -> unit) -> unit

(*
   Set/reset the log level associated with each logger. This is normally
   done just before fine-tuning with 'load_config_file'.
*)
val set_global_level : level -> unit

(*
   Registers a tag generator on each logger, tagging every log message
   with the PID of the current process.

   If you use Parmap, make sure to call this on each spawned process, but
   only once (you can use the Parmap's ~init function for this).
*)
val add_PID_tag : unit -> unit

(*
   Load a json config file that sets log levels for each logger matching
   specific tags.
*)
val load_config_file : Common.filename -> unit
