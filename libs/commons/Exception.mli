(*
   Wrapper around Printexc to ensure or at least encourage uniform
   exception tracing.

   In some cases, exceptions are caught and passed around as data before
   being ultimately reraised. When doing this, it's important to keep track
   of where the exception was originally raised so we can understand what
   happened and debug the issue.

   To do this, we must capture a stack trace where the exception
   is first caught. Then, we can reraise this exception later with a proper
   function that will extend the original backtrace and give us a full trace.

   How to use exceptions for successful error tracking:

   1. Raise fresh exceptions normally using 'raise'.
   2. Use 'Exception.reraise' to reraise exceptions.
   3. Right after catching an exception with a try-with,
      call 'Exception.catch'.
   4. When defining a new exception, define an exception printer and
      register it globally with 'Printexc.register_printer'.

   * Do not pass exceptions around without an accompanying trace.
   * Do not re-raise exceptions using 'raise'.

   When to register exception printers?
   ====================================

   Custom exception printers are desirable for printing structured exceptions
   because the default exception printers don't go very deep.
   This is done with 'Printexc.register_printer'. Two cases should be
   distinguished:

   1. A freshly defined exception. In this case, we register its exception
      printer right away i.e. at module initialization time:

        exception Foo of bar
        let () = Printexc.register_printer (function
          | Foo x -> Some (...)
          | _ -> None
        )

   2. An exception from an external library. In this case, we don't want to
      override the exception printer silently because other applications
      using our library may not like it and prefer the original exception
      printer. For this, we provide a registration function that must be
      called explicitly so that the user can control which exception printer
      is active:

        (* Module System_util *)

        let register_exception_printers () =
          Printexc.register_printer (function
          | Unix.Unix_error x -> Some (...)
          | _ -> None
        )

      Some other module such as the application's entrypoint must call
      the registration functions:

        (* Module Main *)

        let main () =
          System_util.register_exception_printers ();
          ...

        (* Application's entry point *)
        let () = main ()
*)

(*
   A traced exception is a pair (exception, stack trace)

   The stack trace is a best effort to track the execution of the program
   up to the point where the exception was raised. OCaml offers mechanisms
   to produce the following kinds of traces under the same type:

   - stack backtrace: represents the call stack at the point where the last
     exception to be raised was raised ('Printexc.get_backtrace ()').
   - stack trace: represents the current call stack, possibly truncated to
     some maximum size ('Printexc.get_callstack 100').
   - extended trace: a trace that was extended by stitching multiple traces
     together ('Printexc.raise_with_backtrace exn trace').

   This module provides a thin interface to the standard 'Printexc'
   module with the goal of forcing the programmer to do the right thing
   and/or understand what they're doing.
*)
type t

(* Catch any exception and capture a stack backtrace *)
val catch_all : (unit -> 'a) -> ('a, t) Result.t

(* Create a traced exception in case we can't use 'catch_all'.
   This records the stack backtrace which is the state of the call stack
   where the exception was raised. *)
val catch : exn -> t

(* Re-raise an exception, extending the previous trace. *)
val reraise : t -> 'a

(* Re-raise a freshly-caught exception, preserving the stack backtrace
   that indicates where the exception was originally raised.
   Must be called immediately after catching the exception with try-with
   before any other exception could be raised and caught during the
   execution of other functions. *)
val catch_and_reraise : exn -> 'a

(* Create a traced exception and record the state of the stack at this point
   in the program execution. When catching an exception, it's best to
   use 'catch' instead so as to get the full stack
   (where the exception was raised rather than the smaller stack where
   the exception was caught). *)
val trace : exn -> t

(* Low-level creation of a traced exception. *)
val create : exn -> Printexc.raw_backtrace -> t

(* Get the original exception without the trace. *)
val get_exn : t -> exn

(* Get the trace. *)
val get_trace : t -> Printexc.raw_backtrace

(*
   Convert the exception and the trace into a string in a multiline format
   suitable for error messages. It always includes a terminal newline ('\n').

   It uses the standard 'Printexc.to_string' function, which itself will
   call the custom exception printers registered with
   'Printexc.register_printer'.
*)
val to_string : t -> string

(* A timeout exception with accompanying debug information:
   - a descriptive name
   - the time limit
*)
type timeout_info = { name : string; max_duration : float }

(*
   If ever caught, this exception must be re-raised immediately so as
   to not interfere with the timeout handler. See function 'set_timeout'.
*)
exception Timeout of timeout_info
