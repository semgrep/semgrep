(* Same as print_endline: print the string and a newline, then flush stdout.
 * Just shorter. *)
val pr : string -> unit

(*****************************************************************************)
(* debugging *)
(*****************************************************************************)
(* see also Dumper.ml *)

(* Print a string and a newline to stderr, then flush stderr. The '2'
 * is used to refect that it prints on stderr (file descriptor '2' in Unix). *)
val pr2 : string -> unit

(* Print on stderr any data structure (using Dumper.dump) *)
val pr2_gen : 'a -> unit

(* Print on stderr but only once (to avoid printing the same error
 * again and again) *)
val pr2_once : string -> unit

(* forbid pr2_once to do the once "optimisation" *)
val _already_printed : (string, bool) Hashtbl.t
val disable_pr2_once : bool ref

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

(*
   Run a function and print how long it took to return or to raise an
   exception. pr_time prints to stdout.
*)
val pr_time : string -> (unit -> 'a) -> 'a

(*
   Run a function and print how long it took to return or to raise an
   exception. pr2_time prints to stderr.
*)
val pr2_time : string -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* run by main_boilerplate below at its finalize step before exiting.
 * Can be used for example to display some profiling information
 * (see Profiling.ml as an example)
 *)
val before_exit : (unit -> unit) list ref

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit
