(* Pad's poor's man profiler. See pfff's Main.ml for example of use
 * and the -profile command-line flag
 *)
type prof = ProfAll | ProfNone | ProfSome of string list

val profile : prof ref
val show_trace_profile : bool ref
val _profile_table : (string, float ref * int ref) Hashtbl.t ref
val profile_code : string -> (unit -> 'a) -> 'a
val profile_diagnostic : unit -> string
val profile_code_exclusif : string -> (unit -> 'a) -> 'a
val profile_code_inside_exclusif_ok : string -> (unit -> 'a) -> 'a
val warn_if_take_time : int -> string -> (unit -> 'a) -> 'a

(* similar to profile_code but log some information during execution too *)
val profile_code2 : string -> (unit -> 'a) -> 'a

(* to use with Arg, to add a -profile that enables profiling *)
val flags : unit -> (string * Arg.spec * string) list

(* log and print on stderr, usually called just before exit *)
val log_diagnostics_and_gc_stats : unit -> unit
