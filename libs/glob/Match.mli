(*
   Matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.
*)

(* A compiled pattern matcher. *)
type compiled_pattern [@@deriving show]

(* The location of a pattern, for logging and troubleshooting. *)
type loc = {
  (* File name or other source location name useful to a human reader
     in error messages. *)
  source_name : string;
  (* Classify the source if you wish, for better error messages. *)
  source_kind : string option;
  (* Line number, starting from 1. *)
  line_number : int;
  line_contents : string;
}
[@@deriving show]

(*
   Compile the pattern into something efficient (currently uses the
   ocaml-re library). The source should be the original glob pattern
   before parsing. It's used only for debugging purposes.
*)
val compile : source:loc -> Pattern.t -> compiled_pattern

(*
   Match a path against a pattern:
   - The pattern is anchored: the beginning of the pattern must match
     the beginning of the path, and the end of the pattern must match
     the end of the path.
   - The path must be slash-separated (not backslash-separated like on
     Windows).
   - If the pattern starts with a slash, the path must start with a slash
     as well. In both cases, the matching starts from the beginning.
   - Matching is purely syntactic. No file system lookup will be attempted.

   Examples:

   absolute pattern: /*.c
   matching paths: /foo.c /bar.c
   non-matching paths: foo.c bar.c /tmp/foo.c /tmp/bar.c

   relative pattern: *.c
   matching paths: bar.c
   non-matching paths: /bar.c foo.c/bar bar/foo.c

   sliding pattern: **/*.c
   matching paths: foo.c bar/foo.c /foo.c
   non-matching paths: foo.c/bar

   folder pattern: foo/
   matching paths: foo/
   non-matching paths: foo bar/foo/ /foo/ /foo
*)
val run : compiled_pattern -> string -> bool

(* Create a location from a pattern string rather than a location in a file. *)
val string_loc :
  ?source_name:string -> source_kind:string option -> string -> loc

val source : compiled_pattern -> loc
