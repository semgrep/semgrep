(*
   AST and matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.

   The main reference for this is https://git-scm.com/docs/gitignore
   which itself relies partially on POSIX glob
   https://man7.org/linux/man-pages/man7/glob.7.html (man 7 glob)
*)

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

(* Create a location from a pattern string rather than a location in a file. *)
val string_loc :
  ?source_name:string -> source_kind:string option -> string -> loc

val show_loc : loc -> string

type char_class_range = Class_char of char | Range of char * char
[@@deriving show]

type char_class = { complement : bool; ranges : char_class_range list }
[@@deriving show]

type segment_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star
[@@deriving show]

(* A path segment is what represents a simple file name in a directory *)
type segment = Segment of segment_fragment list | Any_subpath
[@@deriving show]

(*
   A pattern which matches paths.
*)
type pattern = segment list [@@deriving show]

val of_path_segments : string list -> pattern

(* A compiled pattern matcher. *)
type t

val show : t -> string
val source : t -> loc

(* The pattern that matches '/' *)
val root_pattern : pattern

(* Append the segments, taking care of trailing slashes that prevent
   us from using a plain list append (@). *)
val append : pattern -> pattern -> pattern

(*
   Compile the pattern into something efficient (currently uses the
   ocaml-re library). The source should be the original glob pattern
   before parsing. It's used only for debugging purposes.
*)
val compile : source:loc -> pattern -> t

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
val run : t -> string -> bool

(* This is used by the unit tests and prints the activity of the
   'run' function. I'm worried that logger#debug would be too expensive
   when not debugging. *)
val debug : bool ref
