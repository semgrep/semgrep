(*
   AST and matching of a glob pattern against a path.
   This is purely syntaxic: the file system is not accessed.
*)

type char_class_range = Class_char of char | Range of char * char
type char_class = { complement : bool; ranges : char_class_range list }

type component_fragment =
  | Char of char
  | Char_class of char_class
  | Question
  | Star

(* A path component is what represents a simple file name in a directory *)
type component =
  | Component of component_fragment list
  | Ellipsis (* '**' = path ellipsis *)

(*
   A pattern which matches paths.
*)
type pattern = component list

(* A compiled pattern matcher. *)
type compiled_pattern

(*
   Compile the pattern into something efficient. The source should be
   the original glob pattern before parsing. It's used only for debugging
   purposes.
*)
val compile : source:string -> pattern -> compiled_pattern

(*
   Match an absolute or relative path against a pattern.

   If the pattern is absolute, then both the beginning and the end of the
   path must match. Otherwise if the pattern is relative, only the end
   of the path must match.

   Example:

   absolute pattern: /*.c
   matching paths: /foo.c /bar.c
   non-matching paths: foo.c bar.c /tmp/foo.c /tmp/bar.c

   relative pattern: *.c
   matching paths: bar.c /bar.c foo/bar.c
   non-matching paths: foo.c/bar
*)
val run : compiled_pattern -> Fpath.t -> bool
val source : compiled_pattern -> string
