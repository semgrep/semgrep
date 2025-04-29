(*
   Matching of a glob pattern against a path.
   This is purely syntactic: the file system is not accessed.
*)

type compiled_pattern [@@deriving show]
(** A compiled pattern *)

type loc = {
  source_name : string;
      (** File name or other source location name useful to a human reader
          in error messages. *)
  source_kind : string option;
      (** Classify the source if you wish, for better error messages. *)
  line_number : int;  (** Line number, starting from 1. *)
  line_contents : string;  (** Contents of the line *)
}
[@@deriving show]
(** The location of a pattern, for logging and troubleshooting. *)

type conf = { glob_period : bool }
(** Options for interpreting glob patterns.

  The options with the [glob_] prefix correspond to
  the man page for glob(3). We don't support all the options but at least
  we support those needed to handle Gitignore files.

  glob_period: allow a leading period to be matched by metacharacters.
  By default, metacharacters can't match a leading period.
*)

val default_conf : conf
(** Default options as per glob(3). *)

val compile : ?conf:conf -> source:loc -> Pattern.t -> compiled_pattern
(** Compiles the pattern into something efficient (e.g., a regex). The
    [source] should be the original glob pattern before parsing. It's used
    only for debugging purposes.

*)

val run : compiled_pattern -> string -> bool
(** Match a path against a pattern:
    - The pattern is anchored: the beginning of the pattern must match
      the beginning of the path, and the end of the pattern must match
      the end of the path.
    - The path must be slash-separated (not backslash-separated like on
      Windows).
    - If the pattern starts with a slash, the path must start with a slash
      as well. In both cases, the matching starts from the beginning.
    - Matching is purely syntactic. No file system lookup will be attempted.

    Examples:

    {t
      Pattern  | Matching paths                 | Non-matching paths
      ---------|--------------------------------|-------------------------
      [/*.c]   | [/foo.c], [/bar.c]             | [foo.c], [bar.c], [/tmp/foo.c], [/tmp/bar.c]
      [*.c]    | [bar.c]                        | [/bar.c], [foo.c/bar], [bar/foo.c]
      [**/*.c] | [foo.c], [bar/foo.c], [/foo.c] | [foo.c/bar]
      [foo/]   | [foo/]                         | [foo], [bar/foo/], [/foo/], [/foo]
    }
*)

val string_loc :
  ?source_name:string -> source_kind:string option -> string -> loc
(** Create a location from a pattern string rather than a location in a file. *)

val source : compiled_pattern -> loc
(** Returns the location provided when the pattern was compiled *)
