(*
   Implement the full gitignore syntax

   https://git-scm.com/docs/gitignore
*)

(* The location of a pattern, for logging and troubleshooting. *)
type loc = {
  (* File name or other source location name useful to a human reader
     in error messages. *)
  source_name : string;
  (* Line number, starting from 1. *)
  line_number : int;
  line_contents : string;
}

(* Path selector. *)
type pattern = {
  loc : loc;
  (* The matcher tells whether a given path matches the pattern.
     For example, the pattern /foo matches the path / *)
  matcher : Fpath.t -> bool;
}

type t = pattern list

(* Parsing functions. They will raise exceptions if the input is malformed. *)
val from_string : string -> t
val from_channel : in_channel -> t
val from_file : string -> t
