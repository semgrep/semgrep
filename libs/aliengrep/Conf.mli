(*
   Parsing and matching configuration for aliengrep
*)

type t = {
  (* Use case-insensitive matching. Rely on PCRE to do this well. *)
  caseless : bool;
  (* multiline = newlines are treated as ordinary whitespace *)
  multiline : bool;
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  brackets : (char * char) list;
}

(* TODO: document the difference in the defaults *)
val default_multiline_conf : t
val default_singleline_conf : t

(* Check the validity of the configuration.
   Raises an exception if the configuration is invalid. *)
val check : t -> unit
