(*
   Produce a stream of tokens for a pattern.

   This doesn't use ocamllex because we need to support character sets
   defined dynamically (e.g. from a config file).

   TODO: UTF-8 support.
   See https://erratique.ch/software/uucp/doc/unicode.html for a refresher
   on Unicode and UTF-8.
*)

type conf = {
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  braces : (char * char) list;
}

val default_conf : conf

type compiled_conf

type token =
  | ELLIPSIS (* "..." *)
  | LONG_ELLIPSIS (* "...." *)
  | METAVAR of string * string (* "$FOO", "FOO" *)
  | METAVAR_ELLIPSIS of string * string (* "$...FOO", "FOO" *)
  | LONG_METAVAR_ELLIPSIS of string * string (* "$....FOO", "FOO" *)
  | WORD of string
  | OPEN of char
  | CLOSE of char
  (* TODO: support UTF8-encoded characters *)
  | CHAR of char

(*
   Validate and compile the configuration. This configuration can be
   reused to compile multiple patterns.

   Raise a 'Failure' exception in case of an invalid configuration.
*)
val compile : conf -> compiled_conf

(*
   Read a pattern into a list of lexical elements as configured.

   Since patterns are expected to be short, we don't bother with tracking
   the source location of the tokens.
*)
val read_string : ?source_name:string -> compiled_conf -> string -> token list
