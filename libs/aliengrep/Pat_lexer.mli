(*
   Produce a stream of tokens for a pattern.

   TODO: UTF-8 support.
   See https://erratique.ch/software/uucp/doc/unicode.html for a refresher
   on Unicode and UTF-8.
*)

type conf = {
  (* multiline = newlines are treated as ordinary whitespace *)
  multiline : bool;
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  braces : (char * char) list;
}

val default_multiline_conf : conf
val default_uniline_conf : conf

type compiled_conf

type token =
  | ELLIPSIS (* "..." *)
  | LONG_ELLIPSIS (* "...." *)
  | METAVAR of string (* "FOO" extracted from "$FOO" *)
  | METAVAR_ELLIPSIS of string (* "FOO" extracted from "$...FOO" *)
  | LONG_METAVAR_ELLIPSIS of string (* "FOO" extracted from "$....FOO" *)
  | WORD of string
  | OPEN of char * char
    (* any of the opening-brace characters
       and the expected closing brace *)
  | CLOSE of char (* any of the closing-brace characters *)
  (* a single character according to PCRE (UTF-8-encoded code point) *)
  | OTHER of string

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
