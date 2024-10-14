(*
   Produce a stream of tokens for a pattern.

   This doesn't use ocamllex because we need to support character sets
   defined dynamically (e.g. from a config file).
*)
[@@@alert "-deprecated"]

module Log = Log_aliengrep.Log
open Printf

type compiled_conf = {
  conf : Conf.t;
  pcre : Pcre_.t; (* holds the source pattern and the compiled regexp *)
}

type token =
  | ELLIPSIS (* "..." *)
  | LONG_ELLIPSIS (* "...." *)
  | METAVAR of string (* "FOO" extracted from "$FOO" *)
  | METAVAR_ELLIPSIS of string (* "FOO" extracted from "$...FOO" *)
  | LONG_METAVAR_ELLIPSIS of string (* "FOO" extracted from "$....FOO" *)
  | WORD of string
  | OPEN of char * char
  | CLOSE of char
  | NEWLINE (* only exists in single-line mode *)
  | OTHER of string

let pattern_error source_name msg =
  failwith
    (sprintf "%s: Error: failed to parse aliengrep pattern: %s" source_name msg)

(*
   Compile into a regexp that will be used to split the string.
   The regexp is of the form (kind 1)|(kind 2)|...
   Each capturing group is numbered and indicates the type of the token.
*)
let compile conf =
  Conf.check conf;
  let open_chars, close_chars = List_.split conf.brackets in
  let long_ellipsis_1 = {|(\.\.\.\.)|} in
  let ellipsis_2 = {|(\.\.\.)|} in
  let metavar_3 = {|\$([A-Z][A-Z0-9_]*)|} in
  let metavar_ellipsis_4 = {|\$\.\.\.([A-Z][A-Z0-9_]*)|} in
  let long_metavar_ellipsis_5 = {|\$\.\.\.\.([A-Z][A-Z0-9_]*)|} in
  let whitespace =
    if conf.multiline then {|[[:space:]]+|} else {|[[:blank:]]+|}
  in
  let word_6 =
    sprintf {|(%s+)|} (Pcre_util.char_class_of_list conf.word_chars)
  in
  let open_7 = sprintf {|(%s)|} (Pcre_util.char_class_of_list open_chars) in
  let close_8 = sprintf {|(%s)|} (Pcre_util.char_class_of_list close_chars) in
  let newline_9 = sprintf {|(\r?\n)|} in
  let other_10 = sprintf {|(.)|} in
  let pat =
    String.concat "|"
      [
        long_ellipsis_1;
        ellipsis_2;
        metavar_3;
        metavar_ellipsis_4;
        long_metavar_ellipsis_5;
        whitespace;
        word_6;
        open_7;
        close_8;
        newline_9;
        other_10;
      ]
  in
  let pcre =
    try Pcre_.regexp pat with
    | exn ->
        let e = Exception.catch exn in
        Log.err (fun m ->
            m "cannot compile PCRE pattern used to parse aliengrep patterns: %s"
              pat);
        Exception.reraise e
  in
  { conf; pcre }

let char_of_string str =
  if String.length str <> 1 then
    invalid_arg (sprintf "Pat_lexer.char_of_string: %S" str)
  else str.[0]

let read_string ?(source_name = "<pattern>") conf str =
  match Pcre_.full_split ~rex:conf.pcre str with
  | Error pcre_err ->
      pattern_error source_name
        (sprintf "PCRE error while parsing aliengrep pattern: %s; pattern: %s"
           (Pcre_.show_error pcre_err)
           conf.pcre.pattern)
  | Ok res ->
      res
      |> List_.filter_map (function
           | Pcre.Delim _
           | Pcre.NoGroup ->
               None
           | Pcre.Text txt ->
               pattern_error source_name
                 (sprintf
                    "Internal error while parsing aliengrep pattern: Text node \
                     %S; pattern: %s"
                    txt conf.pcre.pattern)
           | Pcre.Group (_, "") ->
               (* no capture *)
               None
           | Pcre.Group (num, capture) ->
               Some
                 (match num with
                 | 1 -> LONG_ELLIPSIS
                 | 2 -> ELLIPSIS
                 | 3 -> METAVAR capture
                 | 4 -> METAVAR_ELLIPSIS capture
                 | 5 -> LONG_METAVAR_ELLIPSIS capture
                 | 6 -> WORD capture
                 | 7 ->
                     let opening_brace = char_of_string capture in
                     let expected_closing_brace =
                       try List.assoc opening_brace conf.conf.brackets with
                       | Not_found -> assert false
                     in
                     OPEN (opening_brace, expected_closing_brace)
                 | 8 -> CLOSE (char_of_string capture)
                 | 9 -> NEWLINE
                 | 10 -> OTHER capture
                 | _ -> assert false))
