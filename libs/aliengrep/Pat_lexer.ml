(*
   Produce a stream of tokens for a pattern.

   This doesn't use ocamllex because we need to support character sets
   defined dynamically (e.g. from a config file).
*)

open Printf

type compiled_conf = {
  conf : Conf.t;
  pcre_pattern : string;
  pcre_regexp : Pcre.regexp;
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
  let open_chars, close_chars = List.split conf.braces in
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
  let other_9 = sprintf {|(.|\n)|} in
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
        other_9;
      ]
  in
  let pcre_regexp =
    try SPcre.regexp pat with
    | exn ->
        let e = Exception.catch exn in
        Logs.err (fun m ->
            m "cannot compile PCRE pattern used to parse aliengrep patterns: %s"
              pat);
        Exception.reraise e
  in
  { conf; pcre_pattern = pat; pcre_regexp }

let char_of_string str =
  if String.length str <> 1 then
    invalid_arg (sprintf "Pat_lexer.char_of_string: %S" str)
  else str.[0]

let read_string ?(source_name = "<pattern>") conf str =
  match SPcre.full_split ~rex:conf.pcre_regexp str with
  | Error pcre_err ->
      pattern_error source_name
        (sprintf "PCRE error while parsing aliengrep pattern: %s; pattern: %s"
           (SPcre.show_error pcre_err)
           conf.pcre_pattern)
  | Ok res ->
      res
      |> List.filter_map (function
           | Pcre.Delim _
           | Pcre.NoGroup ->
               None
           | Pcre.Text txt ->
               pattern_error source_name
                 (sprintf
                    "Internal error while parsing aliengrep pattern: Text node \
                     %S; pattern: %s"
                    txt conf.pcre_pattern)
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
                       try List.assoc opening_brace conf.conf.braces with
                       | Not_found -> assert false
                     in
                     OPEN (opening_brace, expected_closing_brace)
                 | 8 -> CLOSE (char_of_string capture)
                 | 9 -> OTHER capture
                 | _ -> assert false))
