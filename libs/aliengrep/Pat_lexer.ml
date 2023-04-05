(*
   Produce a stream of tokens for a pattern.

   This doesn't use ocamllex because we need to support character sets
   defined dynamically (e.g. from a config file).
*)

open Printf

type conf = {
  multiline : bool;
  (* TODO: support Unicode? *)
  (* TODO: support ranges?
     or maybe: specify characters to add or remove from the default set *)
  word_chars : char list;
  braces : (char * char) list;
}

type compiled_conf = {
  conf : conf;
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

let config_error msg =
  failwith (sprintf "Error in aliengrep configuration: %s" msg)

let pattern_error source_name msg =
  failwith
    (sprintf "%s: Error: failed to parse aliengrep pattern: %s" source_name msg)

let upper =
  [
    'A';
    'B';
    'C';
    'D';
    'E';
    'F';
    'G';
    'H';
    'I';
    'J';
    'K';
    'L';
    'M';
    'N';
    'O';
    'P';
    'Q';
    'R';
    'S';
    'T';
    'U';
    'V';
    'W';
    'X';
    'Y';
    'Z';
  ]

let lower =
  [
    'a';
    'b';
    'c';
    'd';
    'e';
    'f';
    'g';
    'h';
    'i';
    'j';
    'k';
    'l';
    'm';
    'n';
    'o';
    'p';
    'q';
    'r';
    's';
    't';
    'u';
    'v';
    'w';
    'x';
    'y';
    'z';
  ]

let digit = [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ]

let default_multiline_conf =
  {
    multiline = true;
    word_chars = ('_' :: upper) @ lower @ digit;
    braces = [ ('(', ')'); ('[', ']'); ('{', '}') ];
  }

let default_uniline_conf =
  {
    multiline = false;
    word_chars = default_multiline_conf.word_chars;
    braces = [ ('"', '"'); ('\'', '\'') ] @ default_multiline_conf.braces;
  }

let check_conf conf =
  let word_chars = Set_.of_list conf.word_chars in
  let open_chars_list, close_chars_list = List.split conf.braces in
  let open_chars = Set_.of_list open_chars_list in
  let close_chars = Set_.of_list close_chars_list in
  let brace_chars = Set_.union open_chars close_chars in
  if Set_.is_empty word_chars then config_error "empty set of word characters";
  let conflicts = Set_.inter word_chars brace_chars |> Set_.elements in
  (match conflicts with
  | [] -> ()
  | chars ->
      let chars =
        chars |> Common.map (fun c -> sprintf "%C" c) |> String.concat " "
      in
      config_error
        ("some word characters are also defined as brace characters: " ^ chars));
  if Set_.cardinal open_chars <> List.length open_chars_list then
    config_error "some opening braces are repeated";
  if Set_.cardinal close_chars <> List.length close_chars_list then
    config_error "some closing braces are repeated"

let char_class_of_set chars =
  let buf = Buffer.create 100 in
  Buffer.add_char buf '[';
  List.iter
    (fun c ->
      match c with
      | '-'
      | '^'
      | '['
      | ']' ->
          bprintf buf {|\x%02X|} (Char.code c)
      | ' ' .. '~' -> Buffer.add_char buf c
      | _ -> bprintf buf {|\x%02X|} (Char.code c))
    chars;
  Buffer.add_char buf ']';
  Buffer.contents buf

(*
   Compile into a regexp that will be used to split the string.
   The regexp is of the form (kind 1)|(kind 2)|...
   Each capturing group is numbered and indicates the type of the token.
*)
let compile conf =
  check_conf conf;
  let open_chars, close_chars = List.split conf.braces in
  let ellipsis_1 = {|\.\.\.|} in
  let long_ellipsis_2 = {|\.\.\.\.|} in
  let metavar_3 = {|\$([A-Z][A-Z0-9_]*)|} in
  let metavar_ellipsis_4 = {|\$\.\.\.([A-Z][A-Z0-9_]*)|} in
  let long_metavar_ellipsis_5 = {|\$\.\.\.\.([A-Z][A-Z0-9_]*)|} in
  let whitespace =
    (* HT (9), optional LF (10), VT (11), FF (12), CR (13), and space (32) *)
    if conf.multiline then {|[\t\n\x0B\x0C\x0D ]|}
    else (* same without \n *)
      {|[\t\n\x0B\x0C\x0D ]|}
  in
  let word_6 = sprintf {|(%s+)|} (char_class_of_set conf.word_chars) in
  let open_7 = sprintf {|(%s+)|} (char_class_of_set open_chars) in
  let close_8 = sprintf {|(%s+)|} (char_class_of_set close_chars) in
  let other_9 = sprintf {|(.)|} in
  let pat =
    String.concat "|"
      [
        ellipsis_1;
        long_ellipsis_2;
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
  { conf; pcre_pattern = pat; pcre_regexp = SPcre.regexp pat }

let char_of_string str =
  if String.length str <> 1 then assert false else str.[0]

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
           | Pcre.Group (num, capture) ->
               Some
                 (match num with
                 | 1 -> ELLIPSIS
                 | 2 -> LONG_ELLIPSIS
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
