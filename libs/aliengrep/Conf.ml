(*
   Parsing and matching configuration for aliengrep
*)

open Printf

type t = {
  (* multiline = newlines are treated as ordinary whitespace *)
  multiline : bool;
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  braces : (char * char) list;
}

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

let config_error msg =
  failwith (sprintf "Error in aliengrep configuration: %s" msg)

let check conf =
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
