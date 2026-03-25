(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Parsing and matching configuration for aliengrep
*)

open Printf
open Sets

type t = {
  caseless : bool;
  (* multiline = newlines are treated as ordinary whitespace *)
  multiline : bool;
  (* TODO: support UTF-8 word characters *)
  word_chars : char list;
  brackets : (char * char) list;
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
    caseless = false;
    multiline = true;
    word_chars = ('_' :: upper) @ lower @ digit;
    brackets = [ ('(', ')'); ('[', ']'); ('{', '}') ];
  }

let default_singleline_conf =
  {
    caseless = false;
    multiline = false;
    word_chars = default_multiline_conf.word_chars;
    brackets = [ ('"', '"'); ('\'', '\'') ] @ default_multiline_conf.brackets;
  }

let config_error msg =
  failwith (sprintf "Error in aliengrep configuration: %s" msg)

let check conf =
  let word_chars = Char_set.of_list conf.word_chars in
  let open_chars_list, close_chars_list = List_.split conf.brackets in
  let open_chars = Char_set.of_list open_chars_list in
  let close_chars = Char_set.of_list close_chars_list in
  let brace_chars = Char_set.union open_chars close_chars in
  if Char_set.is_empty word_chars then
    config_error "empty set of word characters";
  let conflicts = Char_set.inter word_chars brace_chars |> Char_set.elements in
  (match conflicts with
  | [] -> ()
  | chars ->
      let chars =
        chars |> List.map (fun c -> sprintf "%C" c) |> String.concat " "
      in
      config_error
        ("some word characters are also defined as brace characters: " ^ chars));
  if Char_set.cardinal open_chars <> List.length open_chars_list then
    config_error "some opening braces are repeated";
  if Char_set.cardinal close_chars <> List.length close_chars_list then
    config_error "some closing braces are repeated"
