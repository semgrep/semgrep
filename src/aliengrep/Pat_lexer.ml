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
   Produce a stream of tokens for a pattern.

   This doesn't use ocamllex because we need to support character sets
   defined dynamically (e.g. from a config file).
*)

module Log = Log_aliengrep.Log
open Printf

type compiled_conf = {
  conf : Conf.t;
  pcre : Pcre2_.t; (* holds the source pattern and the compiled regexp *)
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
  let newline_9 = {|(\r?\n)|} in
  let other_10 = {|(.)|} in
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
    match Pcre2_.compile pat with
    | Ok re -> re
    | Error err ->
        Log.err (fun m ->
            m
              "cannot compile PCRE2 pattern used to parse aliengrep patterns: \
               %s"
              pat);
        failwith
          (sprintf "cannot compile PCRE2 pattern %S: %s" pat
             (Format.asprintf "%a" Pcre2.pp_compile_error err))
  in
  { conf; pcre }

let char_of_string str =
  if String.length str <> 1 then
    invalid_arg (sprintf "Pat_lexer.char_of_string: %S" str)
  else str.[0]

(* Recover the token for one match by finding which of the numbered alternation
   groups (1..10) participated. A match with no participating group (e.g.
   whitespace, which has no capturing group) yields [None] and is dropped. *)
let token_of_captures conf captures =
  let rec matched_group num =
    if num >= Pcre2_.captures_length captures then None
    else
      match Pcre2_.substring_of_captures captures num with
      | Some capture -> Some (num, capture)
      | None -> matched_group (num + 1)
  in
  matched_group 1
  |> Option.map (fun (num, capture) ->
      match num with
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
      | _ -> assert false)

let read_string ?(source_name = "<pattern>") conf str =
  (* The splitting regexp is an alternation of numbered capturing groups
     (kind 1)|(kind 2)|...; every position in [str] matches exactly one
     alternative (the catch-all group 10 matches any single character), so the
     matches must tile [str] with no gaps and each match has exactly one
     participating capturing group. We fold over the matches, checking that
     each starts where the previous one ended (and that the last reaches the
     end of [str]) so that a coverage gap fails loudly rather than silently
     dropping characters from the token stream. The accumulator carries the
     byte offset the next match must start at and the tokens so far, reversed. *)
  let end_pos, rev_tokens =
    Pcre2_.captures_iter conf.pcre str
    |> Seq.fold_left
         (fun (expected, acc) -> function
           | Error pcre_err ->
               pattern_error source_name
                 (sprintf
                    "PCRE2 error while parsing aliengrep pattern: %s; pattern: \
                     %s"
                    (Format.asprintf "%a" Pcre2.pp_match_error pcre_err)
                    conf.pcre.pattern)
           | Ok captures ->
               let { Pcre2_.start; end_ } = Pcre2_.range_of_captures captures in
               if start <> expected then
                 pattern_error source_name
                   (sprintf
                      "Internal error while parsing aliengrep pattern: gap in \
                       coverage at bytes %d-%d; pattern: %s"
                      expected start conf.pcre.pattern);
               let acc =
                 match token_of_captures conf captures with
                 | Some tok -> tok :: acc
                 | None -> acc
               in
               (end_, acc))
         (0, [])
  in
  if end_pos <> String.length str then
    pattern_error source_name
      (sprintf
         "Internal error while parsing aliengrep pattern: trailing bytes %d-%d \
          not covered; pattern: %s"
         end_pos (String.length str) conf.pcre.pattern);
  List.rev rev_tokens
