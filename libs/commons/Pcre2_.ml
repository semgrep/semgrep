(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Shared settings for using the Pcre2 module (pcre-ocaml library).

   This file is mostly a port of Pcre_.ml (and the previous Regexp_engine.ml)
   for PCRE2.
*)

let ( let* ) = Result.bind
let src = Logs.Src.create "commons.pcre"

module Log = (val Logs.src_log src : Logs.LOG)

(* Keep the regexp source around for better error reporting and
   troubleshooting. *)
type t = {
  pattern : string;
  regex : Pcre2.Interp.t;
      [@opaque] [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
}
[@@deriving show, eq, ord]

(* not sure why needed ??? *)
let _ = pp
let _ = equal
let hash (x : t) = Base.String.hash x.pattern
let hash_fold_t s (x : t) = Base.String.hash_fold_t s x.pattern
let sexp_of_t (x : t) = Sexplib.Std.sexp_of_string x.pattern

type match_ = Pcre2.match_ [@@deriving show]
type captures = Pcre2.captures [@@deriving show]
type range = Pcre2.Interp.range = { start : int; end_ : int } [@@deriving show]

let range_of_match = Pcre2.Interp.range_of_match
let range_of_captures = Pcre2.Interp.range_of_captures
let substring_of_match = Pcre2.Interp.substring_of_match
let match_of_captures = Pcre2.Interp.match_of_captures
let named_match_of_captures = Pcre2.Interp.named_match_of_captures

let substring_of_captures captures group =
  match_of_captures captures group |> Option.map substring_of_match

let named_substring_of_captures captures name =
  named_match_of_captures captures name |> Option.map substring_of_match

let capture_groups ({ regex; _ } : t) = Pcre2.Interp.capture_groups regex
let captures_length = Pcre2.Interp.captures_length

(*
   'match_limit' and 'depth_limit' are set explicitly to make semgrep
   fail consistently across platforms (e.g. CI vs. local Mac): the PCRE2
   compile-time defaults are 10_000_000 for both, but they can be overridden
   during the installation of the pcre2 library, so we protect ourselves from
   such custom installs.

   They are also much lower than the defaults because PCRE2 does not support
   timeouts (and `Common.set_timeout` cannot interrupt the C library): these
   limits are what stops a catastrophically backtracking regex from appearing
   to hang semgrep. See perf/input/semgrep_targets.txt and
   perf/input/semgrep_targets.yaml for an example where Semgrep appeared to
   hang (but it was just the PCRE engine taking way too much time).
*)
let match_limit = 1_000_000
let depth_limit = 10_000

let extra_compilation_options =
  [
    (* Flag required for the following to succeed:
         Pcre2_.compile ~options:[`UTF] "\\x{200E}" *)
    `UTF;
    `MATCH_LIMIT match_limit;
    `DEPTH_LIMIT depth_limit;
  ]

let compile ?(options : Pcre2.Interp.compile_option list = [])
    (pattern : string) =
  (* pcre doesn't mind if a flag is duplicated so we just append extra flags *)
  let options = extra_compilation_options @ options in
  let* regex = Pcre2.Interp.compile ~options pattern in
  Ok { pattern; regex }

let show_compile_error ({ code; offset } : Pcre2.compile_error) =
  (* [Pcre2.show_compile_error_code] is pcre2's own error message, e.g.
     "missing closing parenthesis". *)
  Printf.sprintf "%s at position %d" (Pcre2.show_compile_error_code code) offset

let compile_exn ?options pattern =
  match compile ?options pattern with
  | Ok rex -> rex
  | Error err ->
      invalid_arg
        (Printf.sprintf "Pcre2_.compile_exn: cannot compile regex %S: %s"
           pattern (show_compile_error err))

let is_match ?options ?subject_offset ({ regex; _ } : t) (subject : string) =
  Pcre2.Interp.is_match ?options ?subject_offset regex subject

let find ?options ?subject_offset ({ regex; _ } : t) (subject : string) =
  Pcre2.Interp.find ?options ?subject_offset regex subject

let find_iter ?options ?subject_offset ({ regex; _ } : t) (subject : string) =
  Pcre2.Interp.find_iter ?options ?subject_offset regex subject

let captures ?options ?subject_offset ({ regex; _ } : t) (subject : string) =
  Pcre2.Interp.captures ?options ?subject_offset regex subject

let captures_iter ?options ?subject_offset ({ regex; _ } : t) (subject : string)
    =
  Pcre2.Interp.captures_iter ?options ?subject_offset regex subject

let split ?options ?subject_offset ?limit ({ regex; _ } : t) (subject : string)
    =
  Pcre2.Interp.split ?options ?subject_offset ?limit regex subject

let replace_matches_fn ?limit ~range ~replacement (subject : string) matches =
  let matches =
    match limit with
    | Some n -> Seq.take n matches
    | None -> matches
  in
  (* The current string length should be a fair approximation of the resulting
     string's length. *)
  let new_buf = Buffer.create @@ String.length subject in
  let rec replace_from offset matches =
    match matches () with
    | Seq.Nil ->
        Buffer.add_substring new_buf subject offset
          (String.length subject - offset);
        Ok (Buffer.contents new_buf)
    | Seq.Cons (Error err, _) -> Error err
    | Seq.Cons (Ok matched, rest) ->
        let { start; end_ } = range matched in
        Buffer.add_substring new_buf subject offset (start - offset);
        Buffer.add_string new_buf (replacement matched);
        replace_from end_ rest
  in
  replace_from 0 matches

let replace_captures_fn ?options ?subject_offset ?limit ({ regex; _ } : t)
    (f : captures -> string) (subject : string) =
  Pcre2.Interp.captures_iter ?options ?subject_offset regex subject
  |> replace_matches_fn ?limit ~range:range_of_captures ~replacement:f subject

let expand_capture_references (template : string) (captures : captures) : string
    =
  let len = String.length template in
  let buf = Buffer.create len in
  let rec end_of_digits i =
    if i < len && Base.Char.is_digit template.[i] then end_of_digits (i + 1)
    else i
  in
  let rec scan i =
    if i < len then
      match template.[i] with
      | '$' when i + 1 < len && Char.equal template.[i + 1] '$' ->
          Buffer.add_char buf '$';
          scan (i + 2)
      | '\\'
      | '$'
        when i + 1 < len && Base.Char.is_digit template.[i + 1] ->
          let end_ = end_of_digits (i + 1) in
          let reference = String.sub template (i + 1) (end_ - i - 1) in
          Option.bind
            (int_of_string_opt reference)
            (substring_of_captures captures)
          |> Option.iter (Buffer.add_string buf);
          scan end_
      | c ->
          Buffer.add_char buf c;
          scan (i + 1)
  in
  scan 0;
  Buffer.contents buf

let replace ?options ?subject_offset ?limit rex ~(template : string)
    (subject : string) =
  replace_captures_fn ?options ?subject_offset ?limit rex
    (expand_capture_references template)
    subject

let replace_fn ?options ?subject_offset ?limit (rex : t) (f : string -> string)
    (subject : string) =
  find_iter ?options ?subject_offset rex subject
  |> replace_matches_fn ?limit ~range:range_of_match
       ~replacement:(fun matched -> f (substring_of_match matched))
       subject

let char_needs_escaping = function
  | '\\'
  | '^'
  | '$'
  | '.'
  | '['
  | ']'
  | '|'
  | '('
  | ')'
  | '?'
  | '*'
  | '+'
  | '{'
  | '-' ->
      true
  | _ -> false

let quote s =
  let len = String.length s in
  let escape_count =
    String.fold_left
      (fun count c -> if char_needs_escaping c then count + 1 else count)
      0 s
  in
  if escape_count = 0 then s
  else
    let buf = Bytes.create (len + escape_count) in
    let pos = ref 0 in
    String.iter
      (fun c ->
        if char_needs_escaping c then (
          Bytes.set buf !pos '\\';
          incr pos);
        Bytes.set buf !pos c;
        incr pos)
      s;
    Bytes.unsafe_to_string buf

(* Formerly Regexp_engine *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper to the actual regexp engine(s).
 *
 * Regexps are used in many places in Semgrep:
 *  - in Pattern_vs_code to support the "=~/.../",
 *  - in Semgrep.ml for the metavariable-regexp and pattern-regexp
 *  - in Optimizing/ for skipping rules or target files
 *    (See Analyze_pattern.ml for more information).
 *  - TODO for include/exclude globbing
 *
 * notes: I tried to use the ocaml-re (Re) regexp libray instead of Str
 * because I thought it would be faster, and because it offers regexp
 * combinators (alt, rep, etc.) which might be useful at some point to
 * handle patterns containing explicit DisjExpr or for Analyze_rule.ml.
 * However when running on Zulip codebase with zulip semgrep rules,
 * Str is actually faster than Re.
 *
 * alternatives:
 *  - Str: simple, builtin
 *  - Re: provides alt() to build complex regexp, and also pure OCaml
 *    implem which is great in a JSOO context, but it seems slower.
 *    Can also support globbing with the Re.Glob module!
 *  - PCRE: powerful, but C dependency
 *
 * TODO:
 *  - move the regexp-related code in Pattern_vs_code here!
 *  - use Re.Glob just for globbing?
 *
 *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let show (x : t) = x.pattern
let pp fmt (x : t) = Format.fprintf fmt "\"%s\"" x.pattern
let equal (x1 : t) (x2 : t) = String.equal x1.pattern x2.pattern

(* TODO: use a flag instead? *)
let matching_exact_string s : t = compile_exn (quote s)

let matching_exact_word s =
  let pattern = "\b" ^ quote s ^ "\b" in
  compile_exn pattern

let unanchored_match rex subject = is_match rex subject

let may_contain_end_of_string_assertions =
  (* The absence of the following guarantees (to the best of our knowledge)
     that a regexp does not try to match the beginning or the end of
     the string:
       ^
       $
       \A
       \Z
       \z
       (?<!   negative lookbehind assertion, which could be a DIY \A
       (?!    negative lookahead assertion, which could be a DIY \z
  *)
  let rex = compile_exn {|[$^]|\\[AZz]|\(\?<!|\(\?!|} in
  fun s ->
    match is_match rex s with
    | Ok x -> x
    | Error e ->
        Log.warn (fun m ->
            m
              "error when checking if a regex may have an end of string \
               assertion: %a"
              Pcre2.pp_match_error e);
        (* true since we would rather be conservative given an error *)
        true

(* Any string that may still contain a end-of-string assertions must go
   through this. *)
let finish src =
  if may_contain_end_of_string_assertions src then None else Some src

(*
   Remove beginning-of-string and end-of-string constraints.
   Fail if some of them may remain e.g. if we find '^' in the middle of
   the pattern.
*)
let remove_end_of_string_assertions_from_string src : string option =
  (*
     a0 and a1 are the first two characters.
     z0 and z1 are the last two characters.
  *)
  let len = String.length src in
  if len = 0 then (* "" *)
    Some src
  else
    (* "X" *)
    let a0 = src.[0] in
    if len = 1 then
      Some
        (match a0 with
        | '^' -> ""
        | '$' -> ""
        | _ -> src)
    else
      (* "XX" *)
      let a1 = src.[1] in
      if len = 2 then
        match (a0, a1) with
        | '^', '$' -> Some ""
        | '^', c -> String.make 1 c |> finish
        | '\\', ('A' | 'Z' | 'z') -> Some ""
        | '\\', _ -> Some src
        | c, '$' -> String.make 1 c |> finish
        | _, _ -> src |> finish
      else
        (* "XXX" or longer *)
        let src =
          match (a0, a1) with
          | '^', _ -> String.sub src 1 (len - 1)
          | '\\', 'A' -> String.sub src 2 (len - 2)
          | _ -> src
        in
        (* remaining string: "X" or longer *)
        let len = String.length src in
        let z1 = src.[len - 1] in
        if len = 1 then
          match z1 with
          | '$' -> Some ""
          | _ -> src |> finish
        else
          (* remaining string: "XX" or longer *)
          let z0 = src.[len - 2] in
          match (z0, z1) with
          | '\\', ('Z' | 'z') -> Some (Str.first_chars src (len - 2))
          | '\\', _ -> Some src
          | _, '$' -> Str.first_chars src (len - 1) |> finish
          | _ -> src |> finish

let remove_end_of_string_assertions ({ pattern; _ } : t) =
  match remove_end_of_string_assertions_from_string pattern with
  | None -> None
  | Some pat ->
      (* should never be an illegal transformation *)
      Some (compile_exn ~options:[ `MULTILINE ] pat)
