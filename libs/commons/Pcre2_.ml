(* Cooper Pierce, Martin Jambon

   (c) 2024 Semgrep, Inc.

   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License version 2.1 as
   published by the Free Software Foundation, with the special exception on
   linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE for more details.
*)

(*
   Shared settings for using the Pcre2 module (pcre-ocaml library).

   This file is mostly a port of Pcre_.ml (and the previous Regexp_engine.ml)
   for PCRE2.
*)

open Printf

(* we reuse the one in Pcre_.ml, no need to differentiate *)
let src = Pcre_.src [@@alert "-deprecated"]

module Log = (val Logs.src_log src : Logs.LOG)

(* Keep the regexp source around for better error reporting and
   troubleshooting. *)
type t = {
  pattern : string;
  regexp : Pcre2.regexp; [@opaque] [@equal fun _ _ -> true]
}
[@@deriving show, eq]

(* Not sure why we are getting warnings but *)
let _ = pp
let _ = equal

(*
   Provide missing error->string conversion
*)
type error = Pcre2.error =
  | Partial
  | BadPattern of string * int
  | BadUTF
  | BadUTFOffset
  | MatchLimit
  | DepthLimit
  | WorkspaceSize
  | InternalError of string
[@@deriving show]

(*
   Flag required for the following to succeed:
     Pcre2.regexp ~flags:[`UTF] "\\x{200E}"
*)
let extra_flag = `UTF

(*
   'limit' and 'limit_recursion' are set explicitly to make semgrep
   fail consistently across platforms (e.g. CI vs. local Mac).
   The default compile-time defaults are 10_000_000 for both
   'limit' and 'limit_recursion' but they can be overridden during
   the installation of the pcre library. We protect ourselves
   from such custom installs.
*)
let regexp ?iflags ?(flags = []) ?chtables pat =
  (* pcre doesn't mind if a flag is duplicated so we just append extra flags *)
  let flags = extra_flag :: flags in
  (* OCaml's Pcre library does not support setting timeouts, and since it's just
   * a wrapper for a C library `Common.set_timeout` doesn't work... So, we set a
   * lower `limit` and `limit_recursion` (default values are 10_000_000) to avoid
   * spending too much time on regex matching. See perf/input/semgrep_targets.txt
   * and perf/input/semgrep_targets.yaml for an example where Semgrep appeared to
   * hang (but it was just the Pcre2 engine taking way too much time). *)
  let regexp =
    Pcre2.regexp ~limit:1_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT *)
      ~depth_limit:1_000_000
        (* sets the backtracking depth limit field in a match context; see `pcre2_set_depth_limit(3)` *)
      ?iflags ~flags ?chtables pat
  in
  { pattern = pat; regexp }

let pmatch ?iflags ?flags ~rex ?pos ?callout subj =
  try Ok (Pcre2.pmatch ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj) with
  | Pcre2.Error err -> Error err

let exec ?iflags ?flags ~rex ?pos ?callout subj =
  try
    Ok (Some (Pcre2.exec ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj))
  with
  | Not_found -> Ok None
  | Pcre2.Error err -> Error err

let exec_all ?iflags ?flags ~rex ?pos ?callout subj =
  try Ok (Pcre2.exec_all ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj) with
  | Not_found -> Ok [||]
  | Pcre2.Error err -> Error err

(* for debugging *)
let exec_to_strings ?iflags ?flags ~rex ?pos ?callout subj =
  match exec_all ?iflags ?flags ~rex ?pos ?callout subj with
  | Ok a -> Ok (Array.map Pcre2.get_substrings a)
  | Error _ as e -> e

let split ?iflags ?flags ~rex ?pos ?max ?callout subj =
  try
    Ok (Pcre2.split ?iflags ?flags ~rex:rex.regexp ?pos ?max ?callout subj)
  with
  | Pcre2.Error err -> Error err

let full_split ?iflags ?flags ~rex ?pos ?max ?callout subj =
  try
    Ok (Pcre2.full_split ?iflags ?flags ~rex:rex.regexp ?pos ?max ?callout subj)
  with
  | Pcre2.Error err -> Error err

let log_error rex subj err =
  let string_fragment =
    let len = String.length subj in
    if len < 200 then subj
    else sprintf "%s ... (%i bytes)" (Str.first_chars subj 200) len
  in
  Log.warn (fun m ->
      m "PCRE error: %a on input %S. Source regexp: %S" pp_error err
        string_fragment rex.pattern)

let pmatch_noerr ?iflags ?flags ~rex ?pos ?callout ?(on_error = false) subj =
  match pmatch ?iflags ?flags ~rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error rex subj err;
      on_error

let exec_noerr ?iflags ?flags ~rex ?pos ?callout subj =
  match exec ?iflags ?flags ~rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error rex subj err;
      None

let exec_all_noerr ?iflags ?flags ~rex ?pos ?callout subj =
  match exec_all ?iflags ?flags ~rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error rex subj err;
      [||]

let split_noerr ?iflags ?flags ~rex ?pos ?max ?callout ~on_error subj =
  match split ?iflags ?flags ~rex ?pos ?max ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error rex subj err;
      on_error

let string_of_exn (e : exn) =
  match e with
  | Pcre2.Error error -> Some (sprintf "Pcre2.Error(%s)" (show_error error))
  | Pcre2.Backtrack -> Some "Pcre2.Backtrack"
  | Pcre2.Regexp_or (pat, error) ->
      Some (sprintf "Pcre2.Regexp_or(pat=%S, %s)" pat (show_error error))
  | _not_from_pcre -> None

(*
   You can test this with:

     $ dune utop
     # SPcre2.register_exception_printer ();;
     # Pcre2.pmatch ~pat:"(a+)+$" "aaaaaaaaaaaaaaaaaaaaaaaaaa!"
         |> assert false
       with e -> Printexc.to_string e;;
     - : string = "Pcre2.Error(MatchLimit)"

   See Exception.mli for notes on exception printer registration.
*)
let register_exception_printer () = Printexc.register_printer string_of_exn

let substitute ?iflags ?flags ~rex ?pos ?callout ~subst subj =
  Pcre2.substitute ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~subst subj

let replace ?iflags ?flags ~rex ?pos ?callout ~template subj =
  let itempl = Pcre2.subst template in
  Pcre2.replace ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~itempl subj

let replace_first ?iflags ?flags ~rex ?pos ?callout ~template subj =
  let itempl = Pcre2.subst template in
  Pcre2.replace_first ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~itempl subj

let extract_all ?iflags ?flags ~rex ?pos ?full_match ?callout subj =
  Pcre2.extract_all ?iflags ?flags ~rex:rex.regexp ?pos ?full_match ?callout
    subj

(*****************************************************************************)
(* Subpattern extraction *)
(*****************************************************************************)

let get_substring rex substrings n =
  try Ok (Some (Pcre2.get_substring substrings n)) with
  | Not_found -> Ok None
  | Invalid_argument msg ->
      Error (sprintf "Invalid argument: %s\nSource pattern: %S" msg rex.pattern)

let get_named_substring_and_ofs rex name substrings =
  try
    let substring = Pcre2.get_named_substring rex.regexp name substrings in
    let ofs = Pcre2.get_named_substring_ofs rex.regexp name substrings in
    Ok (Some (substring, ofs))
  with
  | Not_found -> Ok None
  | Invalid_argument msg ->
      Error (sprintf "Invalid argument: %s\nSource pattern: %S" msg rex.pattern)

let quote = Pcre2.quote

(* Formerly Regexp_engine *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper to the actual regexp engine(s).
 *
 * Regexps are used in many places in Semgrep:
 *  - in Generic_vs_generic to support the "=~/.../",
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
 *  - move the regexp-related code in Generic_vs_generic here!
 *  - use Re.Glob just for globbing?
 *
 *)

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

let pcre_pattern (x : t) = x.pattern
let pcre_regexp (x : t) = x.regexp
let show (x : t) = x.pattern
let pp fmt (x : t) = Format.fprintf fmt "\"%s\"" x.pattern
let equal (x1 : t) (x2 : t) = x1.pattern = x2.pattern
let matching_exact_string s : t = regexp (quote s)

let matching_exact_word s =
  let pattern = "\b" ^ quote s ^ "\b" in
  regexp pattern

let pcre_compile_with_flags ~flags pat = regexp ~flags pat [@@profiling]

(*
   MULTILINE = ^ and $ match at the beginning and end of lines rather than
               just at the beginning and end of input.
*)
let pcre_compile pat = regexp ~flags:[ `MULTILINE ] pat [@@profiling]

let anchored_match ?on_error =
  (* ~iflags are precompiled flags for better performance compared to ~flags *)
  let iflags = Pcre2.rflags [ `ANCHORED ] in
  fun rex str -> pmatch_noerr ?on_error ~iflags ~rex str

let unanchored_match ?on_error rex str = pmatch_noerr ?on_error ~rex str

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
  let rex = regexp {|[$^]|\\[AZz]|\(\?<!|\(\?!|} in
  fun s -> pmatch_noerr ~rex s

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

let remove_end_of_string_assertions (rex : t) =
  match remove_end_of_string_assertions_from_string rex.pattern with
  | None -> None
  | Some pat -> Some (pcre_compile pat)
