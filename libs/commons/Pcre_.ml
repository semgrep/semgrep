(* Martin Jambon

   (c) 2019 Semgrep, Inc.

   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License version 2.1 as
   published by the Free Software Foundation, with the special exception on
   linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE for more details.
*)

(*
   Shared settings for using the Pcre module (pcre-ocaml library).
*)

open Printf

(* Keep the regexp source around for better error reporting and
   troubleshooting. *)
type t = {
  pattern : string;
  regexp : Pcre.regexp; [@opaque] [@equal fun _ _ -> true]
}
[@@deriving show, eq]

let src = Logs.Src.create "commons.pcre"

module Log = (val Logs.src_log src : Logs.LOG)

(*
   Provide missing error->string conversion
*)
type error = Pcre.error =
  | Partial
  | BadPartial
  | BadPattern of string * int
  | BadUTF8
  | BadUTF8Offset
  | MatchLimit
  | RecursionLimit
  | WorkspaceSize
  | InternalError of string
[@@deriving show]

(*
   Flag required for the following to succeed:
     Pcre.regexp ~flags:[`UTF8] "\\x{200E}"
*)
let extra_flag = `UTF8

(*
   'limit' and 'limit_recursion' are set explicitly to make semgrep
   fail consistently across platforms (e.g. CI vs. local Mac).
   The default compile-time defaults are 10_000_000 for both
   'limit' and 'limit_recursion' but they can be overridden during
   the installation of the pcre library. We protect ourselves
   from such custom installs.
*)
let regexp ?study ?iflags ?(flags = []) ?chtables pat =
  (* pcre doesn't mind if a flag is duplicated so we just append extra flags *)
  let flags = extra_flag :: flags in
  (* OCaml's Pcre library does not support setting timeouts, and since it's just
   * a wrapper for a C library `Common.set_timeout` doesn't work... So, we set a
   * lower `limit` and `limit_recursion` (default values are 10_000_000) to avoid
   * spending too much time on regex matching. See perf/input/semgrep_targets.txt
   * and perf/input/semgrep_targets.yaml for an example where Semgrep appeared to
   * hang (but it was just the Pcre engine taking way too much time). *)
  let regexp =
    Pcre.regexp ?study ~limit:1_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT *)
      ~limit_recursion:1_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT_RECURSION *)
      ?iflags ~flags ?chtables pat
  in
  { pattern = pat; regexp }

let pmatch ?iflags ?flags ~rex ?pos ?callout subj =
  try Ok (Pcre.pmatch ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj) with
  | Pcre.Error err -> Error err

let exec ?iflags ?flags ~rex ?pos ?callout subj =
  try
    Ok (Some (Pcre.exec ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj))
  with
  | Not_found -> Ok None
  | Pcre.Error err -> Error err

let exec_all ?iflags ?flags ~rex ?pos ?callout subj =
  try Ok (Pcre.exec_all ?iflags ?flags ~rex:rex.regexp ?pos ?callout subj) with
  | Not_found -> Ok [||]
  | Pcre.Error err -> Error err

let exec_to_strings ?iflags ?flags ~rex ?pos ?callout subj =
  match exec_all ?iflags ?flags ~rex ?pos ?callout subj with
  | Ok a -> Ok (Array.map Pcre.get_substrings a)
  | Error _ as e -> e

let split ?iflags ?flags ~rex ?pos ?max ?callout subj =
  try
    Ok (Pcre.split ?iflags ?flags ~rex:rex.regexp ?pos ?max ?callout subj)
  with
  | Pcre.Error err -> Error err

let full_split ?iflags ?flags ~rex ?pos ?max ?callout subj =
  try
    Ok (Pcre.full_split ?iflags ?flags ~rex:rex.regexp ?pos ?max ?callout subj)
  with
  | Pcre.Error err -> Error err

let string_of_error (error : Pcre.error) =
  match error with
  | Partial -> "Partial"
  | BadPartial -> "BadPartial"
  | BadPattern (msg, pos) -> sprintf "Pcre.BadPattern(%S, pos=%i)" msg pos
  | BadUTF8 -> "BadUTF8"
  | BadUTF8Offset -> "BadUTF8Offset"
  | MatchLimit -> "MatchLimit"
  | RecursionLimit -> "RecursionLimit"
  | WorkspaceSize -> "WorkspaceSize"
  | InternalError msg -> sprintf "InternalError(%S)" msg

let log_error rex subj err =
  let string_fragment =
    let len = String.length subj in
    if len < 200 then subj
    else sprintf "%s ... (%i bytes)" (Str.first_chars subj 200) len
  in
  Log.err (fun m ->
      m "PCRE error: %s on input %S. Source regexp: %S" (string_of_error err)
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
  | Pcre.Error error -> Some (sprintf "Pcre.Error(%s)" (string_of_error error))
  | Pcre.Backtrack -> Some "Pcre.Backtrack"
  | Pcre.Regexp_or (pat, error) ->
      Some (sprintf "Pcre.Regexp_or(pat=%S, %s)" pat (string_of_error error))
  | _not_from_pcre -> None

(*
   You can test this with:

     $ dune utop
     # SPcre.register_exception_printer ();;
     # Pcre.pmatch ~pat:"(a+)+$" "aaaaaaaaaaaaaaaaaaaaaaaaaa!"
         |> assert false
       with e -> Printexc.to_string e;;
     - : string = "Pcre.Error(MatchLimit)"

   See Exception.mli for notes on exception printer registration.
*)
let register_exception_printer () = Printexc.register_printer string_of_exn

let substitute ?iflags ?flags ~rex ?pos ?callout ~subst subj =
  Pcre.substitute ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~subst subj

let replace ?iflags ?flags ~rex ?pos ?callout ~template subj =
  let itempl = Pcre.subst template in
  Pcre.replace ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~itempl subj

let replace_first ?iflags ?flags ~rex ?pos ?callout ~template subj =
  let itempl = Pcre.subst template in
  Pcre.replace_first ?iflags ?flags ~rex:rex.regexp ?pos ?callout ~itempl subj

let extract_all ?iflags ?flags ~rex ?pos ?full_match ?callout subj =
  Pcre.extract_all ?iflags ?flags ~rex:rex.regexp ?pos ?full_match ?callout subj

let get_named_substring_and_ofs rex name substrings =
  try
    let substring = Pcre.get_named_substring rex.regexp name substrings in
    let ofs = Pcre.get_named_substring_ofs rex.regexp name substrings in
    Ok (Some (substring, ofs))
  with
  | Not_found -> Ok None
  | Invalid_argument msg ->
      Error (sprintf "Invalid argument: %s\nSource pattern: %S" msg rex.pattern)

let quote = Pcre.quote
