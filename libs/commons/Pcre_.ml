(*
   Shared settings for using the Pcre2 module (pcre-ocaml library).
*)

open Printf

(* Keep the regexp source around for better error reporting and
   troubleshooting. *)
type t = {
  pattern : string;
  regexp : Pcre2.regexp; [@opaque] [@equal fun _ _ -> true]
}
[@@deriving show, eq]

let tags = Logs_.create_tags [ __MODULE__ ]

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

(* TODO: why do we have this plus a derivied show? *)
let string_of_error (error : Pcre2.error) =
  match error with
  | Partial -> "Partial"
  | BadPattern (msg, pos) -> sprintf "Pcre2.BadPattern(%S, pos=%i)" msg pos
  | BadUTF -> "BadUTF"
  | BadUTFOffset -> "BadUTFOffset"
  | MatchLimit -> "MatchLimit"
  | DepthLimit -> "DepthLimit"
  | WorkspaceSize -> "WorkspaceSize"
  | InternalError msg -> sprintf "InternalError(%S)" msg

let log_error rex subj err =
  let string_fragment =
    let len = String.length subj in
    if len < 200 then subj
    else sprintf "%s ... (%i bytes)" (Str.first_chars subj 200) len
  in
  Logs.warn (fun m ->
      m ~tags "PCRE error: %s on input %S. Source regexp: %S"
        (string_of_error err) string_fragment rex.pattern)

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
  | Pcre2.Error error ->
      Some (sprintf "Pcre2.Error(%s)" (string_of_error error))
  | Pcre2.Backtrack -> Some "Pcre2.Backtrack"
  | Pcre2.Regexp_or (pat, error) ->
      Some (sprintf "Pcre2.Regexp_or(pat=%S, %s)" pat (string_of_error error))
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

let get_named_substring_and_ofs rex name substrings =
  try
    let substring = Pcre2.get_named_substring rex.regexp name substrings in
    let ofs = Pcre2.get_named_substring_ofs rex.regexp name substrings in
    Ok (Some (substring, ofs))
  with
  | Not_found -> Ok None
  | Invalid_argument msg ->
      Error (sprintf "Invalid argument: %s\nSource pattern: %S" msg rex.pattern)
