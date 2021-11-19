(*
   Shared settings for using the Pcre module (pcre-ocaml library).
*)

open Printf

let logger = Logging.get_logger [ __MODULE__ ]

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
  Pcre.regexp ?study ~limit:10_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT *)
    ~limit_recursion:10_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT_RECURSION *)
    ?iflags ~flags ?chtables pat

let pmatch ?iflags ?flags ?rex ?pos ?callout subj =
  try Ok (Pcre.pmatch ?iflags ?flags ?rex ?pos ?callout subj)
  with Pcre.Error err -> Error err

let exec ?iflags ?flags ?rex ?pos ?callout subj =
  try Ok (Some (Pcre.exec ?iflags ?flags ?rex ?pos ?callout subj)) with
  | Not_found -> Ok None
  | Pcre.Error err -> Error err

let exec_all ?iflags ?flags ?rex ?pos ?callout subj =
  try Ok (Pcre.exec_all ?iflags ?flags ?rex ?pos ?callout subj) with
  | Not_found -> Ok [||]
  | Pcre.Error err -> Error err

let split ?iflags ?flags ?rex ?pos ?max ?callout subj =
  try Ok (Pcre.split ?iflags ?flags ?rex ?pos ?max ?callout subj)
  with Pcre.Error err -> Error err

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

let log_error subj err =
  let string_fragment =
    let len = String.length subj in
    if len < 200 then subj
    else sprintf "%s ... (%i bytes)" (String.sub subj 0 200) len
  in
  logger#error "PCRE error: %s on input %S" (string_of_error err)
    string_fragment

let pmatch_noerr ?iflags ?flags ?rex ?pos ?callout subj =
  match pmatch ?iflags ?flags ?rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error subj err;
      false

let exec_noerr ?iflags ?flags ?rex ?pos ?callout subj =
  match exec ?iflags ?flags ?rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error subj err;
      None

let exec_all_noerr ?iflags ?flags ?rex ?pos ?callout subj =
  match exec_all ?iflags ?flags ?rex ?pos ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error subj err;
      [||]

let split_noerr ?iflags ?flags ?rex ?pos ?max ?callout ~on_error subj =
  match split ?iflags ?flags ?rex ?pos ?max ?callout subj with
  | Ok res -> res
  | Error err ->
      log_error subj err;
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
*)
let register_exception_printer () = Printexc.register_printer string_of_exn
