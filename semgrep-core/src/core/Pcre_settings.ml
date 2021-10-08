(*
   Shared settings for using the Pcre module (pcre-ocaml library).
*)

open Printf

(*
   'limit' and 'limit_recursion' are set explicitly to make semgrep
   fail consistently across platforms (e.g. CI vs. local Mac).
   The default compile-time defaults are 10_000_000 for both
   'limit' and 'limit_recursion' but they can be overridden during
   the installation of the pcre library. We protect ourselves
   from such custom installs.
*)
let regexp ?study ?iflags ?flags ?chtables pat =
  Pcre.regexp ?study ~limit:10_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT *)
    ~limit_recursion:10_000_000 (* sets PCRE_EXTRA_MATCH_LIMIT_RECURSION *)
    ?iflags ?flags ?chtables pat

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
     # Pcre_settings.register_exception_printer ();;
     # Pcre.pmatch ~pat:"(a+)+$" "aaaaaaaaaaaaaaaaaaaaaaaaaa!"
         |> assert false
       with e -> Printexc.to_string e;;
     - : string = "Pcre.Error(MatchLimit)"
*)
let register_exception_printer () = Printexc.register_printer string_of_exn
