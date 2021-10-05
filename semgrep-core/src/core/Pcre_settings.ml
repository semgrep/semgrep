(*
   Shared settings for using the Pcre module (pcre-ocaml library).
*)

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
