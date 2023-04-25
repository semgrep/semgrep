(* All rules and targets applicable to a specific language.
   This is passed directly by the new osemgrep implementation, not
   from the semgrep-core command line.
*)
type t = { lang : Xlang.t; targets : Fpath.t list; rules : Rule.t list }
