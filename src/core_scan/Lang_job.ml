(* All rules and targets applicable to a specific language.
   This is passed directly by the new osemgrep implementation, not
   from the semgrep-core command line.

   TODO? not sure we need this intermediate data-structure for the
   osemgrep/semgrep-core communication, but this is useful at least
   for the Scan status report (see Status_report.ml)

   related code:
    - interfaces/Input_to_core.atd (used for semgrep-core -target)
*)
type t = { analyzer : Analyzer.t; targets : Fpath.t list; rules : Rule.t list }
