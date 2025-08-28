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
(* All rules and targets applicable to a specific language.
   This is passed directly by the new osemgrep implementation, not
   from the semgrep-core command line.

   TODO? not sure we need this intermediate data-structure for the
   osemgrep/semgrep-core communication, but this is useful at least
   for the Scan status report (see Status_report.ml)

   related code:
    - interfaces/Input_to_core.atd (used for semgrep-core -target)
*)
type t = { analyzer : Analyzer.t; targets : Fppath.t list; rules : Rule.t list }
