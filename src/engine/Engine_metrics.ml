(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module Prefilter_metrics = struct
  (** Metrics for prefiltering. The primary goal of these is to be able to
      measure (1) our ability to generate and utilize prefilters and (2) the
      selectivity of the prefilters we are able to generate. So we want to be
      able to understand how many rules we're able to apply a prefilter for,
      and the number of rules we can actaully skip complete execution for due to
      a prefilter. *)

  module Meter =
    (val Ometrics.make_meter
           Ometrics.
             {
               (* TODO(austin): Should be Some "file_prefiltering" but needs
                  fix for meter names first *)
               name = None;
               attrs = [];
             })

  module RulesProcessed =
    (val Meter.make_int_counter
         @@ Ometrics.make_instrument_meta
              ~name:"semgrep_file_prefilter_rules_processed"
              ~description:
                "Number of rules checked on a given target with a prefilter"
              ~unit_:"{rule}" ())

  module RulesSkipped =
    (val Meter.make_int_counter
         @@ Ometrics.make_instrument_meta
              ~name:"semgrep_file_prefilter_rules_skipped"
              ~description:
                "Number of rules skipped on a given target due to a prefilter"
              ~unit_:"{rule}" ())

  let record_rules_processed ~analyzer n =
    RulesProcessed.record
      ~attrs:[ ("analyzer", `String (Analyzer.to_string analyzer)) ]
      n

  let record_rules_skipped ~analyzer n =
    RulesSkipped.record
      ~attrs:[ ("analyzer", `String (Analyzer.to_string analyzer)) ]
      n
end
