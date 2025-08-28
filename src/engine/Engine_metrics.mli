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
module Prefilter_metrics : sig
  val record_rules_processed : analyzer:Analyzer.t -> int -> unit
  (** When prefiltering is done, the number of rules (for a given target)
      which a prefilter was applied for (i.e., we generated and tested the
      prefilter against some target) *)

  val record_rules_skipped : analyzer:Analyzer.t -> int -> unit
  (** When prefiltering is done, the number of rules (for a given target)
      which we were able to skip execution of due to prefiltering. *)
end
