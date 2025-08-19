module Prefilter_metrics : sig
  val record_rules_processed : analyzer:Analyzer.t -> int -> unit
  (** When prefiltering is done, the number of rules (for a given target)
      which a prefilter was applied for (i.e., we generated and tested the
      prefilter against some target) *)

  val record_rules_skipped : analyzer:Analyzer.t -> int -> unit
  (** When prefiltering is done, the number of rules (for a given target)
      which we were able to skip execution of due to prefiltering. *)
end
