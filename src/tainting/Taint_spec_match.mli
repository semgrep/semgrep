(** Taint spec-match(es)
 *
 * Matches of taint specifications (sources/propagators/sanitizers/sinks) *)

type overlap = float
(** A number in [0.0, 1.0], where 1.0 means that an AST node matches a taint spec
 * perfectly. For example, if `foo(...)` is a source spec, `foo(x)` would be
 * a perfect match, whereas the `x` in `foo(x)` would match the spec but loosely
 * (see note on "Taint-tracking via ranges" in 'Match_tainting_mode'.)
 *
 * In practice we interpret >0.99 as being the same as 1.0, see 'is_exact'. *)

type 'spec t = {
  spec : 'spec;
      (** The specification on which the match is based, e.g. a taint source.
      * This spec should have a pattern formula. *)
  spec_id : string;
      (** See 'source_id' etc in 'Rule' and 'Parse_rule', this is the very same id. *)
  spec_pm : Core_match.t;
      (** What the spec's pattern formula actually matches in the target file. *)
  range : Range.t;
      (** The range of this particular match, which will be a subrange of 'spec_pm',
      * see note on "Taint-tracking via ranges" in 'Match_tainting_mode.ml'. *)
  overlap : overlap;
      (** The overlap of this match ('range') with the spec match ('spec_pm'). *)
}
(** A sub-match for a taint spec, the 'range' is a subset of the range delimited by
 * the spec. This is more of a candidate, sometimes we are fine with a submatch, but
 * sometimes we want the "best match", see NOTE "Best matches". *)

val is_exact : 'spec t -> bool
(** An exact match, i.e. overlap 0.99. Typically useful for l-values. *)

val sink_of_match : Rule.taint_sink t -> Shape_and_sig.Effect.sink
val _show : 'spec t -> string

(** Any kind of spec-match (existential type). *)
type any = Any : 'a t -> any

(** In general we cannot guarantee truly exact (see 'is_exact') matches, e.g. some
 * token may have been "lost" or something in the Generic-to-IL translation. Since
 * we cannot rely on the 'overlap', we instead look for the "top-level" nodes in
 * the CFG, and record whether they match any taint spec in this data structure.
 * If one match is contained in another one, we only keep the _larges_ match.
 * For example, given `foo(...)` then `foo(x)` will be recorded as a top-level
 * match, whereas `foo` or `x` will not. *)
module Best_matches : sig
  type t

  val _debug : t -> string
end

val is_best_match : Best_matches.t -> 'spec t -> bool
(** Similar to 'is_exact' but based on "top matches". *)

val best_matches_in_nodes :
  sub_matches_of_orig:(IL.orig -> any Seq.t) -> IL.fun_cfg -> Best_matches.t
(** Collect the top-level matches in a CFG. *)
