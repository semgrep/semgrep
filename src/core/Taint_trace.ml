(* Iago Abal, Nat Mote
 *
 * Copyright (C) 2022-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Taint traces to help understand semgrep findings (especially useful
 * when in interfile mode).
 *
 * Note that those "core" taint traces are translated at some point in
 * Semgrep_output_v1.match_dataflow_trace
 *
 * See also Matching_explanation.ml for a complementary way to explain
 * findings related to formula composition. The taint traces, as
 * the name suggest, are to explain the flow of taints through different
 * variable assignments and function calls.
 *
 * Do not confuse those traces with our telemetry traces in Trace_data.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* The locations of variables which taint propagates through *)
type tainted_tokens = Tok.t list [@@deriving show, eq]

(* The tokens associated with a single pattern match involved in a taint trace *)
type pattern_match_tokens = Tok.t list [@@deriving show, eq]

(* Simplified version of Taint.source_to_sink meant for finding reporting *)
type call_trace =
  (* A direct match *)
  | Toks of pattern_match_tokens
  (* An indirect match through a function call *)
  | Call of {
      call_toks : pattern_match_tokens;
      intermediate_vars : tainted_tokens;
      call_trace : call_trace;
    }
[@@deriving show, eq]

(* The trace of a single source of taint, to the sink.
   There may be many of these, taking different paths. For a single
   sink, the fact that it produces a finding might be the product of
   many taints, due to labels.
   These taints may also take their own paths, because they might arrive
   via different variables.
*)
type item = {
  source_trace : call_trace;
      (** This is the path that the taint takes, from the source, to get to
        the current function in which the taint finding is reported. *)
  tokens : tainted_tokens;
      (** This is the path taken within the current function, to link the
        taint source obtained earlier with a sink. Both of these might
        be done through a chain of function calls. *)
  sink_trace : call_trace;
      (** This is the path that the taint takes, from the function context,
        to get to the sink. *)
}
[@@deriving show, eq]

type t = item list [@@deriving show, eq]
