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
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Grouping of taint rules. The main observation we are exploiting is the fact
 * many taint rules share similar sanitizers and propagators. When this happens,
 * the dataflow "flows" in the same ways for all the similar rules. We can
 * perform some optimizations by grouping together such rules and running
 * dataflow once on the entire group, and then specializing results to create
 * the signatures of the individual rules.
 *
 * The usage of this module is intended to be relatively limited, and is only
 * designed for deep inter-file tainting.
 *
 * This is only really an optimization because we generate a lot of rules with
 * syntactically equivalent sanitizers and propagators via jsonnet and TARS.
 *
 * Some observations. Many Semgrep rules have the same propagators and sanitizers.
 * So as of 2025/06/28, one group has 400+ rules. If tainting ever becomes
 * multi-threaded, it might be worth breaking up the groups.
 *)

type t
(** Represents a rule group.

    Invariants: always has at least one rule, rules
    have the same sanitizers and propagators. *)

val hook_group_taint_rules : bool Hook.t
(** If [hook_group_taint_rules] is set to [true], then we group taint rules
    together. Default is [false].*)

val rules : t -> Rule.taint_rule list
(** Get the rules of a rule group. *)

val group_rules : Rule.taint_rule list -> t list
(** [group_rules taint_rules] creates a list of rule groups with same sanitizers
    and propagators. *)

val length : t -> int
(** [length group] returns the number of rules in a rule group. O(1) operation. *)

val singleton : Rule.taint_rule -> t
(** [singleton rule] creates a rule group with a single rule. *)

val first_rule : t -> Rule.taint_rule
(** [first_rule group] returns the first rule of a rule group. It can be used as
    a representative of the group. *)

val fold_preds : Taint_spec_preds.t list -> Taint_spec_preds.t
(** [fold_preds preds] returns a new set of predicates that is the union of
    the given predicates.
    Pre: all the predicates come from the same rule group. *)
