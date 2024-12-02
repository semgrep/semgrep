type t = SCA_dependency.t * SCA_pattern.t [@@deriving show, eq]

(* alt: use a record as both constructors have the same type *)
type kind =
  (* Rule had both code patterns and dependency patterns, got matches on *both*,
   * the Pattern Match is in code, annotated with this dependency match *)
  | CodeAndLockfileMatch of t
  (* Rule had dependency patterns, they matched, the Pattern Match is in a
   * lockfile. So the range_loc of the Dependency.t in this dependency_match
   * should be *the same* as the range_loc in the PatternMatch.t
   *)
  | LockfileOnlyMatch of t
[@@deriving show, eq]
