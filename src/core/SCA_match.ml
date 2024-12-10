type t = {
  (* the actual dependency in the lockfile *)
  dep : SCA_dependency.t;
  (* the version constraint on a package and its ecosystem *)
  pat : SCA_pattern.t;
  kind : kind;
}

and kind =
  (* Rule had both code patterns and dependency patterns, got matches on *both*,
   * the Pattern Match is in code, annotated with this dependency match *)
  | CodeAndLockfileMatch
  (* Rule had dependency patterns, they matched, the Pattern Match is in a
   * lockfile. So the range_loc of the Dependency.t in this dependency_match
   * should be *the same* as the range_loc in the PatternMatch.t
   *)
  | LockfileOnlyMatch
[@@deriving show, eq]
