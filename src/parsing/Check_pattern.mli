(* Check semgrep patterns for potential issues. *)
val check : Lang.t -> Pattern.t -> (unit, string) Result.t
