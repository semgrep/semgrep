(* Check semgrep patterns for potential issues. *)
val check : Lang.t -> Pattern.t -> (* TODO Error_code.error list *) unit
