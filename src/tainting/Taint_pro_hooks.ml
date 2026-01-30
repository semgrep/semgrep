(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)
type t = {
  find_attribute_in_class :
    AST_generic.name -> string -> AST_generic.name option;
      (** deep-scan hook *)
}

let hook_taint_pro_hooks : t option Hook.t = Hook.create None
