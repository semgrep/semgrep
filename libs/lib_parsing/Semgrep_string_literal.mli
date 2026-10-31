(** Semgrep-specific string-literal syntax shared by parsers and matchers. *)

val regexp : string
(** Recognition regexp for the legacy [=~/.../] syntax, with captures for the
    regexp body and optional flag. *)

val is_regexp : string -> bool
(** Whether a literal contains a legacy Semgrep regexp pattern. *)
