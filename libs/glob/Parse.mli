(*
   Parse a glob pattern.
*)

val parse_string : ?deprecated_absolute_dotslash:bool -> string -> Pattern.t
(** Parse a glob pattern according to
    {{: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_19_14 }
      [glob(7)] }, which is the POSIX standard for old-fashioned shell globbing
      patterns for matching file paths. Additionally, we support [**] as per
      the {{: https://git-scm.com/docs/gitignore } gitignore specification}.

    deprecated_absolute_dotslash: should be false according to the standard.
    If true, it causes the pattern './a' to be parsed as '/a'.
    It is a deprecated option provided for backward compatibility
    with Semgrepignore v1 behavior.
*)
