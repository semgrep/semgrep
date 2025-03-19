val parse_string : string -> Pattern.t
(** Parse a glob pattern according to
    {{: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/V3_chap02.html#tag_19_14 }
      [glob(7)] }, which is the POSIX standard for old-fashioned shell globbing
      patterns for matching file paths. Additionally, we support [**] as per
      the {{: https://git-scm.com/docs/gitignore } gitignore specification}. *)
