(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
