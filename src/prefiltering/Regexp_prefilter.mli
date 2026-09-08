(*
   Copyright (C) Semgrep Inc.

   This library is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License version 2.1 as
   published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE for more details.
*)
(** Extract required literal substrings from a regexp, for use as a prefilter.

    The regexp is parsed and analyzed to find literals that must appear in any
    string it matches. For example,
    {v
    (?<KEY>\b((AKIA|ABIA|ACCA)[0-9A-Z]{16})\b)
    v}
    yields [Or [String "AKIA"; String "ABIA"; String "ACCA"]]: any match must
    contain one of those literals, so a file containing none of them can be
    skipped without running the regexp.

    See the module implementation for the extraction algorithm. *)

val required_substrings : string -> Predicate.t Formula.t option
(** [required_substrings re] returns a formula of literal substrings that must
    be present for the regexp [re] to match, or [None] when no useful (i.e.
    sufficiently long) condition can be extracted. The returned formula is
    always a *necessary* condition for [re] to match, hence sound to use as a
    prefilter, though it may be weaker than [re] itself. *)
