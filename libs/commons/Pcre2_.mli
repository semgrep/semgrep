(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Wrappers for using the Pcre2 module safely with settings that make
   sense for semgrep such as automatically setting some flags and
   handling exceptions.

   If you need a function from Pcre2 that is not being exposed by this module,
   please add it.
*)

(*
   The type holding the source pattern and a compiled regexp.

   Note that the default 'equal' function is based only on the source
   patterns and doesn't take into account compilation options.
*)
type t = private { pattern : string; regex : Pcre2.Interp.t }
[@@deriving show, eq, hash, ord, sexp_of]

type match_ = Pcre2.match_ [@@deriving show]
(** A single (possibly named) group's match: its text and position within the
    subject. Group 0 is the whole match. *)

type captures = Pcre2.captures [@@deriving show]
(** All the groups captured by one successful match of a pattern against a
    subject. Individual groups are extracted with [match_of_captures] and
    [named_match_of_captures]. *)

type range = Pcre2.Interp.range = { start : int; end_ : int } [@@deriving show]
(** Byte offsets of a match within the subject: [start] is inclusive, [end_]
    is exclusive. *)

(*****************************************************************************)
(* Compilation *)
(*****************************************************************************)

val compile :
  ?options:Pcre2.Interp.compile_option list ->
  string ->
  (t, Pcre2.compile_error) Result.t
(** [compile ?options pattern] compiles [pattern] with [`UTF],
    [`MATCH_LIMIT match_limit], and [`DEPTH_LIMIT depth_limit], in addition to
    [options]. *)

val compile_exn : ?options:Pcre2.Interp.compile_option list -> string -> t
(** Like {!compile}, but raises [Invalid_argument] with a message containing
    the pattern and the pretty-printed compile error. Intended for patterns
    which are statically known to be valid (e.g. literals), so that a typo
    fails with an actionable message rather than a bare
    [Result.get_ok] failure. *)

val show_compile_error : Pcre2.compile_error -> string
(** Human-readable rendering of a compile error (pcre2's own error message)
    and the position (byte offset into the pattern) at which it occurred,
    e.g. ["missing closing parenthesis at position 4"]. *)

val match_limit : int
(** Cap on the number of internal match calls per match attempt (exceeding it
    yields [Error MATCHLIMIT]). Baked into every regex from {!compile}; see
    the comment in the implementation for why. *)

val depth_limit : int
(** Cap on the depth of nested backtracking per match attempt (exceeding it
    yields [Error DEPTHLIMIT]). Baked into every regex from {!compile}. *)

val quote : string -> string
(** [quote str] escapes PCRE metacharacters in [str]. Use it when constructing
    larger patterns from literal fragments; for a wholly literal pattern, use
    {!matching_exact_string}. *)

val matching_exact_string : string -> t
(** [matching_exact_string str] matches [str] literally. *)

val matching_exact_word : string -> t
(** [matching_exact_word str] matches [str] literally between word
    boundaries. *)

(*****************************************************************************)
(* Matching *)
(*****************************************************************************)

val is_match :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  t ->
  string ->
  (bool, Pcre2.match_error) Result.t
(** [is_match rex subject] is whether [rex] matches anywhere in [subject]. *)

val unanchored_match : t -> string -> (bool, Pcre2.match_error) Result.t
(** [unanchored_match rex subject] is an alias for {!is_match} that emphasizes
    its unanchored semantics. *)

val find :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  t ->
  string ->
  (match_ option, Pcre2.match_error) Result.t
(** [find rex subject] is the first match of [rex] in [subject] (starting at
    [subject_offset] if given), or [None] if there is no match. Use this
    rather than {!captures} when only the whole match is needed. *)

val find_iter :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  t ->
  string ->
  (match_, Pcre2.match_error) Result.t Seq.t
(** [find_iter rex subject] is the sequence of non-overlapping matches of
    [rex] in [subject], left to right. If a match attempt fails with an
    error, the sequence yields one [Error] and then terminates. *)

val captures :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  t ->
  string ->
  (captures option, Pcre2.match_error) Result.t
(** [captures rex subject] is the capture groups of the first match of [rex]
    in [subject], or [None] if there is no match. *)

val captures_iter :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  t ->
  string ->
  (captures, Pcre2.match_error) Result.t Seq.t
(** [captures_iter rex subject] is like {!find_iter} but yields the capture
    groups of each match. *)

(*****************************************************************************)
(* Splitting and replacement *)
(*****************************************************************************)

val split :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  ?limit:int ->
  t ->
  string ->
  (string list, Pcre2.match_error) Result.t
(** [split rex subject] splits [subject] on matches of [rex]. [limit] bounds
    the number of resulting fields. Note that unlike Perl-style split,
    trailing empty fields are kept. *)

val replace_fn :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  ?limit:int ->
  t ->
  (string -> string) ->
  string ->
  (string, Pcre2.match_error) Result.t
(** [replace_fn rex f subject] replaces each match of [rex] in [subject]
    (at most [limit] matches, if given) with the result of applying [f] to
    the matched substring. *)

val replace_captures_fn :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  ?limit:int ->
  t ->
  (captures -> string) ->
  string ->
  (string, Pcre2.match_error) Result.t
(** Like {!replace_fn}, but the replacement function receives the full
    captures of each match rather than just the matched substring, so the
    replacement can refer to capture groups. *)

val replace :
  ?options:Pcre2.Interp.match_option list ->
  ?subject_offset:int ->
  ?limit:int ->
  t ->
  template:string ->
  string ->
  (string, Pcre2.match_error) Result.t
(** [replace rex ~template subject] replaces each match of [rex] in [subject]
    with [template]. Numbered capture references may use either [\\1] or [$1];
    [$$] produces a literal dollar sign. A reference to a group that did not
    participate, or does not exist, expands to the empty string. [limit] bounds
    the number of matches replaced. *)

(*****************************************************************************)
(* Match data *)
(*****************************************************************************)

val range_of_match : match_ -> range
(** [range_of_match match_] is the byte range of [match_]. *)

val range_of_captures : captures -> range
(** [range_of_captures captures] is the byte range of the whole match. *)

val substring_of_match : match_ -> string
(** [substring_of_match match_] is the matched substring. *)

val match_of_captures : captures -> int -> match_ option
(** [match_of_captures captures group] is numbered [group], if it
    participated. *)

val named_match_of_captures : captures -> string -> match_ option
(** [named_match_of_captures captures name] is group [name], if it
    participated. *)

val substring_of_captures : captures -> int -> string option
(** [substring_of_captures captures group] is the substring captured by
    numbered [group], if it participated. *)

val named_substring_of_captures : captures -> string -> string option
(** [named_substring_of_captures captures name] is the substring captured by
    group [name], if it participated. *)

val capture_groups : t -> (string * int) list
(** The named capture groups of the pattern, as (name, group number) pairs. *)

val captures_length : captures -> int
(** The number of groups in [captures], including group 0 (the whole match),
    i.e., one more than the highest capture-group number, whether or not each
    group participated in the match. *)

(*****************************************************************************)
(* Pattern rewriting *)
(*****************************************************************************)

val remove_end_of_string_assertions : t -> t option
(** [remove_end_of_string_assertions rex] removes leading and trailing
    beginning/end assertions so [rex] can conservatively be applied to a whole
    target file instead of a substring. It returns [None] when it cannot prove
    that all relevant assertions were removed. *)

val remove_end_of_string_assertions_from_string : string -> string option
(** String-level implementation of {!remove_end_of_string_assertions}, exposed
    for testing. *)
