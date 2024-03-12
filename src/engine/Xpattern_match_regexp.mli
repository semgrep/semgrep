type ('regex, 'libregex, 'substring) re_functions = {
  get_pattern : 'regex -> string;
  get_regex : 'regex -> 'libregex;
  exec_all_noerr : 'regex -> string -> 'substring array;
  get_substring : 'substring -> int -> string;
  get_substring_ofs : 'substring -> int -> int * int;
  names : 'libregex -> string array;
  num_of_subs : 'substring -> int;
  get_named_substring_ofs : 'libregex -> string -> 'substring -> int * int;
  get_named_substring : 'libregex -> string -> 'substring -> string;
}

val pcre2_regex_functions :
  (Pcre2_.t, Pcre2.regexp, Pcre2.substrings) re_functions

val pcre_regex_functions : (Pcre_.t, Pcre.regexp, Pcre.substrings) re_functions
[@@alert "-deprecated"]
[@@ocaml.deprecated
  "PCRE is deprecated for new usage; it should only be used in existing \
   generic mode code."]

(* would label these arguments, but can't to make it work with
   the other generic matchers
*)
val regexp_matcher :
  ?base_offset:int ->
  ('regex, 'libregex, 'substring) re_functions ->
  (* str *) string ->
  Fpath.t ->
  'regex ->
  ((Tok.location * Tok.location) * (string * Metavariable.mvalue) list) list

val matches_of_regexs :
  (Pcre2_.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result
