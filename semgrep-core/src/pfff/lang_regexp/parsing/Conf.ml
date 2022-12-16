(*
   Runtime configuration options defining a regexp dialect.
*)

type t = {
  pcre_dotall: bool;
  pcre_multiline: bool;
  pcre_ucp: bool;
  pcre_javascript_compat: bool;
  with_comment_groups: bool;
  ignore_whitespace: bool;
  ignore_whitespace_in_char_classes: bool;
  ignore_hash_comments: bool;
}
