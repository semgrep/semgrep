(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Runtime configuration options defining a regexp dialect.
*)

type t = {
  (* Match all characters with '.', including LF which otherwise is
     excluded. Same as PCRE_DOTALL. *)
  pcre_dotall : bool;
  (* If enabled, '^' and '$' will match only that the beginning and at the
     end of the input, respectively. Same as PCRE_MULTILINE. *)
  pcre_multiline : bool;
  (* If enabled, some sets of characters like '\w' or '[:alnum:]' are extended
     with non-ascii unicode characters. Same as PCRE_UCP. *)
  pcre_ucp : bool;
  (* Tweaks for JavaScript compatibility. Same as PCRE_JAVASCRIPT_COMPAT. *)
  pcre_javascript_compat : bool;
  (* Support comments in the form of '(?# ... )' *)
  with_comment_groups : bool;
  (* Ignore whitespace outside of character classes.
     Must use '\s' to match a space character etc.
     This corresponds to '/x' in perl and PCRE_EXTENDED in PCRE. *)
  ignore_whitespace : bool;
  (* Ignore whitespace in character classes. *)
  ignore_whitespace_in_char_classes : bool;
  (* Ignore any '#' character and what follows until the end of the line. *)
  ignore_hash_comments : bool;
}
