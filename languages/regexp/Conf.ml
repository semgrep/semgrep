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
  pcre_dotall : bool;
  pcre_multiline : bool;
  pcre_ucp : bool;
  pcre_javascript_compat : bool;
  with_comment_groups : bool;
  ignore_whitespace : bool;
  ignore_whitespace_in_char_classes : bool;
  ignore_hash_comments : bool;
}
