(* Andre Kuhlenschmidt
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

type packed

type unpacked = {
  (* Old comment that is still relevant. In the following comment id_hidden
     used to be a boolean field of the id_info type. With the addition of
     another boolean field case_insensitive we switched to storing these flags
     in a bitfield refered to as id_info_flags. Old comment follows:

     id_hidden=true must be set for any artificial identifier that never
     appears in source code but is introduced in the AST after parsing.

     Don't use this for syntax desugaring or transpilation because the
     resulting function name might exist in some source code. Consider the
     following normalization:

       !foo -> foo.contents
                   ^^^^^^^^
                 should not be marked as hidden because it could appear
                 in target source code.

     However, an artificial identifier like "!sh_quoted_expand!" should
     be marked as hidden in bash.

     This allows not breaking the -fast/-filter_irrelevant_rules optimization
     that skips a target file if some identifier in the pattern AST doesn't
     exist in the source of the target.
  *)
  hidden : bool;
  (* When set, this flag indicates that equality for the associated
   * ident should not pay attention to differences in Upper versus
   * Lower case. This is useful for languages with case insensitive
   * identifiers such as php and apex.
   *)
  case_insensitive : bool;
}

val pack : unpacked -> packed
val unpack : packed -> unpacked
val is_hidden : packed -> bool
val is_case_insensitive : packed -> bool
val show_packed : packed -> string
val show_unpacked : unpacked -> string
val equal_packed : packed -> packed -> bool
val equal_unpacked : unpacked -> unpacked -> bool
val hash_packed : packed -> Base.Hash.hash_value
val pp_packed : packed Fmt.t
val hash_fold_packed : Base.Hash.state -> packed -> Base.Hash.state
