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
type unpacked = { hidden : bool; case_insensitive : bool }

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
