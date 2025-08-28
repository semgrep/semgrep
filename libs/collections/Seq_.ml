(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let pp open_bracket close_bracket pp_elt fmt seq =
  let pp_comma fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "%s%a%s" open_bracket
    (Format.pp_print_seq ~pp_sep:pp_comma pp_elt)
    seq close_bracket
