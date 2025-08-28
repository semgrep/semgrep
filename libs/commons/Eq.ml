(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let phys_equal = ( == )
let phys_not_equal = ( != )

type hidden_by_your_nanny = unit

module Operators = struct
  let ( =|= ) : int -> int -> bool = ( = )
  let ( =$= ) : char -> char -> bool = ( = )
  let ( =:= ) : bool -> bool -> bool = ( = )

  (* dangerous, do not use, see the comment in Common.mli *)
  let ( =*= ) = ( = )

  (* To forbid people to use the polymorphic '='.
   * See https://blog.janestreet.com/the-perils-of-polymorphic-compare/
   *)
  let ( = ) = String.equal
  let ( == ) : hidden_by_your_nanny = ()
  let ( != ) : hidden_by_your_nanny = ()
end
