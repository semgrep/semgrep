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
val repr : ?unicode_version:int * int -> string -> string
(** [repr s] is the representation of [s] as a Python escaped string. That is,
    the output of {[str.__repr__(s)]} in Python. [s] is assumed to be a UTF-8 encoded string (byte sequence). Invalid
    sequences will be replaced with [Uchar.rep].
    The unicode version used to decide what code points to escape is decided by
    [unicode_version] (default is [Uucp.unicode_version]).
*)
