(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Same as 'Either_.partition' but operates on the standard type
   'result' (Ok or Error).
*)
val partition :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list

val collect : ('a, 'e) result list -> ('a list, 'e) result
(** Distributes out the successful results if zero errors inhabit the supplied
    list.  If multiple errors inhabit the list, all but one is discarded. *)

val list_map : ('a -> ('b, 'err) result) -> 'a list -> ('b list, 'err) result
(** Map the list from left to right until an error occurs.
    The result is either the new list or the first error encountered.
    Results past the first error are not computed, therefore this is
    potentially faster than [List.map] followed by a [collect] and it
    may produce clearer logs.
*)

module Operators : sig
  val ( >>= ) :
    ('a, 'error) result -> ('a -> ('b, 'error) result) -> ('b, 'error) result
end

val transpose_result_option :
  ('a, 'err) result option -> ('a option, 'err) result
(** [transpose_result_option x] transposes an optional result into a
    result of an option. *)
