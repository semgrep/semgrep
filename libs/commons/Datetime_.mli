(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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
val now : unit -> float
(** The current time in seconds since epoch *)

val time_n_days_ago : days:int -> time:float -> float
(** [time_n_days_ago ~days ~time] evaluates to [time] shifted by the amount of
 * seconds in [days]
 *)

val of_unix_int_time :
  int64 -> [ `Plus | `Minus ] -> int -> int -> Timedesc.Timestamp.t
