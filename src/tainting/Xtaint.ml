(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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

module Taints = Taint.Taint_set

type t = [ `Tainted of Taints.t | `None | `Clean ]
type t_or_sanitized = [ t | `Sanitized ]

let equal xt1 xt2 =
  match (xt1, xt2) with
  | `Tainted taints1, `Tainted taints2 -> Taints.equal taints1 taints2
  | `None, `None
  | `Clean, `Clean ->
      true
  | `Tainted _, _
  | `None, _
  | `Clean, _ ->
      false

let compare xt1 xt2 =
  match (xt1, xt2) with
  | `Tainted taints1, `Tainted taints2 -> Taints.compare taints1 taints2
  | `None, `None
  | `Clean, `Clean ->
      0
  | `Tainted _, _ -> -1
  | `None, _ -> -1
  | `Clean, _ -> 1

let show = function
  | `Tainted taints -> Taint.show_taints taints
  | `None -> "0"
  | `Clean -> "C"

let union xt1 xt2 =
  match (xt1, xt2) with
  | `Tainted taints1, `Tainted taints2 ->
      `Tainted (Taints.union taints1 taints2)
  | `Tainted taints, (`None | `Clean)
  | (`None | `Clean), `Tainted taints ->
      `Tainted taints
  | `None, `None -> `None
  | `Clean, `Clean -> `Clean
  | `None, `Clean
  | `Clean, `None ->
      (* THINK *)
      `Clean

let is_tainted xt =
  match xt with
  | `Tainted _ -> true
  | __else__ -> false

let of_taints taints = if Taints.is_empty taints then `None else `Tainted taints

let to_taints = function
  | `None
  | `Clean
  | `Sanitized ->
      Taints.empty
  | `Tainted taints -> taints
