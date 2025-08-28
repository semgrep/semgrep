(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* A Semgrep.dev token representing your identity.
 * This is stored in ~/.semgrep/settings.yml and initially fetched
 * from https://semgrep.dev during 'semgrep login'
 *)
type token

(* to be used in headers *)
val string_of_token : token -> string

(* ("Authorisation:", "Bearer <token>") *)
val auth_header_of_token : token -> string * string

(* TODO: should require a semgrep_dev capa or semgrep_settings capa *)
val unsafe_token_of_string : string -> token

val well_formed : token -> bool
(** [well_formed token] checks that the authentication token is structured as
    expected. Currently this just checks if the string is non-empty *)

val equal : token -> token -> bool

(* TODO? rename semgrep_token? *)
type cap_token = < token : token >

val cap_token_and_network :
  token -> < Cap.network ; .. > -> < cap_token ; Cap.network >

val cap_token_and_network_and_tmp :
  token ->
  < Cap.network ; Cap.tmp ; .. > ->
  < cap_token ; Cap.network ; Cap.tmp >

val cap_token_and_network_and_tmp_and_exec :
  token ->
  < Cap.network ; Cap.tmp ; Cap.exec ; .. > ->
  < cap_token ; Cap.network ; Cap.tmp ; Cap.exec >
