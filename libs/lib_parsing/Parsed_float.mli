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

(* Parsed float literals
 *
 * This module provides utilities for parsing float literals from source text.
 * Many languages (Java, C, C++, Rust, etc.) allow type suffixes on float literals
 * (e.g., 1.0f, 2.5d, 3.14F, 4.2D) which need to be stripped before parsing.
 *)

type t = float option * Tok.t

(* Parse a float literal, stripping common type suffixes (f, F, d, D, l, L).
 * This handles literals from languages like:
 * - Java: 1.0f, 2.0F, 3.0d, 4.0D
 * - C/C++: 1.0f, 2.0F, 3.0l, 4.0L
 * - Rust: 1.0f32, 2.0f64
 *)
val parse : string * Tok.t -> t
