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
type eval_strategy = EvalSubst | EvalEnvir (* | EvalStrict *)
[@@deriving show { with_path = false }]

(* nosemgrep: no-ref-declarations-at-top-scope *)
let eval_strategy = ref EvalEnvir

(* set to false to debug *)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let use_std = ref true

(* set also to false to help debug *)
(* nosemgrep: no-ref-declarations-at-top-scope *)
let implement_self = ref true

(* nosemgrep: no-ref-declarations-at-top-scope *)
let implement_dollar = ref true
