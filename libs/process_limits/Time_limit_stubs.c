/*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*/

/*
   Identify the stack (i.e. the fiber) that the current domain is executing
   OCaml code on. See Time_limit.ml for why we need this.

   This relies on OCaml 5 runtime internals: every stack gets a unique id
   when it is allocated, which is preserved when the stack is reallocated to
   grow, so it identifies a fiber (or a thread's or domain's main stack) for
   its whole lifetime. The stack_info pointer itself cannot be used for
   that, since it changes when the stack grows.
*/

#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/domain_state.h>
#include <caml/fiber.h>

/* Doesn't allocate, raise, or release the runtime lock, so it can be
   declared [@@noalloc] on the OCaml side. */
value semgrep_current_stack_id(value unit) {
  (void)unit;
  return Val_long(Caml_state->current_stack->id);
}
