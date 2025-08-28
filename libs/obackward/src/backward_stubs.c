/*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*/
#include <caml/mlvalues.h>

int register_sh();

CAMLprim value ml_register(value v_unit) {
    int exit_code;
    // Register the signal handler for segmentation faults
    //
    // Sometimes it can fail, I'm not sure why, but if it does let's print an
    // exit code
    if ((exit_code = register_sh()) != 1) {
        fprintf(stderr,
                "Failed to register segfault signal handler! exit_code: %d\n",
                exit_code);
    }
    return Val_bool(exit_code == 1);
}
