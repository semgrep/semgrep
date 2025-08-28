/*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*/
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "MurmurHash3.h"

#define Val64 caml_copy_int64

CAMLprim value caml_murmur3_128(value data)
{
    CAMLparam1(data);
    CAMLlocal1(res);
    uint64_t hash[2];
    MurmurHash3_x64_128(String_val(data), caml_string_length(data), 0, &hash);
    res = caml_alloc(2, 0);
    Store_field(res, 0, Val64(hash[0]));
    Store_field(res, 1, Val64(hash[1]));
    CAMLreturn(res);
}
