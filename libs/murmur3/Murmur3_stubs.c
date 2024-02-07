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
