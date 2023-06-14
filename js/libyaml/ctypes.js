// Copied from https://gitlab.com/nomadic-labs/ctypes_stubs_js/-/blob/main/src/runtime.js
// We can't simply use ctypes_stubs_js because we're overriding a few methods, which generates build warnings
// Longer-term solution would be to mark these methods as WeakRef in the upstream package.

//Provides: ldouble_init
function ldouble_init(unit) {
  return 0;
}

//Provides: ctypes_ldouble_of_float
function ctypes_ldouble_of_float(x) {
  return x;
}

//Provides: ctypes_ldouble_to_float
function ctypes_ldouble_to_float(x) {
  return x;
}

//Provides: ctypes_ldouble_to_int
function ctypes_ldouble_to_int(x) {
  return x | 0;
}

//Provides: ctypes_ldouble_of_int
function ctypes_ldouble_of_int(x) {
  return x;
}

//Provides: ctypes_ldouble_format
//Requires: caml_format_float, caml_string_of_jsbytes
function ctypes_ldouble_format(width, prec, x) {
  var fmt = caml_string_of_jsbytes("%" + width + "." + prec + "f");
  return caml_format_float(fmt, x);
}

//Provides: ctypes_ldouble_of_string
//Requires: caml_float_of_string
function ctypes_ldouble_of_string(x) {
  return caml_float_of_string(x);
}

//Provides: ctypes_ldouble_add
function ctypes_ldouble_add(a, b) {
  return a + b;
}

//Provides: ctypes_ldouble_sub
function ctypes_ldouble_sub(a, b) {
  return a - b;
}

//Provides: ctypes_ldouble_mul
function ctypes_ldouble_mul(a, b) {
  return a * b;
}

//Provides: ctypes_ldouble_div
function ctypes_ldouble_div(a, b) {
  return a / b;
}

//Provides: ctypes_ldouble_neg
function ctypes_ldouble_neg(a) {
  return -a;
}

//Provides: ctypes_ldouble_powl
function ctypes_ldouble_powl(a, b) {
  return Math.pow(a, b);
}

//Provides: ctypes_ldouble_sqrtl
function ctypes_ldouble_sqrtl(a) {
  return Math.sqrt(a);
}

//Provides: ctypes_ldouble_expl
function ctypes_ldouble_expl(a) {
  return Math.exp(a);
}

//Provides: ctypes_ldouble_logl
function ctypes_ldouble_logl(a) {
  return Math.log(a);
}

//Provides: ctypes_ldouble_log10l
function ctypes_ldouble_log10l(a) {
  return Math.log10(a);
}

//Provides: ctypes_ldouble_expm1l
function ctypes_ldouble_expm1l(a) {
  return Math.expm1(a);
}

//Provides: ctypes_ldouble_log1pl
function ctypes_ldouble_log1pl(a) {
  return Math.log1p(a);
}
//Provides: ctypes_ldouble_cosl
function ctypes_ldouble_cosl(x) {
  return Math.cos(x);
}
//Provides: ctypes_ldouble_sinl
function ctypes_ldouble_sinl(x) {
  return Math.sin(x);
}
//Provides: ctypes_ldouble_tanl
function ctypes_ldouble_tanl(x) {
  return Math.tan(x);
}
//Provides: ctypes_ldouble_acosl
function ctypes_ldouble_acosl(x) {
  return Math.acos(x);
}
//Provides: ctypes_ldouble_asinl
function ctypes_ldouble_asinl(x) {
  return Math.asin(x);
}
//Provides: ctypes_ldouble_atanl
function ctypes_ldouble_atanl(x) {
  return Math.atan(x);
}
//Provides: ctypes_ldouble_coshl
function ctypes_ldouble_coshl(x) {
  return Math.cosh(x);
}
//Provides: ctypes_ldouble_sinhl
function ctypes_ldouble_sinhl(x) {
  return Math.sinh(x);
}
//Provides: ctypes_ldouble_tanhl
function ctypes_ldouble_tanhl(x) {
  return Math.tanh(x);
}
//Provides: ctypes_ldouble_acoshl
function ctypes_ldouble_acoshl(x) {
  return Math.acosh(x);
}
//Provides: ctypes_ldouble_asinhl
function ctypes_ldouble_asinhl(x) {
  return Math.asinh(x);
}
//Provides: ctypes_ldouble_atanhl
function ctypes_ldouble_atanhl(x) {
  return Math.atanh(x);
}
//Provides: ctypes_ldouble_ceill
function ctypes_ldouble_ceill(x) {
  return Math.ceil(x);
}
//Provides: ctypes_ldouble_floorl
function ctypes_ldouble_floorl(x) {
  return Math.floor(x);
}
//Provides: ctypes_ldouble_fabsl
function ctypes_ldouble_fabsl(x) {
  return Math.abs(x);
}

//Provides: ctypes_ldouble_atan2l
function ctypes_ldouble_atan2l(x, y) {
  return Math.atan2(x, y);
}
//Provides: ctypes_ldouble_hypotl
function ctypes_ldouble_hypotl(x, y) {
  return Math.hypot(x, y);
}
//Provides: ctypes_ldouble_remainderl
//Requires: caml_failwith
function ctypes_ldouble_remainderl(x) {
  caml_failwith("ctypes: remainderl does not exist on current platform");
}

//Provides: ctypes_ldouble_copysignl
function ctypes_ldouble_copysignl(a, b) {
  if (b < 0) return -Math.abs(a);
  else return Math.abs(a);
}

//Provides: ctypes_ldouble_frexp
//Requires: caml_failwith
function ctypes_ldouble_frexp(x) {
  caml_failwith("ctypes: frexp does not exist on current platform");
}

//Provides: ctypes_ldouble_ldexp
//Requires: caml_failwith
function ctypes_ldouble_ldexp(x, i) {
  caml_failwith("ctypes: ldexp does not exist on current platform");
}

//Provides: ctypes_ldouble_modf
//Requires: caml_failwith
function ctypes_ldouble_modf(x) {
  caml_failwith("ctypes: modf does not exist on current platform");
}

//Provides: ctypes_ldouble_classify
//Requires: caml_classify_float
function ctypes_ldouble_classify(x) {
  return caml_classify_float(x);
}

//Provides: ctypes_ldouble_min
function ctypes_ldouble_min(unit) {
  return -Number.MAX_VALUE;
}

//Provides: ctypes_ldouble_max
function ctypes_ldouble_max(unit) {
  return Number.MAX_VALUE;
}

//Provides: ctypes_ldouble_epsilon
function ctypes_ldouble_epsilon(unit) {
  return Number.EPSILON;
}

//Provides: ctypes_ldouble_nan
function ctypes_ldouble_nan(unit) {
  return NaN;
}

//Provides: ctypes_ldouble_inf
function ctypes_ldouble_inf(unit) {
  return Infinity;
}
//Provides: ctypes_ldouble_ninf
function ctypes_ldouble_ninf(unit) {
  return -Infinity;
}

//Provides: ctypes_ldouble_size
function ctypes_ldouble_size(unit) {
  return [0, 8, 8];
}

//Provides: ctypes_ldouble_mant_dig
function ctypes_ldouble_mant_dig(unit) {
  return 53;
}

//Provides: CtypesComplex
//Requires: caml_compare_val
function CtypesComplex(re, im) {
  this.re = re;
  this.im = im;
}

CtypesComplex.prototype.compare = function (b, total) {
  var a = this;
  var r = caml_compare_val(a.re, b.re, total);
  if (r == 0) return caml_compare_val(a.im, b.im, total);
  else return r;
};

//Provides: ctypes_ldouble_complex_make
//Requires: CtypesComplex
function ctypes_ldouble_complex_make(re, im) {
  return new CtypesComplex(re, im);
}

//Provides: ctypes_ldouble_complex_real
//Requires: CtypesComplex
function ctypes_ldouble_complex_real(x) {
  return x.re;
}

//Provides: ctypes_ldouble_complex_imag
//Requires: CtypesComplex
function ctypes_ldouble_complex_imag(x) {
  return x.im;
}

//Provides: ctypes_ldouble_complex_neg
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_neg(x) {
  return ctypes_ldouble_complex_make(-x.re, -x.im);
}

//Provides: ctypes_ldouble_complex_conjl
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_conjl(x) {
  return ctypes_ldouble_complex_make(x.re, -x.im);
}

//Provides: ctypes_ldouble_complex_add
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_add(x, y) {
  return ctypes_ldouble_complex_make(x.re + y.re, x.im + y.im);
}

//Provides: ctypes_ldouble_complex_sub
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_sub(x, y) {
  return ctypes_ldouble_complex_make(x.re - y.re, x.im - y.im);
}

//Provides: ctypes_ldouble_complex_mul
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_mul(x, y) {
  return ctypes_ldouble_complex_make(
    x.re * y.re - x.im * y.im,
    x.re * y.im + x.im * y.re
  );
}

//Provides: ctypes_ldouble_complex_div
//Requires: ctypes_ldouble_complex_make
function ctypes_ldouble_complex_div(x, y) {
  var a = x.re,
    b = x.im,
    c = y.re,
    d = y.im;
  if (y.re == 0 && y.im == 0) {
    return ctypes_ldouble_complex_make(x.re / 0, x.im / 0);
  }
  var re = (a * c + b * d) / (c * c + d * d);
  var im = (b * c - a * d) / (c * c + d * d);
  return ctypes_ldouble_complex_make(re, im);
}

//Provides: ctypes_ldouble_complex_csqrtl
//Requires: caml_failwith
function ctypes_ldouble_complex_csqrtl(x) {
  caml_failwith("ctypes: ctypes_ldouble_complex_csqrtl not implemented");
}

//Provides: ctypes_ldouble_complex_cexpl
//Requires: caml_failwith
function ctypes_ldouble_complex_cexpl(x) {
  caml_failwith("ctypes: ctypes_ldouble_complex_cexpl not implemented");
}

//Provides: ctypes_ldouble_complex_clogl
//Requires: caml_failwith
function ctypes_ldouble_complex_clogl(x) {
  caml_failwith("ctypes: ctypes_ldouble_complex_clogl not implemented");
}

//Provides: ctypes_ldouble_complex_cpowl
//Requires: caml_failwith
function ctypes_ldouble_complex_cpowl(x) {
  caml_failwith("ctypes: ctypes_ldouble_complex_cpowl not implemented");
}

//Provides: ctypes_ldouble_complex_cargl
//Requires: caml_failwith
function ctypes_ldouble_complex_cargl(x) {
  caml_failwith("ctypes: ctypes_ldouble_complex_cargl not implemented");
}

//Provides: ctypes_string_of_prim
//Requires: caml_failwith
function ctypes_string_of_prim(prim, v) {
  caml_failwith("ctypes: ctypes_string_of_prim not implemented");
}

//Provides: ctypes_string_of_pointer
//Requires: caml_failwith
function ctypes_string_of_pointer(x) {
  caml_failwith("ctypes: ctypes_string_of_pointer not implemented");
}

//Provides: ctypes_write_pointer
//Requires: caml_failwith
function ctypes_write_pointer(ptr1, ptr2) {
  caml_failwith("ctypes: ctypes_write_pointer not implemented");
}

//Provides: ctypes_memcpy
//Requires: caml_failwith
function ctypes_memcpy(dst, src, size) {
  caml_failwith("ctypes: ctypes_memcpy not implemented");
}

//Provides: ctypes_string_of_array
//Requires: caml_failwith
function ctypes_string_of_array(ptr, len) {
  caml_failwith("ctypes: ctypes_string_of_array not implemented");
}

//Provides: ctypes_use
//Requires: caml_failwith
function ctypes_use(x) {
  caml_failwith("ctypes: ctypes_use not implemented");
}

//Provides: ctypes_cstring_of_string
//Requires: caml_failwith
function ctypes_cstring_of_string(x) {
  caml_failwith("ctypes: ctypes_cstring_of_string not implemented");
}

//Provides: integers_uintptr_t_size
function integers_uintptr_t_size(unit) {
  return 4;
}

//Provides: integers_intptr_t_size
function integers_intptr_t_size(unit) {
  return 4;
}

//Provides: integers_ptrdiff_t_size
function integers_ptrdiff_t_size(unit) {
  return 4;
}

//Provides: ctypes_caml_root_create
//Requires: caml_failwith
function ctypes_caml_root_create(x) {
  caml_failwith("ctypes: ctypes_caml_root_create not implemented");
}

//Provides: ctypes_caml_root_set
//Requires: caml_failwith
function ctypes_caml_root_set(ptr, x) {
  caml_failwith("ctypes: ctypes_caml_root_set not implemented");
}

//Provides: ctypes_caml_root_get
//Requires: caml_failwith
function ctypes_caml_root_get(ptr) {
  caml_failwith("ctypes: ctypes_caml_root_get not implemented");
}

//Provides: ctypes_caml_root_release
//Requires: caml_failwith
function ctypes_caml_root_release(ptr) {
  caml_failwith("ctypes: ctypes_caml_root_release not implemented");
}

//Provides: ctypes_bigarray_address
//Requires: caml_failwith
function ctypes_bigarray_address(x) {
  caml_failwith("ctypes: ctypes_bigarray_address not implemented");
}

//Provides: ctypes_bigarray_view
//Requires: caml_failwith
function ctypes_bigarray_view(kind, dims, ptr, layout) {
  caml_failwith("ctypes: ctypes_bigarray_view not implemented");
}

//Provides: ctypes_typeof_clock_t
function ctypes_typeof_clock_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_dev_t
function ctypes_typeof_dev_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_ino_t
function ctypes_typeof_ino_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_mode_t
function ctypes_typeof_mode_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_nlink_t
function ctypes_typeof_nlink_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_off_t
function ctypes_typeof_off_t(unit) {
  return 2;
}
//Provides: ctypes_typeof_pid_t
function ctypes_typeof_pid_t(unit) {
  return 2;
}
//Provides: ctypes_typeof_ssize_t
function ctypes_typeof_ssize_t(unit) {
  return 2;
}
//Provides: ctypes_typeof_time_t
function ctypes_typeof_time_t(unit) {
  return 6;
}
//Provides: ctypes_typeof_useconds_t
function ctypes_typeof_useconds_t(unit) {
  return 6;
}
//Provides: ctypes_sizeof_sigset_t
function ctypes_sizeof_sigset_t(unit) {
  return 4;
}
//Provides: ctypes_alignmentof_sigset_t
function ctypes_alignmentof_sigset_t(unit) {
  return 4;
}

//The following should be provied by integers_stubs_js

//Provides: integers_uint32_to_hexstring
//Requires: caml_failwith
function integers_uint32_to_hexstring(x) {
  caml_failwith("integers_uint32_to_hexstring not implemented");
}

//Provides: integers_uint64_to_hexstring
//Requires: caml_failwith
function integers_uint64_to_hexstring(x) {
  caml_failwith("integers_uint64_to_hexstring not implemented");
}
