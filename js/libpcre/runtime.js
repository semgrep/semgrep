//Provides: libpcre
var libpcre = globalThis.LibPcreModule;

//Provides: NULL const
var NULL = 0;

//Provides: PCRE_CONFIG_UTF8 const
var PCRE_CONFIG_UTF8 = 0;
//Provides: PCRE_CONFIG_NEWLINE const
var PCRE_CONFIG_NEWLINE = 1;
//Provides: PCRE_CONFIG_LINK_SIZE const
var PCRE_CONFIG_LINK_SIZE = 2;

//Provides: PCRE_CONFIG_MATCH_LIMIT const
var PCRE_CONFIG_MATCH_LIMIT = 4;
//Provides: PCRE_CONFIG_STACKRECURSE const
var PCRE_CONFIG_STACKRECURSE = 5;

//Provides: PCRE_CONFIG_MATCH_LIMIT_RECURSION const
var PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7;

//Provides: PCRE_STUDY_JIT_COMPILE const
var PCRE_STUDY_JIT_COMPILE = 0x0001;

//Provides: PCRE_EXTRA_MATCH_LIMIT const
var PCRE_EXTRA_MATCH_LIMIT = 0x0002;
//Provides: PCRE_EXTRA_MATCH_LIMIT_RECURSION const
var PCRE_EXTRA_MATCH_LIMIT_RECURSION = 0x0010;

//Provides: PCRE_INFO_SIZE const
var PCRE_INFO_SIZE = 1;
//Provides: PCRE_INFO_CAPTURECOUNT const
var PCRE_INFO_CAPTURECOUNT = 2;

//Provides: PCRE_ERROR_NOMATCH const
var PCRE_ERROR_NOMATCH = -1;

//Provides: STRUCT_PCRE_EXTRA const
var STRUCT_PCRE_EXTRA = {
  flags: 0,
  study_data_ptr: 4,
  match_limit: 8,
  callout_data_ptr: 12,
  tables_ptr: 16,
  match_limit_recursion: 20,
  mark_ptr: 24,
  executable_jit_ptr: 28,
};

//Provides: STRUCT_PCRE const
var STRUCT_PCRE = {
  magic_number: 0,
  size: 4,
  options: 8,
  flags: 12,
  limit_match: 16,
  limit_recursion: 20,
  first_char: 24,
  req_char: 26,
  max_lookbehind: 28,
  top_bracket: 30,
  top_backref: 32,
  name_table_offset: 34,
  name_entry_size: 36,
  name_count: 38,
  ref_count: 40,
  dummy1: 42,
  dummy2: 44,
  dummy3: 48,
  tables_ptr: 50, // TODO: is this really 4 bytes?
  nullpad_ptr: 54,
};

//Provides: pcre_ocaml_init const
function pcre_ocaml_init() {
  // noop
}
//Provides: pcre_version_stub const
//Requires: libpcre
function pcre_version_stub() {
  var ptr = libpcre._pcre_version();
  var value = libpcre.UTF8ToString(ptr);
  return value;
}

//Provides: pcre_config_get_int
//Requires: libpcre
function pcre_config_get_int(what) {
  var ptr = libpcre._malloc(4);
  libpcre._pcre_config(what, ptr);
  var value = libpcre.getValue(ptr, "i8");
  libpcre._free(ptr);
  return value;
}

//Provides: pcre_config_get_long
//Requires: libpcre
function pcre_config_get_long(what) {
  var ptr = libpcre._malloc(4);
  libpcre._pcre_config(what, ptr);
  var value = libpcre.getValue(ptr, "i16");
  libpcre._free(ptr);
  return value;
}

//Provides: pcre_config_utf8_stub const
//Requires: pcre_config_get_int, PCRE_CONFIG_UTF8
function pcre_config_utf8_stub() {
  return pcre_config_get_int(PCRE_CONFIG_UTF8);
}

//Provides: pcre_config_newline_stub
//Requires: pcre_config_get_int, PCRE_CONFIG_NEWLINE
function pcre_config_newline_stub() {
  return pcre_config_get_int(PCRE_CONFIG_NEWLINE);
}

//Provides: pcre_config_link_size_stub_bc
//Requires: pcre_config_get_int, PCRE_CONFIG_LINK_SIZE
function pcre_config_link_size_stub_bc() {
  return pcre_config_get_int(PCRE_CONFIG_LINK_SIZE);
}

//Provides: pcre_config_match_limit_stub_bc
//Requires: pcre_config_get_long, PCRE_CONFIG_MATCH_LIMIT
function pcre_config_match_limit_stub_bc() {
  return pcre_config_get_long(PCRE_CONFIG_MATCH_LIMIT);
}

//Provides: pcre_config_match_limit_recursion_stub_bc
//Requires: pcre_config_get_long, PCRE_CONFIG_MATCH_LIMIT_RECURSION
function pcre_config_match_limit_recursion_stub_bc() {
  return pcre_config_get_long(PCRE_CONFIG_MATCH_LIMIT_RECURSION);
}

//Provides: pcre_config_stackrecurse_stub
//Requires: pcre_config_get_int, PCRE_CONFIG_STACKRECURSE
function pcre_config_stackrecurse_stub() {
  return pcre_config_get_int(PCRE_CONFIG_STACKRECURSE);
}
//Provides: pcre_make_string
//Requires: libpcre, PCRE_CONFIG_UTF8
function pcre_make_string(v_opt, string) {
  var ptr;
  if (v_opt & PCRE_CONFIG_UTF8) {
    ptr = libpcre._malloc(string.length * 4 + 1);
    libpcre.stringToUTF8(string, ptr);
  } else {
    ptr = libpcre._malloc(string.length + 1);
    libpcre.stringToAscii(string, ptr);
  }
  return ptr;
}

//Provides: pcre_compile_stub_bc
//Requires: PCRE_INFO_SIZE, NULL, libpcre, pcre_make_string
function pcre_compile_stub_bc(v_opt, v_tables, v_pat) {
  //size_t regexp_size, ocaml_regexp_size = sizeof(struct pcre_ocaml_regexp);
  var regexp_info_ptr = libpcre._malloc(16);
  var error_ptr = libpcre._malloc(4);
  var error_ptr_ptr = libpcre._malloc(4);
  libpcre.setValue(error_ptr_ptr, error_ptr, "i32");
  var error_ofs_ptr =
    libpcre._malloc(4); /* offset in the pattern at which error occurred */

  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  //chartables tables = Is_none(v_tables) ? NULL : get_tables(Field(v_tables, 0));
  if (v_tables != 0) {
    throw new Error("need to do something with v_tables");
  }

  var pattern_ptr = pcre_make_string(v_opt, v_pat);

  /* Compiles the pattern */
  var regexp_ptr = libpcre._pcre_compile(
    pattern_ptr,
    v_opt,
    error_ptr_ptr,
    error_ofs_ptr,
    0
  );

  /* Raises appropriate exception with [BadPattern] if the pattern
     could not be compiled */
  if (regexp_ptr == NULL) {
    var errorString = libpcre.UTF8ToString(
      libpcre.getValue(error_ptr_ptr, "i32")
    );
    throw new Error(errorString + " at offset " + libpcre.HEAP8[error_ofs_ptr]);
  }

  /* It's unknown at this point whether the user will study the pattern
     later (probably), or if JIT compilation is going to be used, but we
     have to decide on a size.  Tests with some simple patterns indicate a
     roughly 50% increase in size when studying without JIT.  A factor of
     two times hence seems like a reasonable bound to use here. */
  var fullinfo_result = libpcre._pcre_fullinfo(
    regexp_ptr,
    0,
    PCRE_INFO_SIZE,
    regexp_info_ptr
  );

  return {
    regexp_ptr: regexp_ptr,
    extra_ptr: NULL,
    studied: 0,
  };
}

//Provides: pcre_study_stub
//Requires: PCRE_STUDY_JIT_COMPILE, NULL, libpcre
function pcre_study_stub(v_rex, v_jit_compile) {
  if (!v_rex.studied) {
    var flags = v_jit_compile ? PCRE_STUDY_JIT_COMPILE : 0;
    var error_ptr = libpcre._malloc(1024);
    var extra_ptr = libpcre._pcre_study(v_rex.regexp_ptr, flags, error_ptr);
    if (libpcre.HEAP8[error_ptr] != NULL) {
      throw new Error(
        "invalid argument: " +
          libpcre.UTF8ToString(libpcre.getValue(error_ptr, "i32"))
      );
    }
    v_rex.extra_ptr = extra_ptr;
    v_rex.studied = 1;
  }
  return v_rex;
}

//Provides: pcre_set_imp_match_limit_stub_bc
//Requires: PCRE_EXTRA_MATCH_LIMIT, NULL, libpcre, STRUCT_PCRE_EXTRA
function pcre_set_imp_match_limit_stub_bc(v_rex, v_lim) {
  if (v_rex.extra_ptr == NULL) {
    v_rex.extra_ptr = libpcre._malloc(32);
    libpcre.setValue(
      v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags,
      PCRE_EXTRA_MATCH_LIMIT,
      "i32"
    );
  } else {
    libpcre.setValue(
      v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags,
      libpcre.getValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, "i32") |
        PCRE_EXTRA_MATCH_LIMIT,
      "i32"
    );
  }
  libpcre.setValue(
    v_rex.extra_ptr + STRUCT_PCRE_EXTRA.match_limit,
    v_lim,
    "i32"
  );
  return v_rex;
}

//Provides: pcre_set_imp_match_limit_recursion_stub_bc
//Requires: PCRE_EXTRA_MATCH_LIMIT_RECURSION, NULL, libpcre, STRUCT_PCRE_EXTRA
function pcre_set_imp_match_limit_recursion_stub_bc(v_rex, v_lim) {
  if (v_rex.extra_ptr == NULL) {
    v_rex.extra_ptr = libpcre._malloc(32);
    libpcre.setValue(
      v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags,
      PCRE_EXTRA_MATCH_LIMIT_RECURSION,
      "i32"
    );
  } else {
    libpcre.setValue(
      v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags,
      libpcre.getValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, "i32") |
        PCRE_EXTRA_MATCH_LIMIT_RECURSION,
      "i32"
    );
  }
  libpcre.setValue(
    v_rex.extra_ptr + STRUCT_PCRE_EXTRA.match_limit_recursion,
    v_lim,
    "i32"
  );
  return v_rex;
}

//Provides: pcre_capturecount_stub_bc
//Requires: PCRE_INFO_CAPTURECOUNT, libpcre
function pcre_capturecount_stub_bc(v_rex) {
  var options_ptr = libpcre._malloc(4);
  var ret = libpcre._pcre_fullinfo(
    v_rex.regexp_ptr,
    v_rex.extra_ptr,
    PCRE_INFO_CAPTURECOUNT,
    options_ptr
  );
  if (ret != 0) {
    throw new Error("TODO");
  }
  return libpcre.getValue(options_ptr, "i32");
}

//Provides: handle_exec_error
//Requires: caml_raise_not_found, PCRE_ERROR_NOMATCH
function handle_exec_error(loc, ret) {
  switch (ret) {
    case PCRE_ERROR_NOMATCH:
      caml_raise_not_found();
      return;
    default:
      throw new Error("dont know what to do with error: " + ret);
  }
}

//Provides: pcre_exec_stub_bc
//Requires: handle_exec_error, libpcre, pcre_make_string
function pcre_exec_stub_bc(
  v_opt,
  v_rex,
  v_pos,
  v_subj_start,
  v_subj,
  v_ovec,
  v_maybe_cof,
  v_workspace
) {
  var ret;
  var is_dfa = !!v_workspace;
  var pos = v_pos;
  var len = v_subj.length;
  var subj_start = v_subj_start;

  var v_subj_ptr = pcre_make_string(v_opt, v_subj);

  var ovec_len = v_ovec.length;

  if (pos > len || pos < subj_start) {
    throw new Error("Pcre.pcre_exec_stub: illegal position");
  }

  if (subj_start > len || subj_start < 0) {
    throw new Error("Pcre.pcre_exec_stub: illegal subject start");
  }

  pos -= subj_start;
  len -= subj_start;

  var ocaml_subj_ptr = v_subj_ptr + subj_start;
  var opt = v_opt;

  if (!v_maybe_cof) {
    var ovec_ptr = libpcre._malloc(ovec_len * 4);
    if (is_dfa) {
      ret = libpcre._pcre_dfa_exec();
    } else {
      ret = libpcre._pcre_exec(
        v_rex.regexp_ptr,
        v_rex.extra_ptr,
        ocaml_subj_ptr,
        len,
        pos,
        opt,
        ovec_ptr,
        ovec_len
      );
    }
    if (ret < 0) {
      handle_exec_error("pcre_exec_stub", ret);
    } else {
      for (var i = 0; i < ovec_len; i++) {
        v_ovec[i] = libpcre.getValue(ovec_ptr + i * 4, "i32");
      }
    }
  } else {
    throw new Error("callout functions unimplemented");
  }
}

//Always
//Requires: pcre_version_stub,pcre_config_stackrecurse_stub
(() => {
  if (globalThis.exposePcreStubsForTesting) {
    module.exports = {
      pcre_version_stub,
      pcre_config_utf8_stub,
      pcre_compile_stub_bc,
    };
  }
})();
