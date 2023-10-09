//Provides: libpcre
var libpcre = globalThis.LibPcreModule;

//Provides: NULL const
const NULL = 0;

//Provides: PCRE_CONFIG_UTF8 const
const PCRE_CONFIG_UTF8 = 0;
//Provides: PCRE_CONFIG_NEWLINE const
const PCRE_CONFIG_NEWLINE = 1;
//Provides: PCRE_CONFIG_LINK_SIZE const
const PCRE_CONFIG_LINK_SIZE = 2;

//Provides: PCRE_CONFIG_MATCH_LIMIT const
const PCRE_CONFIG_MATCH_LIMIT = 4;
//Provides: PCRE_CONFIG_STACKRECURSE const
const PCRE_CONFIG_STACKRECURSE = 5;

//Provides: PCRE_CONFIG_MATCH_LIMIT_RECURSION const
const PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7;

//Provides: PCRE_STUDY_JIT_COMPILE const
const PCRE_STUDY_JIT_COMPILE = 0x0001;

//Provides: PCRE_EXTRA_MATCH_LIMIT const
const PCRE_EXTRA_MATCH_LIMIT = 0x0002;
//Provides: PCRE_EXTRA_MATCH_LIMIT_RECURSION const
const PCRE_EXTRA_MATCH_LIMIT_RECURSION = 0x0010;

//Provides: PCRE_INFO_SIZE const
const PCRE_INFO_SIZE = 1;
//Provides: PCRE_INFO_CAPTURECOUNT const
const PCRE_INFO_CAPTURECOUNT = 2;

//Provides: PCRE_INFO_NAMEENTRYSIZE const
const PCRE_INFO_NAMEENTRYSIZE = 7;

//Provides: PCRE_INFO_NAMECOUNT const
const PCRE_INFO_NAMECOUNT = 8;

// Provides: PCRE_INFO_NAMETABLE const
const PCRE_INFO_NAMETABLE = 9;

//Provides: PCRE_ERROR_NOMATCH const
const PCRE_ERROR_NOMATCH = -1;

//Provides: PCRE_ERROR_NOSUBSTRING const
const PCRE_ERROR_NOSUBSTRING = -7;

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

//Provides: auto_malloc
//Requires: libpcre
function auto_malloc(sizes, func) {
  const ptrs = sizes.map((size) => libpcre._malloc(size));
  try {
    return func(ptrs);
  } finally {
    ptrs.forEach((ptr) => {
      libpcre._free(ptr);
    });
  }
}

//Provides: pcre_ocaml_init const
function pcre_ocaml_init() {
  // noop
}

//Provides: pcre_version_stub const
//Requires: libpcre
function pcre_version_stub() {
  return libpcre.UTF8ToString(libpcre._pcre_version());
}

//Provides: pcre_config_get_int
//Requires: libpcre, auto_malloc
function pcre_config_get_int(what) {
  return auto_malloc([1], ([ptr]) => {
    libpcre._pcre_config(what, ptr);
    return libpcre.getValue(ptr, "i8");
  });
}

//Provides: pcre_config_get_long
//Requires: libpcre, auto_malloc
function pcre_config_get_long(what) {
  return auto_malloc([2], ([ptr]) => {
    libpcre._pcre_config(what, ptr);
    return libpcre.getValue(ptr, "i16");
  });
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

//Provides: pcre_alloc_string
//Requires: libpcre
function pcre_alloc_string(js_string) {
  var ptr;
  const array = libpcre.intArrayFromString(js_string);
  const length = array.length;
  ptr = libpcre._malloc(length + 1);
  libpcre.writeArrayToMemory(array, ptr);
  return ptr;
}

//Provides: pcre_compile_stub_bc
//Requires: PCRE_INFO_SIZE, NULL, libpcre, pcre_alloc_string, auto_malloc
function pcre_compile_stub_bc(v_opt, v_tables, v_pat) {
  const regexp_ptr = auto_malloc([4, 4], ([error_ptr_ptr, error_ofs_ptr]) => {
    if (v_tables != 0) {
      throw new Error("v_tables not supported");
    }

    const ptr = libpcre._pcre_compile(
      pcre_alloc_string(v_pat),
      v_opt,
      error_ptr_ptr,
      error_ofs_ptr,
      0
    );

    /* Raises appropriate exception with [BadPattern] if the pattern
      could not be compiled */
    if (ptr == NULL) {
      const errorString = libpcre.UTF8ToString(
        libpcre.getValue(error_ptr_ptr, "i32")
      );
      throw new Error(
        `${errorString} at offset ${libpcre.getValue(error_ofs_ptr, "i32")}`
      );
    }

    return ptr;
  });

  return {
    regexp_ptr,
    extra_ptr: NULL,
    studied: 0,
  };
}

//Provides: pcre_study_stub
//Requires: PCRE_STUDY_JIT_COMPILE, NULL, libpcre, auto_malloc
function pcre_study_stub(v_rex, v_jit_compile) {
  if (!v_rex.studied) {
    const flags = v_jit_compile ? PCRE_STUDY_JIT_COMPILE : 0;
    auto_malloc([4], ([error_ptr_ptr]) => {
      const extra_ptr = libpcre._pcre_study(
        v_rex.regexp_ptr,
        flags,
        error_ptr_ptr
      );
      if (libpcre.getValue(error_ptr_ptr, "i8") != NULL) {
        throw new Error(
          "invalid argument: " +
            libpcre.UTF8ToString(libpcre.getValue(error_ptr_ptr, "i32"))
        );
      }
      v_rex.extra_ptr = extra_ptr;
      v_rex.studied = 1;
    });
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
//Requires: PCRE_INFO_CAPTURECOUNT, libpcre, auto_malloc
function pcre_capturecount_stub_bc(v_rex) {
  return auto_malloc([4], (options_ptr) => {
    const ret = libpcre._pcre_fullinfo(
      v_rex.regexp_ptr,
      v_rex.extra_ptr,
      PCRE_INFO_CAPTURECOUNT,
      options_ptr
    );
    if (ret != 0) {
      throw new Error("TODO");
    }
    return libpcre.getValue(options_ptr, "i32");
  });
}

//Provides: handle_exec_error
//Requires: caml_raise_not_found, PCRE_ERROR_NOMATCH
function handle_exec_error(loc, ret) {
  switch (ret) {
    case PCRE_ERROR_NOMATCH:
      caml_raise_not_found();
      return;
    default:
      throw new Error(`${loc}: unhandled PCRE error code: ${ret}`); // TODO: make pcre-ocaml's raise_internal_error() work
  }
}

//Provides: pcre_exec_stub_bc
//Requires: handle_exec_error, libpcre, pcre_alloc_string, caml_invalid_argument, auto_malloc, caml_jsstring_of_string
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
  var v_subj = caml_jsstring_of_string(v_subj);
  var len = libpcre.lengthBytesUTF8(v_subj);
  var subj_start = v_subj_start;

  var v_subj_ptr = pcre_alloc_string(v_subj);

  var ovec_len = v_ovec.length - 1;

  if (pos > len || pos < subj_start) {
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal position");
  }

  if (subj_start > len || subj_start < 0) {
    caml_invalid_argument("Pcre.pcre_exec_stub: illegal subject start");
  }

  pos -= subj_start;
  len -= subj_start;

  const ocaml_subj_ptr = v_subj_ptr + subj_start;
  const opt = v_opt;

  if (!v_maybe_cof) {
    auto_malloc([ovec_len * 4], ([ovec_ptr]) => {
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
          const val = libpcre.getValue(ovec_ptr + i * 4, "i32");
          if (val > -1) {
            v_ovec[i + 1] = val + subj_start;
          } else {
            v_ovec[i + 1] = -1;
          }
        }
      }
    });
  } else {
    throw new Error("callout functions unimplemented");
  }
}

//Provides: pcre_names_stub
//Requires: libpcre, PCRE_INFO_NAMECOUNT, PCRE_INFO_NAMEENTRYSIZE, PCRE_INFO_NAMETABLE, caml_js_to_array, auto_malloc
function pcre_names_stub(v_rex) {
  const { regexp_ptr, extra_ptr } = v_rex;

  return auto_malloc(
    [4, 4, 4],
    ([name_count_ptr, entry_size_ptr, tbl_ptr_ptr]) => {
      var ret = libpcre._pcre_fullinfo(
        regexp_ptr,
        extra_ptr,
        PCRE_INFO_NAMECOUNT,
        name_count_ptr
      );
      if (ret != 0) throw new Error("pcre_names_stub: namecount");

      ret = libpcre._pcre_fullinfo(
        regexp_ptr,
        extra_ptr,
        PCRE_INFO_NAMEENTRYSIZE,
        entry_size_ptr
      );
      if (ret != 0) throw new Error("pcre_names_stub: nameentrysize");

      ret = libpcre._pcre_fullinfo(
        regexp_ptr,
        extra_ptr,
        PCRE_INFO_NAMETABLE,
        tbl_ptr_ptr
      );
      if (ret != 0) throw new Error("pcre_names_stub: nametable");

      var result = [];

      const name_count = libpcre.getValue(name_count_ptr, "i32");
      const entry_size = libpcre.getValue(entry_size_ptr, "i32");
      var tbl_ptr = libpcre.getValue(tbl_ptr_ptr, "i32");

      for (var i = 0; i < name_count; i++) {
        result[i] = libpcre.UTF8ToString(tbl_ptr + 2);
        tbl_ptr += entry_size;
      }
      return caml_js_to_array(result);
    }
  );
}

//Provides: pcre_get_stringnumber_stub_bc
//Requires: caml_invalid_argument, PCRE_INFO_SIZE, NULL, libpcre, pcre_alloc_string, PCRE_CONFIG_UTF8, PCRE_ERROR_NOSUBSTRING, auto_malloc
function pcre_get_stringnumber_stub_bc(v_rex, v_name) {
  const { regexp_ptr } = v_rex;
  const string_ptr = pcre_alloc_string(v_name);
  try {
    const ret = libpcre._pcre_get_stringnumber(regexp_ptr, string_ptr);

    if (ret == PCRE_ERROR_NOSUBSTRING) {
      caml_invalid_argument("Named string not found");
    }

    return ret;
  } finally {
    libpcre._free(string_ptr);
  }
}

//Always
//Requires: pcre_version_stub,pcre_config_utf8_stub,pcre_compile_stub_bc,pcre_exec_stub_bc,pcre_get_stringnumber_stub_bc
(() => {
  if (globalThis.exposePcreStubsForTesting) {
    module.exports = {
      pcre_version_stub,
      pcre_config_utf8_stub,
      pcre_compile_stub_bc,
      pcre_exec_stub_bc,
      pcre_get_stringnumber_stub_bc,
    };
  }
})();
