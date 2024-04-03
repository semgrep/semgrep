//Provides: libpcre2
var libpcre2 = globalThis.LibPcre2Module;

// See <https://github.com/ocaml/ocaml/blob/fcad28647767cdffec18de0a4e8726bf92b5f308/runtime/hash.c#L311-L323>
//Provides: caml_hash_variant const
function caml_hash_variant(tag) {
    // Convert from JS UTF-16 to UTF-8.
    const buf = (new TextEncoder()).encode(tag);
    let accum = 0;
    for (let i = 0; i < buf.length; ++i) {
        accum = 223 * accum + buf[i];
    }
    return accum;
}

//Provides: NULL const
const NULL = 0;

//Provides: PCRE2_CONFIG_BSR const
const PCRE2_CONFIG_BSR = 0;
//Provides: PCRE2_CONFIG_JIT const
const PCRE2_CONFIG_JIT = 1;
//Provides: PCRE2_CONFIG_JITTARGET const
const PCRE2_CONFIG_JITTARGET = 2;
//Provides: PCRE2_CONFIG_LINKSIZE const
const PCRE2_CONFIG_LINKSIZE = 3;
//Provides: PCRE2_CONFIG_MATCHLIMIT const
const PCRE2_CONFIG_MATCHLIMIT = 4;
//Provides: PCRE2_CONFIG_NEWLINE const
const PCRE2_CONFIG_NEWLINE = 5;
//Provides: PCRE2_CONFIG_PARENSLIMIT const
const PCRE2_CONFIG_PARENSLIMIT = 6;
//Provides: PCRE2_CONFIG_DEPTHLIMIT const
const PCRE2_CONFIG_DEPTHLIMIT = 7;
//Provides: PCRE2_CONFIG_RECURSIONLIMIT const
const PCRE2_CONFIG_RECURSIONLIMIT = 7;  /* Obsolete synonym */
//Provides: PCRE2_CONFIG_STACKRECURSE const
const PCRE2_CONFIG_STACKRECURSE = 8;  /* Obsolete */
//Provides: PCRE2_CONFIG_UNICODE const
const PCRE2_CONFIG_UNICODE = 9;
//Provides: PCRE2_CONFIG_UNICODE_VERSION const
const PCRE2_CONFIG_UNICODE_VERSION = 10;
//Provides: PCRE2_CONFIG_VERSION const
const PCRE2_CONFIG_VERSION = 11;
//Provides: PCRE2_CONFIG_HEAPLIMIT const
const PCRE2_CONFIG_HEAPLIMIT = 12;
//Provides: PCRE2_CONFIG_NEVER_BACKSLASH_C const
const PCRE2_CONFIG_NEVER_BACKSLASH_C = 13;
//Provides: PCRE2_CONFIG_COMPILED_WIDTHS const
const PCRE2_CONFIG_COMPILED_WIDTHS = 14;
//Provides: PCRE2_CONFIG_TABLES_LENGTH const
const PCRE2_CONFIG_TABLES_LENGTH = 15;

//Provides: PCRE2_INFO_ALLOPTIONS const
const PCRE2_INFO_ALLOPTIONS = 0;
//Provides: PCRE2_INFO_ARGOPTIONS const
const PCRE2_INFO_ARGOPTIONS = 1;
//Provides: PCRE2_INFO_BACKREFMAX const
const PCRE2_INFO_BACKREFMAX = 2;
//Provides: PCRE2_INFO_BSR const
const PCRE2_INFO_BSR = 3;
//Provides: PCRE2_INFO_CAPTURECOUNT const
const PCRE2_INFO_CAPTURECOUNT = 4;
//Provides: PCRE2_INFO_FIRSTCODEUNIT const
const PCRE2_INFO_FIRSTCODEUNIT = 5;
//Provides: PCRE2_INFO_FIRSTCODETYPE const
const PCRE2_INFO_FIRSTCODETYPE = 6;
//Provides: PCRE2_INFO_FIRSTBITMAP const
const PCRE2_INFO_FIRSTBITMAP = 7;
//Provides: PCRE2_INFO_HASCRORLF const
const PCRE2_INFO_HASCRORLF = 8;
//Provides: PCRE2_INFO_JCHANGED const
const PCRE2_INFO_JCHANGED = 9;
//Provides: PCRE2_INFO_JITSIZE const
const PCRE2_INFO_JITSIZE = 10;
//Provides: PCRE2_INFO_LASTCODEUNIT const
const PCRE2_INFO_LASTCODEUNIT = 11;
//Provides: PCRE2_INFO_LASTCODETYPE const
const PCRE2_INFO_LASTCODETYPE = 12;
//Provides: PCRE2_INFO_MATCHEMPTY const
const PCRE2_INFO_MATCHEMPTY = 13;
//Provides: PCRE2_INFO_MATCHLIMIT const
const PCRE2_INFO_MATCHLIMIT = 14;
//Provides: PCRE2_INFO_MAXLOOKBEHIND const
const PCRE2_INFO_MAXLOOKBEHIND = 15;
//Provides: PCRE2_INFO_MINLENGTH const
const PCRE2_INFO_MINLENGTH = 16;
//Provides: PCRE2_INFO_NAMECOUNT const
const PCRE2_INFO_NAMECOUNT = 17;
//Provides: PCRE2_INFO_NAMEENTRYSIZE const
const PCRE2_INFO_NAMEENTRYSIZE = 18;
//Provides: PCRE2_INFO_NAMETABLE const
const PCRE2_INFO_NAMETABLE = 19;
//Provides: PCRE2_INFO_NEWLINE const
const PCRE2_INFO_NEWLINE = 20;
//Provides: PCRE2_INFO_DEPTHLIMIT const
const PCRE2_INFO_DEPTHLIMIT = 21;
//Provides: PCRE2_INFO_RECURSIONLIMIT const
const PCRE2_INFO_RECURSIONLIMIT = 21;  /* Obsolete synonym */
//Provides: PCRE2_INFO_SIZE const
const PCRE2_INFO_SIZE = 22;
//Provides: PCRE2_INFO_HASBACKSLASHC const
const PCRE2_INFO_HASBACKSLASHC = 23;
//Provides: PCRE2_INFO_FRAMESIZE const
const PCRE2_INFO_FRAMESIZE = 24;
//Provides: PCRE2_INFO_HEAPLIMIT const
const PCRE2_INFO_HEAPLIMIT = 25;
//Provides: PCRE2_INFO_EXTRAOPTIONS const
const PCRE2_INFO_EXTRAOPTIONS = 26;


/* "Expected" matching error codes: no match and partial match. */
//Provides: PCRE2_ERROR_NOMATCH const
const PCRE2_ERROR_NOMATCH = (-1);
//Provides: PCRE2_ERROR_PARTIAL const
const PCRE2_ERROR_PARTIAL = (-2);

/* Error codes for UTF-8 validity checks */
//Provides: PCRE2_ERROR_UTF8_ERR1 const
const PCRE2_ERROR_UTF8_ERR1 = (-3);
//Provides: PCRE2_ERROR_UTF8_ERR2 const
const PCRE2_ERROR_UTF8_ERR2 = (-4);
//Provides: PCRE2_ERROR_UTF8_ERR3 const
const PCRE2_ERROR_UTF8_ERR3 = (-5);
//Provides: PCRE2_ERROR_UTF8_ERR4 const
const PCRE2_ERROR_UTF8_ERR4 = (-6);
//Provides: PCRE2_ERROR_UTF8_ERR5 const
const PCRE2_ERROR_UTF8_ERR5 = (-7);
//Provides: PCRE2_ERROR_UTF8_ERR6 const
const PCRE2_ERROR_UTF8_ERR6 = (-8);
//Provides: PCRE2_ERROR_UTF8_ERR7 const
const PCRE2_ERROR_UTF8_ERR7 = (-9);
//Provides: PCRE2_ERROR_UTF8_ERR8 const
const PCRE2_ERROR_UTF8_ERR8 = (-10);
//Provides: PCRE2_ERROR_UTF8_ERR9 const
const PCRE2_ERROR_UTF8_ERR9 = (-11);
//Provides: PCRE2_ERROR_UTF8_ERR10 const
const PCRE2_ERROR_UTF8_ERR10 = (-12);
//Provides: PCRE2_ERROR_UTF8_ERR11 const
const PCRE2_ERROR_UTF8_ERR11 = (-13);
//Provides: PCRE2_ERROR_UTF8_ERR12 const
const PCRE2_ERROR_UTF8_ERR12 = (-14);
//Provides: PCRE2_ERROR_UTF8_ERR13 const
const PCRE2_ERROR_UTF8_ERR13 = (-15);
//Provides: PCRE2_ERROR_UTF8_ERR14 const
const PCRE2_ERROR_UTF8_ERR14 = (-16);
//Provides: PCRE2_ERROR_UTF8_ERR15 const
const PCRE2_ERROR_UTF8_ERR15 = (-17);
//Provides: PCRE2_ERROR_UTF8_ERR16 const
const PCRE2_ERROR_UTF8_ERR16 = (-18);
//Provides: PCRE2_ERROR_UTF8_ERR17 const
const PCRE2_ERROR_UTF8_ERR17 = (-19);
//Provides: PCRE2_ERROR_UTF8_ERR18 const
const PCRE2_ERROR_UTF8_ERR18 = (-20);
//Provides: PCRE2_ERROR_UTF8_ERR19 const
const PCRE2_ERROR_UTF8_ERR19 = (-21);
//Provides: PCRE2_ERROR_UTF8_ERR20 const
const PCRE2_ERROR_UTF8_ERR20 = (-22);
//Provides: PCRE2_ERROR_UTF8_ERR21 const
const PCRE2_ERROR_UTF8_ERR21 = (-23);

/* Miscellaneous error codes for pcre2[_dfa]_match(), substring extraction
 * functions, context functions, and serializing functions. They are in
 * numerical order. Originally they were in alphabetical order too, but now
 * that PCRE2 is released, the numbers must not be changed. */
//Provides: PCRE2_ERROR_BADDATA const
const PCRE2_ERROR_BADDATA = (-29);
//Provides: PCRE2_ERROR_MIXEDTABLES const
const PCRE2_ERROR_MIXEDTABLES = (-30);  /* Name was changed */
//Provides: PCRE2_ERROR_BADMAGIC const
const PCRE2_ERROR_BADMAGIC = (-31);
//Provides: PCRE2_ERROR_BADMODE const
const PCRE2_ERROR_BADMODE = (-32);
//Provides: PCRE2_ERROR_BADOFFSET const
const PCRE2_ERROR_BADOFFSET = (-33);
//Provides: PCRE2_ERROR_BADOPTION const
const PCRE2_ERROR_BADOPTION = (-34);
//Provides: PCRE2_ERROR_BADREPLACEMENT const
const PCRE2_ERROR_BADREPLACEMENT = (-35);
//Provides: PCRE2_ERROR_BADUTFOFFSET const
const PCRE2_ERROR_BADUTFOFFSET = (-36);
//Provides: PCRE2_ERROR_CALLOUT const
const PCRE2_ERROR_CALLOUT = (-37);  /* Never used by PCRE2 itself */
//Provides: PCRE2_ERROR_DFA_BADRESTART const
const PCRE2_ERROR_DFA_BADRESTART = (-38);
//Provides: PCRE2_ERROR_DFA_RECURSE const
const PCRE2_ERROR_DFA_RECURSE = (-39);
//Provides: PCRE2_ERROR_DFA_UCOND const
const PCRE2_ERROR_DFA_UCOND = (-40);
//Provides: PCRE2_ERROR_DFA_UFUNC const
const PCRE2_ERROR_DFA_UFUNC = (-41);
//Provides: PCRE2_ERROR_DFA_UITEM const
const PCRE2_ERROR_DFA_UITEM = (-42);
//Provides: PCRE2_ERROR_DFA_WSSIZE const
const PCRE2_ERROR_DFA_WSSIZE = (-43);
//Provides: PCRE2_ERROR_INTERNAL const
const PCRE2_ERROR_INTERNAL = (-44);
//Provides: PCRE2_ERROR_JIT_BADOPTION const
const PCRE2_ERROR_JIT_BADOPTION = (-45);
//Provides: PCRE2_ERROR_JIT_STACKLIMIT const
const PCRE2_ERROR_JIT_STACKLIMIT = (-46);
//Provides: PCRE2_ERROR_MATCHLIMIT const
const PCRE2_ERROR_MATCHLIMIT = (-47);
//Provides: PCRE2_ERROR_NOMEMORY const
const PCRE2_ERROR_NOMEMORY = (-48);
//Provides: PCRE2_ERROR_NOSUBSTRING const
const PCRE2_ERROR_NOSUBSTRING = (-49);
//Provides: PCRE2_ERROR_NOUNIQUESUBSTRING const
const PCRE2_ERROR_NOUNIQUESUBSTRING = (-50);
//Provides: PCRE2_ERROR_NULL const
const PCRE2_ERROR_NULL = (-51);
//Provides: PCRE2_ERROR_RECURSELOOP const
const PCRE2_ERROR_RECURSELOOP = (-52);
//Provides: PCRE2_ERROR_DEPTHLIMIT const
const PCRE2_ERROR_DEPTHLIMIT = (-53);
//Provides: PCRE2_ERROR_RECURSIONLIMIT const
const PCRE2_ERROR_RECURSIONLIMIT = (-53);  /* Obsolete synonym */
//Provides: PCRE2_ERROR_UNAVAILABLE const
const PCRE2_ERROR_UNAVAILABLE = (-54);
//Provides: PCRE2_ERROR_UNSET const
const PCRE2_ERROR_UNSET = (-55);
//Provides: PCRE2_ERROR_BADOFFSETLIMIT const
const PCRE2_ERROR_BADOFFSETLIMIT = (-56);
//Provides: PCRE2_ERROR_BADREPESCAPE const
const PCRE2_ERROR_BADREPESCAPE = (-57);
//Provides: PCRE2_ERROR_REPMISSINGBRACE const
const PCRE2_ERROR_REPMISSINGBRACE = (-58);
//Provides: PCRE2_ERROR_BADSUBSTITUTION const
const PCRE2_ERROR_BADSUBSTITUTION = (-59);
//Provides: PCRE2_ERROR_BADSUBSPATTERN const
const PCRE2_ERROR_BADSUBSPATTERN = (-60);
//Provides: PCRE2_ERROR_TOOMANYREPLACE const
const PCRE2_ERROR_TOOMANYREPLACE = (-61);
//Provides: PCRE2_ERROR_BADSERIALIZEDDATA const
const PCRE2_ERROR_BADSERIALIZEDDATA = (-62);
//Provides: PCRE2_ERROR_HEAPLIMIT const
const PCRE2_ERROR_HEAPLIMIT = (-63);
//Provides: PCRE2_ERROR_CONVERT_SYNTAX const
const PCRE2_ERROR_CONVERT_SYNTAX = (-64);
//Provides: PCRE2_ERROR_INTERNAL_DUPMATCH const
const PCRE2_ERROR_INTERNAL_DUPMATCH = (-65);
//Provides: PCRE2_ERROR_DFA_UINVALID_UTF const
const PCRE2_ERROR_DFA_UINVALID_UTF = (-66);
//Provides: PCRE2_ERROR_INVALIDOFFSET const
const PCRE2_ERROR_INVALIDOFFSET = (-67);


//Provides: pcre2_exc_Error const
var pcre2_exc_Error = undefined;
//Provides: pcre2_exc_Backtrack const
var pcre2_exc_Backtrack = undefined;
//Provides: var_Start_only const
var var_Start_only = undefined;
//Provides: var_ANCHORED const
var var_ANCHORED = undefined;
//Provides: var_Char const
var var_Char = undefined;

//Provides: pcre2_auto_malloc
//Requires: libpcre2
function pcre2_auto_malloc(sizes, func) {
    const ptrs = sizes.map((size) => libpcre2._malloc(size));
    try {
        return func(ptrs);
    } finally {
        ptrs.forEach((ptr) => {
            libpcre2._free(ptr);
        });
    }
}

//Provides: raise_pcre2_error
//Requires: caml_raise_with_arg, pcre2_exc_Error
function raise_pcre2_error(v_arg) {
    caml_raise_with_arg(pcre2_exc_Error, v_arg);
}

//Provides: raise_partial
//Requires: raise_pcre2_error
function raise_partial() {raise_pcre2_error(0);}
//Provides: raise_bad_utf
//Requires: raise_pcre2_error
function raise_bad_utf() {raise_pcre2_error(1);}
//Provides: raise_bad_utf_offset
//Requires: raise_pcre2_error
function raise_bad_utf_offset() {raise_pcre2_error(2);}
//Provides: raise_match_limit
//Requires: raise_pcre2_error
function raise_match_limit() {raise_pcre2_error(3);}
//Provides: raise_depth_limit
//Requires: raise_pcre2_error
function raise_depth_limit() {raise_pcre2_error(4);}
//Provides: raise_workspace_size
//Requires: raise_pcre2_error
function raise_workspace_size() {raise_pcre2_error(5);}

//Provides: raise_bad_pattern
//Requires: pcre2_auto_malloc, libpcre2, raise_pcre2_error
function raise_bad_pattern(code, pos) {
    const msg = pcre2_auto_malloc([128], ([buf]) => {
        // TODO: ideally we check this for error code and resize buf, but
        // this is fine for now.
        // NOTE: The length here is in PCRE2 *code points*. Since we're using
        // 8-bit bindings, this means it happens to be the same as bytes.
        libpcre2._pcre2_get_error_message_8(code, buf, 128);
        return libpcre2.UTF8ToString(buf);
    })
    // TODO: may need caml_string_of_jsbytes but seems like we can just pass a
    // string back?
    raise_pcre2_error([msg, pos]);
}

//Provides: raise_internal_error
//Requires: raise_pcre2_error
function raise_internal_error(msg) {
    raise_pcre2_error(msg);
}

//Provides: pcre2_ocaml_init
//Requires: caml_named_value, caml_hash_variant, pcre2_exc_Error, pcre2_exc_Backtrack, var_Start_only, var_Char, var_ANCHORED
function pcre2_ocaml_init() {
    pcre2_exc_Error = caml_named_value("Pcre2.Error");
    pcre2_exc_Backtrack = caml_named_value("Pcre2.Error");

    var_Start_only = caml_hash_variant("Start_only");
    var_ANCHORED = caml_hash_variant("ANCHORED");
    var_Char = caml_hash_variant("Char");
}

//Provides: pcre2_config_i32
//Requires: libpcre2, pcre2_auto_malloc
function pcre2_config_i32(what) {
    return pcre2_auto_malloc([4], ([ptr]) => {
        libpcre2._pcre2_config_8(what, ptr);
        return libpcre2.getValue(ptr, "i32");
    });
}

//Provides: pcre2_version_stub const
//Requires: libpcre2, PCRE2_CONFIG_VERSION, pcre2_auto_malloc
function pcre2_version_stub() {
    return pcre2_auto_malloc([32], ([buf]) => {
        libpcre2._pcre2_config_8(PCRE2_CONFIG_VERSION, buf);
        return libpcre2.UTF8ToString(buf);
    });
}

//Provides: pcre2_config_unicode_stub
//Requires: pcre2_config_i32, PCRE2_CONFIG_UNICODE
function pcre2_config_unicode_stub() {return pcre2_config_i32(PCRE2_CONFIG_UNICODE);}

//Provides: pcre2_config_newline_stub
//Requires: pcre2_config_i32, PCRE2_CONFIG_NEWLINE
function pcre2_config_newline_stub() {return pcre2_config_i32(PCRE2_CONFIG_NEWLINE);}

//Provides: pcre2_config_link_size_stub_bc
//Requires: pcre2_config_i32, PCRE2_CONFIG_LINKSIZE
function pcre2_config_link_size_stub_bc() {return pcre2_config_i32(PCRE2_CONFIG_LINKSIZE);}

//Provides: pcre2_config_match_limit_stub_bc
//Requires: pcre2_config_i32, PCRE2_CONFIG_MATCHLIMIT
function pcre2_config_match_limit_stub_bc() {return pcre2_config_i32(PCRE2_CONFIG_MATCHLIMIT);}

//Provides: pcre2_config_depth_limit_stub_bc
//Requires: pcre2_config_i32, PCRE2_CONFIG_DEPTHLIMIT
function pcre2_config_depth_limit_stub_bc() {return pcre2_config_i32(PCRE2_CONFIG_DEPTHLIMIT);}

//Provides: pcre2_config_stackrecurse_stub
//Requires: pcre2_config_i32, PCRE2_CONFIG_STACKRECURSE
function pcre2_config_stackrecurse_stub() {return pcre2_config_i32(PCRE2_CONFIG_STACKRECURSE);}

//Provides: cstring_of_jsstring
//Requires: libpcre2
function cstring_of_jsstring(js_string) {
    const array = libpcre2.intArrayFromString(js_string);
    const length = array.length;
    const ptr = libpcre2._malloc(length);
    libpcre2.writeArrayToMemory(array, ptr);
    return [ptr, length - 1];
}

//Provides: pcre2_compile_stub_bc
//Requires: libpcre2, caml_jsstring_of_string, cstring_of_jsstring, pcre2_auto_malloc, raise_bad_pattern, NULL
function pcre2_compile_stub_bc(v_opt, v_tables, v_pat) {
    var v_pat = caml_jsstring_of_string(v_pat);
    const regexp_ptr = pcre2_auto_malloc([4, 4, 4], ([error_code_ptr, error_pos_ptr, regex_size_ptr]) => {
        if (v_tables != NULL) {
            throw new Error("v_tables not supported");
        }

        const [str, len] = cstring_of_jsstring(v_pat);
        const regex = libpcre2._pcre2_compile_8(
            str,
            len,
            v_opt,
            error_code_ptr,
            error_pos_ptr,
            NULL
        );
        libpcre2._free(str);

        /* Raises appropriate exception with [BadPattern] if the pattern could
         * not be compiled */
        if (regex == NULL) {
            raise_bad_pattern(libpcre2.getValue(error_code_ptr, "i32"),
                libpcre2.getValue(error_pos_ptr, "i32"))
        }
        /* It's unknown at this point whether JIT compilation is going to be
         * used, but we have to decide on a size.  Tests with some simple
         * patterns indicate a roughly 50% increase in size when studying
         * without JIT.  A factor of two times hence seems like a reasonable
         * bound to use here. */
        // TODO: We skip this in the libpcre bindings. Doing the same here for
        // now.
        // libpcre2._pcre2_pattern_info_8(regex, PCRE2_INFO_SIZE, regex_size_ptr);

        return regex;
    });

    // TODO: C version allocates with caml_alloc_custom_mem, but we don't in the
    // libpcre bindings. Check why with Tom.
    return {
        rex: regexp_ptr,
        match_context: libpcre2._pcre2_match_context_create_8(NULL),
    };
}

//Provides: pcre2_set_imp_match_limit_stub_bc
//Requires: libpcre2
function pcre2_set_imp_match_limit_stub_bc(v_rex, v_lim) {
    libpcre2._pcre2_set_match_limit_8(v_rex.match_context, v_lim);
    return v_rex;
}

//Provides: pcre2_set_imp_depth_limit_stub_bc
//Requires: libpcre2
function pcre2_set_imp_depth_limit_stub_bc(v_rex, v_lim) {
    libpcre2._pcre2_set_depth_limit_8(v_rex.match_context, v_lim);
    return v_rex;
}

//Provides: pcre2_pattern_info_stub
//Requires: libpcre2
function pcre2_pattern_info_stub(v_rex, what, where) {
    return libpcre2._pcre2_pattern_info_8(v_rex.rex, what, where);
}

//Provides: make_intnat_info
//Requires: pcre2_auto_malloc, pcre2_pattern_info_stub, raise_internal_error
function make_intnat_info(size, ty, name, option, v_rex) {
    return pcre2_auto_malloc([size], ([ptr]) => {
        const ret = pcre2_pattern_info_stub(v_rex, option, ptr);
        if (ret != 0) {
            raise_internal_error(name);
        }
        return libpcre2.getValue(ptr, ty);
    });
}

//Provides: pcre2_size_stub_bc
//Requires: libpcre2, make_intnat_info, PCRE2_INFO_SIZE
function pcre2_size_stub_bc(v_rex) {
    return make_intnat_info(4, "i32", "pcre2_size_stub_bc", PCRE2_INFO_SIZE, v_rex);
}

//Provides: pcre2_capturecount_stub_bc
//Requires: libpcre2, make_intnat_info, PCRE2_INFO_CAPTURECOUNT
function pcre2_capturecount_stub_bc(v_rex) {
    return make_intnat_info(4, "i32", "pcre2_capturecount_stub_bc", PCRE2_INFO_CAPTURECOUNT, v_rex);
}

//Provides: pcre2_backrefmax_stub_bc
//Requires: libpcre2, make_intnat_info, PCRE2_INFO_BACKREFMAX
function pcre2_backrefmax_stub_bc(v_rex) {
    return make_intnat_info(4, "i32", "pcre2_backrefmax_stub_bc", PCRE2_INFO_BACKREFMAX, v_rex);
}

//Provides: pcre2_namecount_stub_bc
//Requires: libpcre2, make_intnat_info, PCRE2_INFO_NAMECOUNT
function pcre2_namecount_stub_bc(v_rex) {
    return make_intnat_info(4, "i32", "pcre2_namecount_stub_bc", PCRE2_INFO_NAMECOUNT, v_rex);
}

//Provides: pcre2_nameentrysize_stub_bc
//Requires: libpcre2, make_intnat_info, PCRE2_INFO_NAMEENTRYSIZE
function pcre2_nameentrysize_stub_bc(v_rex) {
    return make_intnat_info(4, "i32", "pcre2_nameentrysize_stub_bc", PCRE2_INFO_NAMEENTRYSIZE, v_rex);
}

//Provides: pcre2_argoptions_stub
//Requires: libpcre2, make_intnat_info, caml_int64_create_lo_hi, PCRE2_INFO_ARGOPTIONS
function pcre2_argoptions_stub(v_rex) {
    const val = make_intnat_info(4, "i32", "pcre2_argoptions_stub", PCRE2_INFO_ARGOPTIONS, v_rex);
    return caml_int64_create_lo_hi(val, 0);
}

//Provides: pcre2_firstcodeunit_stub
//Requires: libpcre2, raise_internal_error, pcre2_auto_malloc, PCRE2_INFO_FIRSTCODETYPE, PCRE2_INFO_FIRSTCODEUNIT, var_Char, var_Start_only, var_ANCHORED
function pcre2_firstcodeunit_stub(v_rex) {
    const firstcodetype = pcre2_auto_malloc([4], ([firstcodetype_ptr]) => {
        const ret = libpcre2._pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODETYPE, firstcodetype_ptr);
        if (ret != 0) {
            raise_internal_error("pcre2_firstcodeunit_stub");
        }
        return libpcre2.getValue(firstcodetype_ptr, "i32");
    });

    switch (firstcodetype) {
        case 2: return var_Start_only;
        case 0: return var_ANCHORED;
        case 1: {
            const firstcodeunit = pcre2_auto_malloc([4], ([firstcodeunit_ptr]) => {
                const ret = libpcre2._pcre2_pattern_info_stub(v_rex, PCRE2_INFO_FIRSTCODEUNIT, firstcodeunit_ptr);
                if (ret != 0) {
                    raise_internal_error("pcre2_firstcodeunit_stub");
                }
                return libpcre2.getValue(firstcodeunit_ptr, "i32");
            });

            return [var_Char, firstcodeunit];
        }
    }
}

//Provides: pcre2_lastcodeunit_stub
//Requires: libpcre2, pcre2_auto_malloc, PCRE2_INFO_LASTCODETYPE, PCRE2_INFO_LASTCODEUNIT, raise_internal_error
function pcre2_lastcodeunit_stub(v_rex) {
    const lastcodetype = pcre2_auto_malloc([4], ([lastcodetype_ptr]) => {
        const ret = libpcre2._pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODETYPE, lastcodetype_ptr);
        if (ret != 0) {
            raise_internal_error("pcre2_lastcodeunit_stub");
        }
        return libpcre2.getValue(lastcodetype_ptr, "i32");
    });

    if (lastcodetype == 0) {
        return 0; /* None */
    } else if (lastcodetype != 1) {
        raise_internal_error("pcre2_lastcodeunit_stub");
    } else {
        const lastcodeunit = pcre2_auto_malloc([4], ([lastcodeunit_ptr]) => {
            const ret = libpcre2._pcre2_pattern_info_stub(v_rex, PCRE2_INFO_LASTCODEUNIT, lastcodeunit_ptr);
            if (ret != 0) {
                raise_internal_error("pcre2_lastcodeunit_stub");
            }
            return libpcre2.getValue(lastcodeunit_ptr, "i32");
        });

        return [0, lastcodeunit];
    }
}

//Provides: handle_match_error
//Requires: caml_raise_not_found, raise_partial, raise_match_limit, raise_bad_utf_offset, raise_depth_limit, raise_workspace_size, raise_bad_utf, raise_internal_error, PCRE2_ERROR_NOMATCH, PCRE2_ERROR_PARTIAL, PCRE2_ERROR_MATCHLIMIT, PCRE2_ERROR_BADUTFOFFSET, PCRE2_ERROR_DEPTHLIMIT, PCRE2_ERROR_DFA_WSSIZE, PCRE2_ERROR_UTF8_ERR1, PCRE2_ERROR_UTF8_ERR21
function handle_match_error(loc, ret) {
    switch (ret) {
        case PCRE2_ERROR_NOMATCH: caml_raise_not_found(); break;
        case PCRE2_ERROR_PARTIAL: raise_partial(); break;
        case PCRE2_ERROR_MATCHLIMIT: raise_match_limit(); break;
        case PCRE2_ERROR_BADUTFOFFSET: raise_bad_utf_offset(); break;
        case PCRE2_ERROR_DEPTHLIMIT: raise_depth_limit(); break;
        case PCRE2_ERROR_DFA_WSSIZE: raise_workspace_size(); break;
        default:
            // "backwards" since the error codes are negative values
            if (PCRE2_ERROR_UTF8_ERR21 <= ret && ret <= PCRE2_ERROR_UTF8_ERR1) {
                raise_bad_utf();
            }
            /* Unknown error */
            raise_internal_error(`${loc}: unhandled PCRE2 error code (${ret})`);
    }
}

//Provides: handle_pcre2_match_result
//Requires: libpcre2
function handle_pcre2_match_result(ovec_ptr, v_ovec, ovec_len, subj_start, ret) {
    for (var i = 0; i < ovec_len; i++) {
        const val = libpcre2.getValue(ovec_ptr + i * 4, "i32");
        if (val > -1) {
            v_ovec[i + 1] = val + subj_start;
        } else {
            v_ovec[i + 1] = -1;
        }
    }
}

//Provides: pcre2_match_stub0
//Requires: libpcre2, caml_invalid_argument, cstring_of_jsstring, handle_match_error, handle_pcre2_match_result, NULL
function pcre2_match_stub0(
    v_opt,
    v_rex,
    v_pos,
    v_subj_start,
    v_subj,
    v_ovec,
    v_maybe_cof,
    v_workspace_ptr
) {
    var ret;
    var is_dfa = !!v_workspace_ptr;

    var pos = v_pos;
    var subj_start = v_subj_start;

    var [v_subj_ptr, len] = cstring_of_jsstring(v_subj);

    var ovec_len = v_ovec.length - 1; // Wosize_val(v_ovec)

    if (!(subj_start <= pos && pos <= len)) {
        caml_invalid_argument("Pcre2.pcre2_match_stub: illegal position");
    }

    if (!(0 <= subj_start && subj_start <= len)) {
        caml_invalid_argument("Pcre2.pcre2_match_stub: illegal subject start");
    }

    pos -= subj_start;
    len -= subj_start;

    const code = v_rex.rex;
    const match_context = v_rex.match_context;
    const ocaml_subj_ptr = v_subj_ptr + subj_start;

    const match_data = libpcre2._pcre2_match_data_create_from_pattern_8(code, NULL);

    if (!v_maybe_cof) {
        if (is_dfa) {
            ret = libpcre2._pcre2_dfa_match_8(
                code, ocaml_subj_ptr, len, pos, v_opt, match_data,
                match_context, v_workspace_ptr,
                v_workspace_ptr.length - 1 /* Wosize_val(v_workspace) */
            );
        } else {
            ret = libpcre2._pcre2_match_8(
                code,
                ocaml_subj_ptr,
                len,
                pos,
                v_opt,
                match_data,
                match_context
            );
        }

        const ovec_ptr = libpcre2._pcre2_get_ovector_pointer_8(match_data);

        if (ret < 0) {
            libpcre2._pcre2_match_data_free_8(match_data);
            handle_match_error("pcre2_match_stub", ret);
        } else {
            handle_pcre2_match_result(ovec_ptr, v_ovec, ovec_len, subj_start, ret);
        }
    } else {
        throw new Error("callout functions unimplemented");
    }

    libpcre2._pcre2_match_data_free_8(match_data);
    libpcre2._free(v_subj_ptr);
    return 0 /* Unit */;
}


//Provides: pcre2_match_stub_bc
//Requires: pcre2_match_stub0, NULL
function pcre2_match_stub_bc(
    v_opt,
    v_rex,
    v_pos,
    v_subj_start,
    v_subj,
    v_ovec,
    v_maybe_cof,
) {
    return pcre2_match_stub0(v_opt, v_rex, v_pos, v_subj_start, v_subj, v_ovec, v_maybe_cof, NULL);
}

//Provides: pcre2_names_stub
//Requires: libpcre2, pcre2_auto_malloc, caml_js_to_array, pcre2_pattern_info_stub, raise_internal_error, PCRE2_INFO_NAMECOUNT, PCRE2_INFO_NAMEENTRYSIZE, PCRE2_INFO_NAMETABLE
function pcre2_names_stub(v_rex) {
    return pcre2_auto_malloc(
        [4, 4, 4],
        ([name_count_ptr, entry_size_ptr, tbl_ptr_ptr]) => {
            var ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMECOUNT, name_count_ptr);
            if (ret != 0) raise_internal_error("pcre_names_stub: namecount");

            ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMEENTRYSIZE, entry_size_ptr);
            if (ret != 0) raise_internal_error("pcre_names_stub: nameentrysize");

            ret = pcre2_pattern_info_stub(v_rex, PCRE2_INFO_NAMETABLE, tbl_ptr_ptr);
            if (ret != 0) raise_internal_error("pcre_names_stub: nametable");

            var result = [];

            const name_count = libpcre2.getValue(name_count_ptr, "i32");
            const entry_size = libpcre2.getValue(entry_size_ptr, "i32");
            var tbl_ptr = libpcre2.getValue(tbl_ptr_ptr, "i8*");

            for (var i = 0; i < name_count; ++i) {
                result[i] = libpcre2.UTF8ToString(tbl_ptr + 2);
                tbl_ptr += entry_size;
            }
            return caml_js_to_array(result);
        }
    );
}

//Provides: pcre2_substring_number_from_name_stub_bc
//Requires: libpcre2, caml_invalid_argument, cstring_of_jsstring, PCRE2_ERROR_NOSUBSTRING
function pcre2_substring_number_from_name_stub_bc(v_rex, v_name) {
    const [str, _] = cstring_of_jsstring(v_name);
    const ret = libpcre2._pcre2_substring_number_from_name_8(v_rex.rex, str);
    libpcre2._free(str);

    if (ret == PCRE2_ERROR_NOSUBSTRING) {
        caml_invalid_argument("Named string not found");
    }

    return ret;
}

//Always
//Requires: pcre2_ocaml_init, pcre2_version_stub, pcre2_config_unicode_stub, pcre2_config_newline_stub, pcre2_config_link_size_stub_bc, pcre2_config_match_limit_stub_bc, pcre2_config_depth_limit_stub_bc, pcre2_config_stackrecurse_stub, pcre2_compile_stub_bc, pcre2_match_stub_bc, pcre2_substring_number_from_name_stub_bc
(() => {
    if (globalThis.exposePcreStubsForTesting) {
        module.exports = {
            pcre2_ocaml_init,
            pcre2_version_stub,
            pcre2_config_unicode_stub,
            pcre2_config_newline_stub,
            pcre2_config_link_size_stub_bc,
            pcre2_config_match_limit_stub_bc,
            pcre2_config_depth_limit_stub_bc,
            pcre2_config_stackrecurse_stub,
            pcre2_compile_stub_bc,
            pcre2_match_stub_bc,
            pcre2_substring_number_from_name_stub_bc
        };
    }
})();
