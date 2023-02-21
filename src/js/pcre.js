//Provides: pcre_wasm_module const
const pcre_wasm_module = (() => {
	if (typeof Module !== "object") {
		throw new Error("wasm must be loaded first");
	}
	return Module;
})();

//Provides: NULL const
const NULL = 0;

//Provides: PCRE_CONFIG_UTF8 const
const PCRE_CONFIG_UTF8                    = 0
//Provides: PCRE_CONFIG_NEWLINE const
const PCRE_CONFIG_NEWLINE                 = 1
//Provides: PCRE_CONFIG_LINK_SIZE const
const PCRE_CONFIG_LINK_SIZE               = 2

//Provides: PCRE_CONFIG_MATCH_LIMIT const
const PCRE_CONFIG_MATCH_LIMIT             = 4
//Provides: PCRE_CONFIG_STACKRECURSE const
const PCRE_CONFIG_STACKRECURSE            = 5

//Provides: PCRE_CONFIG_MATCH_LIMIT_RECURSION const
const PCRE_CONFIG_MATCH_LIMIT_RECURSION   = 7

//Provides: PCRE_STUDY_JIT_COMPILE const
const PCRE_STUDY_JIT_COMPILE                = 0x0001

//Provides: PCRE_EXTRA_MATCH_LIMIT const
const PCRE_EXTRA_MATCH_LIMIT           = 0x0002
//Provides: PCRE_EXTRA_MATCH_LIMIT_RECURSION const
const PCRE_EXTRA_MATCH_LIMIT_RECURSION = 0x0010

//Provides: PCRE_INFO_SIZE const
const PCRE_INFO_SIZE               = 1
//Provides: PCRE_INFO_CAPTURECOUNT const
const PCRE_INFO_CAPTURECOUNT       = 2

//Provides: PCRE_ERROR_NOMATCH const
const PCRE_ERROR_NOMATCH          = (-1)

const STRUCT_PCRE_EXTRA = {
	flags: 0,
	study_data_ptr: 4,
	match_limit: 8,
	callout_data_ptr: 12,
	tables_ptr: 16,
	match_limit_recursion: 20,
	mark_ptr: 24,
	executable_jit_ptr: 28
};

const STRUCT_PCRE = {
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
}

function struct_sizeof(struct) {
	return Math.max(...Object.values(struct))+4
}

function print_struct(wasm, ptr, type) {
	console.log(Object.keys(type).map((key) => `${key}: ${wasm.getValue(ptr + type[key], 'i32')}`).join(", "))
}

//Provides: pcre_ocaml_init
function pcre_ocaml_init() {
	
}

//Provides: pcre_version_stub const
//Requires: pcre_wasm_module
function pcre_version_stub() {
	const ptr = pcre_wasm_module._pcre_version()
	const value = pcre_wasm_module.AsciiToString()
	pcre_wasm_module._free(ptr)
	return value
}

//Provides: pcre_config_get_int
//Requires: pcre_wasm_module
function pcre_config_get_int(what) {
	const ptr = pcre_wasm_module._malloc(4)
	pcre_wasm_module._pcre_config(what, ptr)
	const value = pcre_wasm_module.HEAP8[ptr]
	pcre_wasm_module._free(ptr)
	return value
}

//Provides: pcre_config_get_long
//Requires: pcre_wasm_module
function pcre_config_get_long(what) {
	const ptr = pcre_wasm_module._malloc(4)
	pcre_wasm_module._pcre_config(what, ptr)
	const value = pcre_wasm_module.HEAP16[ptr]
	pcre_wasm_module._free(ptr)
	return value
}

//Provides: pcre_config_utf8_stub const
//Requires: pcre_config_get_int, PCRE_CONFIG_UTF8
function pcre_config_utf8_stub() {
	return pcre_config_get_int(PCRE_CONFIG_UTF8)
}

//Provides: pcre_config_newline_stub
//Requires: pcre_config_get_int, PCRE_CONFIG_NEWLINE
function pcre_config_newline_stub() {
	return pcre_config_get_int(PCRE_CONFIG_NEWLINE)
}

//Provides: pcre_config_link_size_stub_bc
//Requires: pcre_config_get_int, PCRE_CONFIG_LINK_SIZE
function pcre_config_link_size_stub_bc() {
	return pcre_config_get_int(PCRE_CONFIG_LINK_SIZE)
}

//Provides: pcre_config_match_limit_stub_bc
//Requires: pcre_config_get_long, PCRE_CONFIG_MATCH_LIMIT
function pcre_config_match_limit_stub_bc() {
	return pcre_config_get_long(PCRE_CONFIG_MATCH_LIMIT)
}

//Provides: pcre_config_match_limit_recursion_stub_bc
//Requires: pcre_config_get_long, PCRE_CONFIG_MATCH_LIMIT_RECURSION
function pcre_config_match_limit_recursion_stub_bc() {
	return pcre_config_get_long(PCRE_CONFIG_MATCH_LIMIT_RECURSION);
}

//Provides: pcre_config_stackrecurse_stub
//Requires: pcre_config_get_int, PCRE_CONFIG_STACKRECURSE
function pcre_config_stackrecurse_stub() {
	return pcre_config_get_int(PCRE_CONFIG_STACKRECURSE)
}

//Provides: pcre_compile_stub_bc
//Requires: PCRE_INFO_SIZE, NULL, pcre_wasm_module
function pcre_compile_stub_bc(v_opt, v_tables, v_pat) {
  //size_t regexp_size, ocaml_regexp_size = sizeof(struct pcre_ocaml_regexp);
  var regexp_info_ptr = pcre_wasm_module._malloc(16);
  var error_ptr = pcre_wasm_module._malloc(4);
  var error_ptr_ptr = pcre_wasm_module._malloc(4);
  pcre_wasm_module.setValue(error_ptr_ptr, error_ptr, 'i32')
  var error_ofs_ptr = pcre_wasm_module._malloc(4);  /* offset in the pattern at which error occurred */

  /* If v_tables = [None], then pointer to tables is NULL, otherwise
     set it to the appropriate value */
  //chartables tables = Is_none(v_tables) ? NULL : get_tables(Field(v_tables, 0));
  if (v_tables != 0) {
	throw new Error("need to do something with v_tables");
  }

  const pattern_ptr = pcre_wasm_module._malloc(v_pat.length*4);  // TODO
  if (v_opt & 0x800) {  // TODO: const
	pcre_wasm_module.stringToUTF8(v_pat, pattern_ptr);
  } else {
	pcre_wasm_module.stringToAscii(v_pat, pattern_ptr);
  }
  
  /* Compiles the pattern */
  const regexp_ptr = pcre_wasm_module._pcre_compile(pattern_ptr, v_opt, error_ptr_ptr, error_ofs_ptr, 0);

  /* Raises appropriate exception with [BadPattern] if the pattern
     could not be compiled */
	if (regexp_ptr == NULL) {
		const errorString = pcre_wasm_module.AsciiToString(pcre_wasm_module.getValue(error_ptr_ptr, 'i32'))
		throw new Error(errorString + " at offset " + pcre_wasm_module.HEAP8[error_ofs_ptr])
	}
  

  /* It's unknown at this point whether the user will study the pattern
     later (probably), or if JIT compilation is going to be used, but we
     have to decide on a size.  Tests with some simple patterns indicate a
     roughly 50% increase in size when studying without JIT.  A factor of
     two times hence seems like a reasonable bound to use here. */
  const fullinfo_result = pcre_wasm_module._pcre_fullinfo(regexp_ptr, 0, PCRE_INFO_SIZE, regexp_info_ptr);


  return {
	regexp_ptr: regexp_ptr,
	extra_ptr: NULL,
	studied: 0,
  }
}

//Provides: pcre_study_stub
//Requires: PCRE_STUDY_JIT_COMPILE, NULL, pcre_wasm_module
function pcre_study_stub(v_rex, v_jit_compile) {
	if (!v_rex.studied) {
		const flags = v_jit_compile ? PCRE_STUDY_JIT_COMPILE : 0
		const error_ptr = pcre_wasm_module._malloc(1024)
		const extra_ptr = pcre_wasm_module._pcre_study(v_rex.regexp_ptr, flags, error_ptr)
		if (pcre_wasm_module.HEAP8[error_ptr] != NULL) {
			throw new Error(`invalid argument: ${pcre_wasm_module.AsciiToString(pcre_wasm_module.HEAP8[error_ptr])}`)
		}
		v_rex.extra_ptr = extra_ptr
		v_rex.studied = 1
	}
	return v_rex
}

//Provides: pcre_set_imp_match_limit_stub_bc
//Requires: PCRE_EXTRA_MATCH_LIMIT, NULL, pcre_wasm_module
function pcre_set_imp_match_limit_stub_bc(v_rex, v_lim) {
	if (v_rex.extra_ptr == NULL) {
		v_rex.extra_ptr = pcre_wasm_module._malloc(struct_sizeof(STRUCT_PCRE_EXTRA))
		pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, PCRE_EXTRA_MATCH_LIMIT, 'i32')
	} else {
		pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, pcre_wasm_module.getValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, 'i32') | PCRE_EXTRA_MATCH_LIMIT, 'i32')
	}
	pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.match_limit, v_lim, 'i32')
	return v_rex
}

//Provides: pcre_set_imp_match_limit_recursion_stub_bc
//Requires; PCRE_EXTRA_MATCH_LIMIT_RECURSION, NULL, pcre_wasm_module
function pcre_set_imp_match_limit_recursion_stub_bc(v_rex, v_lim) {
	if (v_rex.extra_ptr == NULL) {
		v_rex.extra_ptr = pcre_wasm_module._malloc(struct_sizeof(STRUCT_PCRE_EXTRA))
		pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, PCRE_EXTRA_MATCH_LIMIT_RECURSION, 'i32')
	} else {
		pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, pcre_wasm_module.getValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.flags, 'i32') | PCRE_EXTRA_MATCH_LIMIT_RECURSION, 'i32')
	}
	pcre_wasm_module.setValue(v_rex.extra_ptr + STRUCT_PCRE_EXTRA.match_limit_recursion, v_lim, 'i32')
	return v_rex
}

//Provides: pcre_capturecount_stub_bc
//Requires: PCRE_INFO_CAPTURECOUNT, pcre_wasm_module
function pcre_capturecount_stub_bc(v_rex) {
	const options_ptr = pcre_wasm_module._malloc(4)
	const ret = pcre_wasm_module._pcre_fullinfo(v_rex.regexp_ptr, v_rex.extra_ptr, PCRE_INFO_CAPTURECOUNT, options_ptr)
	if (ret != 0) {
		throw new Error("shit")  // TODO
	}
	return pcre_wasm_module.getValue(options_ptr, 'i32')
}

//Provides: handle_exec_error
//Requires: caml_raise_not_found, PCRE_ERROR_NOMATCH
function handle_exec_error(loc, ret) {
	switch (ret) {
		case PCRE_ERROR_NOMATCH:
			caml_raise_not_found()
			return;
		default:
			throw new Error("dont know what to do with error: " + ret)
	}
}

//Provides: pcre_exec_stub_bc
//Requires: handle_exec_error, pcre_wasm_module
function pcre_exec_stub_bc(v_opt, v_rex, v_pos, v_subj_start, v_subj, v_ovec, v_maybe_cof, v_workspace) {
	var ret;
	const is_dfa = !!v_workspace;
	var pos = v_pos
	var len = v_subj.length
	var subj_start = v_subj_start

	const v_subj_ptr = pcre_wasm_module._malloc(v_subj.length)
	pcre_wasm_module.stringToAscii(v_subj, v_subj_ptr)

	var ovec_len = v_ovec.length

	if (pos > len || pos < subj_start) {
		throw new Error("Pcre.pcre_exec_stub: illegal position")
	}

	if (subj_start > len || subj_start < 0) {
		throw new Error("Pcre.pcre_exec_stub: illegal subject start")
	}

	pos -= subj_start;
	len -= subj_start;

	const ocaml_subj_ptr = v_subj_ptr + subj_start;
	const opt = v_opt;

	if (!v_maybe_cof) {
		const ovec_ptr = pcre_wasm_module._malloc(ovec_len * 4)
		if (is_dfa) {
			ret = pcre_wasm_module._pcre_dfa_exec()
		} else {
			ret = pcre_wasm_module._pcre_exec(v_rex.regexp_ptr, v_rex.extra_ptr, ocaml_subj_ptr, len, pos, opt, ovec_ptr, ovec_len)
		}
		if (ret < 0) {
			handle_exec_error("pcre_exec_stub", ret)
		} else {
			for (var i=0; i<ovec_len; i++) {
				v_ovec[i] = pcre_wasm_module.getValue(ovec_ptr + (i*4))
			}
		}
	} else {
		throw new Error("callout functions unimplemented")
	}
}