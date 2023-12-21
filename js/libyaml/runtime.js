//Provides: libyaml
var libyaml;

//Provides: set_libyaml_wasm_module
//Requires: libyaml
function set_libyaml_wasm_module(value) {
  libyaml = value;
}

//Provides: ctypes_allocate
//Requires: libyaml
function ctypes_allocate(count, size) {
  return libyaml._malloc(count * size);
}

//Provides: ctypes_block_address
function ctypes_block_address(a) {
  return a;
}

//Provides: ctypes_write
//Requires: libyaml
function ctypes_write(primType, v, buffer) {
  switch (primType) {
    case 0: // Ctypes_Char
      libyaml.setValue(buffer[2], v, "i8");
      return;
    default:
      throw new Error(`dont know how to write prim ${primType}`);
  }
}

//Provides: ctypes_string_of_cstring
//Requires: libyaml, caml_string_of_jsstring
function ctypes_string_of_cstring(ptr) {
  return caml_string_of_jsstring(libyaml.UTF8ToString(ptr[2]));
}

//Provides: ctypes_cstring_of_string
//Requires: libyaml, caml_string_of_jsstring
function ctypes_cstring_of_string(s) {
  var str = caml_string_of_jsstring(s);
  var ptr;
  const array = libyaml.intArrayFromString(str);
  const length = array.length;
  ptr = libyaml._malloc(length + 1);
  libyaml.writeArrayToMemory(array, ptr);
  return ptr;
}
//
// Provides: ctypes_string_of_array
//Requires: libyaml
function ctypes_string_of_array(ptr, len) {
  var array = new Uint8Array(libyaml.HEAPU8.buffer, ptr[2], len);
  return libyaml.intArrayToString(array);
}

//Provides: caml_float_of_string (const)
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_float_of_string(s) {
  var res;
  s = caml_jsbytes_of_string(s);
  // float_of_string natively fails on strings with whitespace
  // But the Js_of_ocaml does not. This breaks things :(
  // So we check for whitespace and fail if it exists
  if (/\s/g.test(s)) caml_failwith("float_of_string");
  res = +s;
  if (s.length > 0 && res === res) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if ((s.length > 0 && res === res) || /^[+-]?nan$/i.test(s)) return res;
  var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)(p([+-]?[0-9]+))?/i.exec(s);
  //          1        2             3           5
  if (m) {
    var m3 = m[3].replace(/0+$/, "");
    var mantissa = parseInt(m[1] + m[2] + m3, 16);
    var exponent = (m[5] | 0) - 4 * m3.length;
    res = mantissa * Math.pow(2, exponent);
    return res;
  }
  if (/^\+?inf(inity)?$/i.test(s)) return Infinity;
  if (/^-inf(inity)?$/i.test(s)) return -Infinity;
  caml_failwith("float_of_string");
}

//Provides: ctypes_read
//Requires: libyaml, UInt32
function ctypes_read(primType, buffer) {
  switch (primType) {
    case 0:
      return libyaml.getValue(buffer[2], "i8");
    case 5: // Ctypes_Int
      return libyaml.getValue(buffer[2], "i32");
    case 13: // Ctypes_Size_t
      return new UInt32(libyaml.getValue(buffer[2], "i32"));
    case 20: // Ctypes_Uint32_t
      return new UInt32(libyaml.getValue(buffer[2], "i32"));
    default:
      throw new Error(`Don't know how to read prim ${primType}`);
  }
}

//Provides: ctypes_read_pointer
//Requires: libyaml
function ctypes_read_pointer(ptr) {
  return libyaml.getValue(ptr[2], "i32");
}

//Provides: yaml_stub_1_yaml_get_version_string const
//Requires: libyaml, caml_string_of_jsstring
function yaml_stub_1_yaml_get_version_string() {
  return caml_string_of_jsstring(
    libyaml.UTF8ToString(libyaml._yaml_get_version_string())
  );
}

//Provides: yaml_stub_2_yaml_get_version
//Requires: libyaml
function yaml_stub_2_yaml_get_version(majorPtr, minorPtr, patchPtr) {
  libyaml._yaml_get_version(majorPtr, minorPtr, patchPtr);
}

//Provides: yaml_stub_3_yaml_token_delete
//Requires: libyaml
function yaml_stub_3_yaml_token_delete(yaml_token) {
  const { ptr } = yaml_token;
  libyaml._yaml_token_delete(ptr);
}

//Provides: yaml_stub_4_yaml_parser_initialize
//Requires: libyaml
function yaml_stub_4_yaml_parser_initialize(parser_ptr) {
  return libyaml._yaml_parser_initialize(parser_ptr[2]);
}

//Provides: yaml_stub_5_yaml_parser_delete
//Requires: libyaml
function yaml_stub_5_yaml_parser_delete(parser_ptr) {
  return libyaml._yaml_parser_delete(parser_ptr[2]);
}

//Provides: yaml_stub_6_yaml_parser_set_input_string
//Requires: libyaml
function yaml_stub_6_yaml_parser_set_input_string(parser_ptr, input_ptr, size) {
  libyaml._yaml_parser_set_input_string(
    parser_ptr[2],
    input_ptr[2],
    size.value
  );
}

//Provides: yaml_stub_7_yaml_parser_parse
//Requires: libyaml
function yaml_stub_7_yaml_parser_parse(parser_ptr, event_ptr) {
  return libyaml._yaml_parser_parse(parser_ptr[2], event_ptr[2]);
}

//Provides: yaml_stub_8_yaml_emitter_initialize
//Requires: libyaml
function yaml_stub_8_yaml_emitter_initialize(yaml_emitter) {
  return libyaml._yaml_emitter_initialize(yaml_emitter[2]);
}

//Provides: yaml_stub_9_yaml_emitter_delete
//Requires: libyaml
function yaml_stub_9_yaml_emitter_delete(yaml_emitter) {
  libyaml._yaml_emitter_delete(yaml_emitter[2]);
}

//Provides: yaml_stub_10_yaml_emitter_set_output_string
//Requires: libyaml
function yaml_stub_10_yaml_emitter_set_output_string(
  yaml_emitter,
  output_ptr,
  size,
  size_written_ptr
) {
  libyaml._yaml_emitter_set_output_string(
    yaml_emitter[2],
    output_ptr[2],
    size.value,
    size_written_ptr[2]
  );
}

//Provides: yaml_stub_11_yaml_emitter_set_encoding
//Requires: libyaml
function yaml_stub_11_yaml_emitter_set_encoding(yaml_emitter, encoding) {
  libyaml._yaml_emitter_set_encoding(yaml_emitter[2], encoding);
}

//Provides: yaml_stub_12_yaml_emitter_set_canonical
//Requires: libyaml
function yaml_stub_12_yaml_emitter_set_canonical(yaml_emitter, canonical) {
  libyaml._yaml_emitter_set_canonical(yaml_emitter[2], canonical);
}

//Provides: yaml_stub_13_yaml_emitter_set_indent
//Requires: libyaml
function yaml_stub_13_yaml_emitter_set_indent(yaml_emitter, indent) {
  libyaml._yaml_emitter_set_indent(yaml_emitter[2], indent);
}

//Provides: yaml_stub_14_yaml_emitter_set_width
//Requires: libyaml
function yaml_stub_14_yaml_emitter_set_width(yaml_emitter, width) {
  libyaml._yaml_emitter_set_width(yaml_emitter[2], width);
}

//Provides: yaml_stub_15_yaml_emitter_set_unicode
//Requires: libyaml
function yaml_stub_15_yaml_emitter_set_unicode(yaml_emitter, unicode) {
  libyaml._yaml_emitter_set_unicode(yaml_emitter[2], unicode);
}

//Provides: yaml_stub_16_yaml_emitter_flush
//Requires: libyaml
function yaml_stub_16_yaml_emitter_flush(yaml_emitter) {
  return libyaml._yaml_emitter_flush(yaml_emitter[2]);
}

//Provides: yaml_stub_17_yaml_emitter_emit
//Requires: libyaml
function yaml_stub_17_yaml_emitter_emit(yaml_emitter, event_ptr) {
  return libyaml._yaml_emitter_emit(yaml_emitter[2], event_ptr[2]);
}

//Provides: yaml_stub_18_yaml_stream_start_event_initialize
//Requires: libyaml
function yaml_stub_18_yaml_stream_start_event_initialize(event, encoding) {
  return libyaml._yaml_stream_start_event_initialize(event[2], encoding);
}

//Provides: yaml_stub_19_yaml_stream_end_event_initialize
//Requires: libyaml
function yaml_stub_19_yaml_stream_end_event_initialize(event) {
  return libyaml._yaml_stream_end_event_initialize(event[2]);
}

//Provides: yaml_stub_20_yaml_document_start_event_initialize
// Requires: libyaml
function yaml_stub_20_yaml_document_start_event_initialize(
  event,
  version_directive,
  tag_directives_start,
  tag_directives_end,
  implicit
) {
  return libyaml._yaml_document_start_event_initialize(
    event[2],
    version_directive[2],
    tag_directives_start[2],
    tag_directives_end[2],
    implicit
  );
}

//Provides: yaml_stub_21_yaml_document_end_event_initialize
//Requires: libyaml
function yaml_stub_21_yaml_document_end_event_initialize(
  event,
  implicit
) {
  return libyaml._yaml_document_end_event_initialize(
    event[2],
    implicit
  );
}

//Provides: yaml_stub_22_yaml_alias_event_initialize
//Requires: libyaml
function yaml_stub_22_yaml_alias_event_initialize(
  event,
  anchor
) {
  return libyaml._yaml_alias_event_initialize(
    event[2],
    anchor[2]
  );
}

//Provides: yaml_stub_23_yaml_scalar_event_initialize_byte8
//Requires: libyaml
function yaml_stub_23_yaml_scalar_event_initialize_byte8(
  event,
  anchor,
  tag,
  value,
  length,
  plain_implicit,
  quoted_implicit,
  style
) {
  return libyaml._yaml_scalar_event_initialize(
    event[2],
    anchor[2],
    tag[2],
    value[2],
    length,
    plain_implicit,
    quoted_implicit,
    style
  );
}

//Provides: yaml_stub_24_yaml_sequence_start_event_initialize
//Requires: libyaml
function yaml_stub_24_yaml_sequence_start_event_initialize(
  event,
  anchor,
  tag,
  implicit,
  style
) {
  return libyaml._yaml_sequence_start_event_initialize(
    event[2],
    anchor[2],
    tag[2],
    implicit,
    style
  );
}

//Provides: yaml_stub_25_yaml_sequence_end_event_initialize
//Requires: libyaml
function yaml_stub_25_yaml_sequence_end_event_initialize(
  event
) {
  return libyaml._yaml_sequence_end_event_initialize(
    event[2]
  );
}

//Provides: yaml_stub_26_yaml_mapping_start_event_initialize
//Requires: libyaml
function yaml_stub_26_yaml_mapping_start_event_initialize(
  event,
  anchor,
  tag,
  implicit,
  style
) {
  return libyaml._yaml_mapping_start_event_initialize(
    event[2],
    anchor[2],
    tag[2],
    implicit,
    style
  );
}

//Provides: yaml_stub_27_yaml_mapping_end_event_initialize
//Requires: libyaml
function yaml_stub_27_yaml_mapping_end_event_initialize(
  event
) {
  return libyaml._yaml_mapping_end_event_initialize(
    event[2]
  );
}

//Always
//Requires: set_libyaml_wasm_module,ctypes_allocate,yaml_stub_1_yaml_get_version_string,yaml_stub_2_yaml_get_version,yaml_stub_4_yaml_parser_initialize,yaml_stub_5_yaml_parser_delete,yaml_stub_8_yaml_emitter_initialize
(() => {
  if (globalThis.exposeYamlStubsForTesting) {
    module.exports = {
      set_libyaml_wasm_module,
      ctypes_allocate,
      ctypes_read,
      caml_float_of_string,
      yaml_stub_1_yaml_get_version_string,
      yaml_stub_2_yaml_get_version,
      yaml_stub_4_yaml_parser_initialize,
      yaml_stub_5_yaml_parser_delete,
    };
  }
})();
