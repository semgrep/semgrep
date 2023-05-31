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

//Provides: ctypes_read
//Requires: libyaml, UInt32
function ctypes_read(primType, buffer) {
  switch (primType) {
    case 0:
      return libyaml.getValue(buffer[2], "i8");
    case 5: // Ctypes_Int
      return libyaml.getValue(buffer[2], "i32");
    case 13: // Ctypes_Size_t
      return libyaml.getValue(buffer[2], "i32");
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
  const { ptr } = yaml_emitter;
  return libyaml._yaml_emitter_initialize(ptr);
}

//Provides: yaml_stub_9_yaml_emitter_delete
//Requires: libyaml
function yaml_stub_9_yaml_emitter_delete(yaml_emitter) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_delete(ptr);
}

//Provides: yaml_stub_10_yaml_emitter_set_output_string
//Requires: libyaml
function yaml_stub_10_yaml_emitter_set_output_string(
  yaml_emitter,
  output_ptr,
  size,
  size_written_ptr
) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_output_string(
    ptr,
    output_ptr,
    size,
    size_written_ptr
  );
}

//Provides: yaml_stub_11_yaml_emitter_set_encoding
//Requires: libyaml
function yaml_stub_11_yaml_emitter_set_encoding(yaml_emitter, encoding) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_encoding(ptr, encoding);
}

//Provides: yaml_stub_12_yaml_emitter_set_canonical
//Requires: libyaml
function yaml_stub_12_yaml_emitter_set_canonical(yaml_emitter, canonical) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_canonical(ptr, canonical);
}

//Provides: yaml_stub_13_yaml_emitter_set_indent
//Requires: libyaml
function yaml_stub_13_yaml_emitter_set_indent(yaml_emitter, indent) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_indent(ptr, indent);
}

//Provides: yaml_stub_14_yaml_emitter_set_width
//Requires: libyaml
function yaml_stub_14_yaml_emitter_set_width(yaml_emitter, width) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_width(ptr, width);
}

//Provides: yaml_stub_15_yaml_emitter_set_unicode
//Requires: libyaml
function yaml_stub_15_yaml_emitter_set_unicode(yaml_emitter, unicode) {
  const { ptr } = yaml_emitter;
  libyaml._yaml_emitter_set_unicode(ptr, unicode);
}

//Provides: yaml_stub_16_yaml_emitter_flush
//Requires: libyaml
function yaml_stub_16_yaml_emitter_flush(yaml_emitter) {
  const { ptr } = yaml_emitter;
  return libyaml._yaml_emitter_flush(ptr);
}

//Provides: yaml_stub_17_yaml_emitter_emit
//Requires: libyaml
function yaml_stub_17_yaml_emitter_emit(yaml_emitter, event_ptr) {
  const { ptr } = yaml_emitter;
  return libyaml._yaml_emitter_emit(ptr, event_ptr);
}

//Provides: yaml_stub_18_yaml_stream_start_event_initialize
//Requires: libyaml
function yaml_stub_18_yaml_stream_start_event_initialize(event, encoding) {
  const { ptr } = event;
  return libyaml._yaml_stream_start_event_initialize(ptr, encoding);
}

//Provides: yaml_stub_19_yaml_stream_end_event_initialize
//Requires: libyaml
function yaml_stub_19_yaml_stream_end_event_initialize(event) {
  const { ptr } = event;
  return libyaml._yaml_stream_end_event_initialize(ptr);
}

// yaml_stub_20_yaml_document_start_event_initialize
// yaml_stub_21_yaml_document_end_event_initialize
// yaml_stub_22_yaml_alias_event_initialize
// yaml_stub_23_yaml_scalar_event_initialize_byte8
// yaml_stub_24_yaml_sequence_start_event_initialize
// yaml_stub_25_yaml_sequence_end_event_initialize
// yaml_stub_26_yaml_mapping_start_event_initialize
// yaml_stub_27_yaml_mapping_end_event_initialize

//Always
//Requires: set_libyaml_wasm_module,ctypes_allocate,yaml_stub_1_yaml_get_version_string,yaml_stub_2_yaml_get_version,yaml_stub_4_yaml_parser_initialize,yaml_stub_5_yaml_parser_delete
(() => {
  if (globalThis.exposeYamlStubsForTesting) {
    module.exports = {
      set_libyaml_wasm_module,
      ctypes_allocate,
      ctypes_read,
      yaml_stub_1_yaml_get_version_string,
      yaml_stub_2_yaml_get_version,
      yaml_stub_4_yaml_parser_initialize,
      yaml_stub_5_yaml_parser_delete,
    };
  }
})();
