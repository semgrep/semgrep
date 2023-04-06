//Provides: ctypes_allocate
function ctypes_allocate(count, size) {
  const wasm = globalThis.LibYamlModule;
  console.log(`ctypes_allocate(${count}, ${size})`);
  const ptr = wasm._malloc(count * size);
  console.log(` -> return: ${ptr}`);
  return ptr;
}

//Provides: ctypes_block_address
function ctypes_block_address(a) {
  console.log(`ctypes_block_address(${a})`);
  return a;
}

//Provides: ctypes_write
function ctypes_write(primType, v, buffer) {
  console.log(
    `ctypes_write(primType=${primType}, v=${v}, buffer=${buffer[2]})`
  );
  switch (primType) {
    case 0: // Ctypes_Char
      globalThis.LibYamlModule.setValue(buffer[2], v, "i8");
      return;
    default:
      throw new Error(`dont know how to write prim ${primType}`);
  }
}

//Provides: ctypes_read
function ctypes_read(primType, buffer) {
  console.log(`ctypes_read(primType=${primType}, buffer=${buffer[2]})`);
  switch (primType) {
    case 0:
      return globalThis.LibYamlModule.getValue(buffer[2], "i8");
    case 5: // Ctypes_Int
      return globalThis.LibYamlModule.getValue(buffer[2], "i32");
    case 13: // Ctypes_Size_t
      return globalThis.LibYamlModule.getValue(buffer[2], "i32");
    case 20: // Ctypes_Uint32_t
      return new UInt32(globalThis.LibYamlModule.getValue(buffer[2], "i32"));
    default:
      throw new Error(`how to read prim ${primType}`);
  }
}

//Provides: ctypes_read_pointer
function ctypes_read_pointer(ptr) {
  console.log(`ctypes_read_ptr(ptr=${ptr[2]})`);
  return ptr[2];
}

//Provides: yaml_stub_1_yaml_get_version_string
function yaml_stub_1_yaml_get_version_string() {
  console.log(`yaml_stub_1_yaml_get_version_string()`);
}

//Provides: yaml_stub_2_yaml_get_version
function yaml_stub_2_yaml_get_version(major_ptr, minor_ptr, patch_ptr) {
  console.log(`yaml_stub_2_yaml_get_version()`);
  globalThis.LibYamlModule._get_version(major_ptr, minor_ptr, patch_ptr);
}

//Provides: yaml_stub_3_yaml_token_delete
function yaml_stub_3_yaml_token_delete(yaml_token) {
  const { wasm, ptr } = yaml_token;
  globalThis.LibYamlModule._yaml_token_delete(ptr);
}

//Provides: yaml_stub_4_yaml_parser_initialize
function yaml_stub_4_yaml_parser_initialize(parser_ptr) {
  console.log(`yaml_parser_initialize(${JSON.stringify(parser_ptr[2])})`);
  return globalThis.LibYamlModule._yaml_parser_initialize(parser_ptr[2]);
}

//Provides: yaml_stub_5_yaml_parser_delete
function yaml_stub_5_yaml_parser_delete(yaml_parser) {
  const { wasm, ptr } = yaml_parser;
  wasm._yaml_parser_delete(ptr);
}

//Provides: yaml_stub_6_yaml_parser_set_input_string
function yaml_stub_6_yaml_parser_set_input_string(parser_ptr, input_ptr, size) {
  console.log(
    `yaml_stub_6_yaml_parser_set_input_string(parser_ptr=${
      parser_ptr[parser_ptr.length - 1][1]
    }, input_ptr=${input_ptr[3][1]} (${globalThis.LibYamlModule.AsciiToString(
      input_ptr[3][1]
    )}), size=${size.value})`
  );
  globalThis.LibYamlModule._yaml_parser_set_input_string(
    parser_ptr[parser_ptr.length - 1][1],
    input_ptr[3][1],
    size.value
  );
}

//Provides: yaml_stub_7_yaml_parser_parse
function yaml_stub_7_yaml_parser_parse(parser_ptr, event_ptr) {
  console.log(
    `yaml_stub_7_yaml_parser_parse(parser_ptr=${parser_ptr[2]}, event_ptr=${event_ptr[2]})`
  );
  return globalThis.LibYamlModule._yaml_parser_parse(
    parser_ptr[2],
    event_ptr[2]
  );
}

//Provides: yaml_stub_8_yaml_emitter_initialize
function yaml_stub_8_yaml_emitter_initialize(yaml_emitter) {
  const { wasm, ptr } = yaml_emitter;
  return wasm._yaml_emitter_initialize(ptr);
}

//Provides: yaml_stub_9_yaml_emitter_delete
function yaml_stub_9_yaml_emitter_delete(yaml_emitter) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_delete(ptr);
}

//Provides: yaml_stub_10_yaml_emitter_set_output_string
function yaml_stub_10_yaml_emitter_set_output_string(
  yaml_emitter,
  output_ptr,
  size,
  size_written_ptr
) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_output_string(ptr, output_ptr, size, size_written_ptr);
}

//Provides: yaml_stub_11_yaml_emitter_set_encoding
function yaml_stub_11_yaml_emitter_set_encoding(yaml_emitter, encoding) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_encoding(ptr, encoding);
}

//Provides: yaml_stub_12_yaml_emitter_set_canonical
function yaml_stub_12_yaml_emitter_set_canonical(yaml_emitter, canonical) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_canonical(ptr, canonical);
}

//Provides: yaml_stub_13_yaml_emitter_set_indent
function yaml_stub_13_yaml_emitter_set_indent(yaml_emitter, indent) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_indent(ptr, indent);
}

//Provides: yaml_stub_14_yaml_emitter_set_width
function yaml_stub_14_yaml_emitter_set_width(yaml_emitter, width) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_width(ptr, width);
}

//Provides: yaml_stub_15_yaml_emitter_set_unicode
function yaml_stub_15_yaml_emitter_set_unicode(yaml_emitter, unicode) {
  const { wasm, ptr } = yaml_emitter;
  wasm._yaml_emitter_set_unicode(ptr, unicode);
}

//Provides: yaml_stub_16_yaml_emitter_flush
function yaml_stub_16_yaml_emitter_flush(yaml_emitter) {
  const { wasm, ptr } = yaml_emitter;
  return wasm._yaml_emitter_flush(ptr);
}

//Provides: yaml_stub_17_yaml_emitter_emit
function yaml_stub_17_yaml_emitter_emit(yaml_emitter, event_ptr) {
  const { wasm, ptr } = yaml_emitter;
  return wasm._yaml_emitter_emit(ptr, event_ptr);
}

//Provides: yaml_stub_18_yaml_stream_start_event_initialize
function yaml_stub_18_yaml_stream_start_event_initialize(event, encoding) {
  const { wasm, ptr } = event;
  return wasm._yaml_stream_start_event_initialize(ptr, encoding);
}

//Provides: yaml_stub_19_yaml_stream_end_event_initialize
function yaml_stub_19_yaml_stream_end_event_initialize(event) {
  const { wasm, ptr } = event;
  return wasm._yaml_stream_end_event_initialize(ptr);
}

// yaml_stub_20_yaml_document_start_event_initialize
// yaml_stub_21_yaml_document_end_event_initialize
// yaml_stub_22_yaml_alias_event_initialize
// yaml_stub_23_yaml_scalar_event_initialize_byte8
// yaml_stub_24_yaml_sequence_start_event_initialize
// yaml_stub_25_yaml_sequence_end_event_initialize
// yaml_stub_26_yaml_mapping_start_event_initialize
// yaml_stub_27_yaml_mapping_end_event_initialize
