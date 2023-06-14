const LibYamlFactory = require("./dist/libyaml");

const EXPECTED_LIBYAML_VERSION = [0, 1, 7];
const EXPECTED_LIBYAML_VERSION_STRING = "0.1.7";
const SIZEOF_YAML_PARSER_T = 248;
const SIZEOF_YAML_EVENT_T = 56;
const TEST_YAML_STRING = "foo: bar";
const EXPECTED_EVENT_TYPE_STREAM = [1, 3, 9, 6, 6, 10, 4, 2];

globalThis.caml_string_of_jsstring = (x) => x;

describe("libyaml", () => {
  const libyamlPromise = LibYamlFactory();

  test("size of yaml_parser_t is expected", async () => {
    const libyaml = await libyamlPromise;
    expect(libyaml._get_sizeof_yaml_parser_t()).toBe(SIZEOF_YAML_PARSER_T);
  });

  test("size of yaml_event_t is expected", async () => {
    const libyaml = await libyamlPromise;
    expect(libyaml._get_sizeof_yaml_event_t()).toBe(SIZEOF_YAML_EVENT_T);
  });

  test("can return version string", async () => {
    const libyaml = await libyamlPromise;
    const version = libyaml.UTF8ToString(libyaml._yaml_get_version_string());
    expect(version).toBe(EXPECTED_LIBYAML_VERSION_STRING);
  });

  test("can return version values", async () => {
    const libyaml = await libyamlPromise;
    const majorPtr = libyaml._malloc(4);
    const minorPtr = libyaml._malloc(4);
    const patchPtr = libyaml._malloc(4);
    libyaml._yaml_get_version(majorPtr, minorPtr, patchPtr);
    const major = libyaml.getValue(majorPtr, "i32");
    const minor = libyaml.getValue(minorPtr, "i32");
    const patch = libyaml.getValue(patchPtr, "i32");
    expect([major, minor, patch]).toEqual(EXPECTED_LIBYAML_VERSION);
  });

  test("can parse a yaml string", async () => {
    const libyaml = await libyamlPromise;
    const parserPtr = libyaml._malloc(SIZEOF_YAML_PARSER_T);
    expect(libyaml._yaml_parser_initialize(parserPtr)).toBe(1);

    const stringBytes = libyaml.lengthBytesUTF8(TEST_YAML_STRING);
    const stringPtr = libyaml._malloc(stringBytes + 1);
    libyaml.stringToUTF8(TEST_YAML_STRING, stringPtr, stringBytes + 1);

    libyaml._yaml_parser_set_input_string(parserPtr, stringPtr, stringBytes);

    const eventPtr = libyaml._malloc(SIZEOF_YAML_EVENT_T);
    const eventTypeStream = [];

    for (var i = 0; i < 10; i++) {
      if (!libyaml._yaml_parser_parse(parserPtr, eventPtr)) {
        const errType = libyaml.getValue(parserPtr, "i8");
        throw new Error(`Error while parsing: ${errType}`);
      }
      const eventType = libyaml.getValue(eventPtr, "i8");
      eventTypeStream.push(eventType);
      if (eventType == 2) {
        break;
      }
    }

    expect(eventTypeStream).toEqual(EXPECTED_EVENT_TYPE_STREAM);

    libyaml._yaml_event_delete(eventPtr);
    libyaml._yaml_parser_delete(parserPtr);
  });
});

describe("ocaml-yaml stubs", () => {
  const libyamlPromise = LibYamlFactory();
  globalThis.exposeYamlStubsForTesting = true;
  const stubs = require("../libyaml");
  test("yaml_stub_1_yaml_get_version_string", async () => {
    stubs.set_libyaml_wasm_module(await libyamlPromise);
    const version = stubs.yaml_stub_1_yaml_get_version_string();
    expect(version).toBe(EXPECTED_LIBYAML_VERSION_STRING);
  });
  test("yaml_stub_2_yaml_get_version", async () => {
    stubs.set_libyaml_wasm_module(await libyamlPromise);
    const majorPtr = stubs.ctypes_allocate(1, 4);
    const minorPtr = stubs.ctypes_allocate(1, 4);
    const patchPtr = stubs.ctypes_allocate(1, 4);
    stubs.yaml_stub_2_yaml_get_version(majorPtr, minorPtr, patchPtr);
    const major = stubs.ctypes_read(5, [0, 0, majorPtr]);
    const minor = stubs.ctypes_read(5, [0, 0, minorPtr]);
    const patch = stubs.ctypes_read(5, [0, 0, patchPtr]);
    expect([major, minor, patch]).toEqual(EXPECTED_LIBYAML_VERSION);
  });
  test("yaml_stub_4_yaml_parser_initialize", async () => {
    stubs.set_libyaml_wasm_module(await libyamlPromise);
    const parserPtr = [0, 0, stubs.ctypes_allocate(1, SIZEOF_YAML_PARSER_T)];
    expect(stubs.yaml_stub_4_yaml_parser_initialize(parserPtr)).toBe(1);
  });
  test("yaml_stub_5_yaml_parser_delete", async () => {
    stubs.set_libyaml_wasm_module(await libyamlPromise);
    const parserPtrValue = [
      0,
      0,
      stubs.ctypes_allocate(1, SIZEOF_YAML_PARSER_T),
    ];
    expect(stubs.yaml_stub_4_yaml_parser_initialize(parserPtrValue)).toBe(1);
    stubs.yaml_stub_5_yaml_parser_delete(parserPtrValue);
  });
});
