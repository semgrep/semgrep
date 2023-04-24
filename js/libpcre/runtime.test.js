const LibPcreFactory = require("./dist/libpcre");

const EXPECTED_VERSION = "8.45 2021-06-15";

describe("pcre-ocaml stubs", () => {
  const libpcrePromise = LibPcreFactory();
  globalThis.exposePcreStubsForTesting = true;
  libpcrePromise.then((wasm) => (globalThis.LibPcreModule = wasm));

  test("version is expected", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    expect(stubs.pcre_version_stub()).toBe(EXPECTED_VERSION);
  });

  test("utf-8 is supported", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    expect(stubs.pcre_config_utf8_stub()).toBe(1);
  });

  test("correctly fails to compile an invalid regex", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    expect(() => stubs.pcre_compile_stub_bc(0, 0, `(`)).toThrow(
      "missing ) at offset 1"
    );
  });

  test("compiles a regex that javascript cant", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    stubs.pcre_compile_stub_bc(
      0,
      0,
      `////(?i)snyk.{0,50}['|\"|\`]?[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}['\"\\s]?`
    );
  });
});
