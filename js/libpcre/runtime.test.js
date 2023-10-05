const LibPcreFactory = require("./dist/libpcre");

const EXPECTED_VERSION = "8.45 2021-06-15";

const NotFoundError = "not found";
const InvalidArgumentError = "invalid argument";

globalThis.caml_raise_not_found = () => {
  throw new Error(NotFoundError);
};

globalThis.caml_invalid_argument = () => {
  throw new Error(InvalidArgumentError);
};

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

  test("utf-8 is parsed", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    let s = "E2 80 AA";

    let bytes = [...s.matchAll(/[^ ]{1,2}/g)].map((a) => parseInt(a[0], 16));

    stubs.pcre_compile_stub_bc(
      0x0800,
      0,
      `(${Buffer.from(bytes).toString("utf-8")})+`
    );
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

  test("match multiple times with offset", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    const regex = stubs.pcre_compile_stub_bc(0, 0, `([a-z]+)`);
    const subject = "foo.bar.baz.quux";
    const subject_start = 3;
    const ovec = [0, 0, 0, 0];

    stubs.pcre_exec_stub_bc(
      0,
      regex,
      subject_start,
      subject_start,
      subject,
      ovec,
      0,
      0
    );
    expect(subject.slice(ovec[1], ovec[2])).toEqual("bar");

    stubs.pcre_exec_stub_bc(
      0,
      regex,
      ovec[2],
      subject_start,
      subject,
      ovec,
      0,
      0
    );
    expect(subject.slice(ovec[1], ovec[2])).toEqual("baz");

    stubs.pcre_exec_stub_bc(
      0,
      regex,
      ovec[2],
      subject_start,
      subject,
      ovec,
      0,
      0
    );
    expect(subject.slice(ovec[1], ovec[2])).toEqual("quux");

    expect(() =>
      stubs.pcre_exec_stub_bc(0, regex, ovec[2], 0, subject, ovec, 0, 0)
    ).toThrow(NotFoundError);
  });

  test("pcre_get_stringnumber works with named capture groups", async () => {
    await libpcrePromise;
    const stubs = require("../libpcre");
    const regex = stubs.pcre_compile_stub_bc(
      0,
      0,
      "(?<numbers>[0-9]+)(?<letters>[a-z]+)"
    );
    const subject = "123abc";

    stubs.pcre_exec_stub_bc(0, regex, 0, 0, subject, [0, 0, 0, 0], 0, 0);

    expect(stubs.pcre_get_stringnumber_stub_bc(regex, "numbers")).toEqual(1);
    expect(stubs.pcre_get_stringnumber_stub_bc(regex, "letters")).toEqual(2);

    expect(() => stubs.pcre_get_stringnumber_stub_bc(regex, "foobar")).toThrow(
      "invalid argument"
    );
  });
});
