const pcreFactory = require("../build/libpcre");
var libpcre;

const expectedPcreVersion = "8.45 2021-06-15";

beforeAll(async () => {
  globalThis.PCRE = await pcreFactory();
  libpcre = require("../pcre.js");
});

test(`pcre_version() returns the expected version of pcre (${expectedPcreVersion})`, () => {
  expect(libpcre.pcre_version_stub()).toBe(expectedPcreVersion);
});

test("studies a regex", () => {
  const patternPtr = libpcre.pcre_make_string(0, "ca+t");
  const regex = libpcre.pcre_compile_stub_bc(0, 0, patternPtr);
  expect(regex.studied).toBe(0);
  libpcre.pcre_study_stub(regex, 0);
  expect(regex.studied).toBe(1);
  PCRE._free(patternPtr);
});

test("matches a regex", () => {
  const patternPtr = libpcre.pcre_make_string(0, "^cat$");
  const regex = libpcre.pcre_compile_stub_bc(0, 0, patternPtr);
  console.log(regex);

  const ovec = [0, 0];
  libpcre.pcre_exec_stub_bc(0, regex, 0, 0, "cet", ovec, 0, 0);
  console.log(ovec);
});
