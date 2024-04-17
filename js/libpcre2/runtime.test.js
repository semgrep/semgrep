const LibPcre2Factory = require("./dist/libpcre2");

const EXPECTED_VERSION = "10.43 2024-02-16";

const NotFoundError = "not found";
const InvalidArgumentError = "invalid argument";

globalThis.caml_raise_not_found = () => {
    throw new Error(NotFoundError);
};

globalThis.caml_invalid_argument = () => {
    throw new Error(InvalidArgumentError);
};

globalThis.caml_jsstring_of_string = (value) => value;

globalThis.caml_raise_with_arg = (error, arg) => {
    throw new Error(`caml raise: {tag: ${error}, arg: ${arg}}`);
};

globalThis.caml_int64_to_int32 = (x) => x;

globalThis.caml_named_value = (name) => {
    switch (name) {
        case "Pcre2.Error":
            return 0; /* Tag? */
        default:
            return undefined;
    }
};

globalThis.caml_hash_variant = (name) => {
    switch (name) {
        // Think these values are arbitrary.
        case "Start_only": return 0;
        case "ANCHORED": return 1;
        case "Char": return 2;
        default:
            return undefined;
    }
};

describe("pcre2-ocaml stubs", () => {
    globalThis.exposePcreStubsForTesting = true;
    const libpcre2Promise = LibPcre2Factory().then((wasm) => (globalThis.LibPcre2Module = wasm));

    test("version is expected", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        expect(stubs.pcre2_version_stub()).toBe(EXPECTED_VERSION);
    });

    test("utf-8 is supported", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        expect(stubs.pcre2_config_unicode_stub()).toBe(1);
    });

    test("utf-8 is parsed", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        const s = "E2 8D BC";

        const bytes = [...s.matchAll(/[^ ]{1,2}/g)].map((a) => parseInt(a[0], 16));
        const re = `(${Buffer.from(bytes).toString("utf-8")})+`;
        stubs.pcre2_compile_stub_bc(
            0x00080000, /* PCRE2_UTF */
            0,
            re,
        );
    });

    test("correctly fails to compile an invalid regex", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        expect(() => stubs.pcre2_compile_stub_bc(0, 0, "(")).toThrow(
            "caml raise: {tag: 0, arg: missing closing parenthesis,1}"
        );
    });

    test("compiles a regex that javascript can't", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        stubs.pcre2_compile_stub_bc(
            0,
            0,
            `////(?i)snyk.{0,50}['|\"|\`]?[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}['\"\\s]?`
        );
    });

    test("match multiple times with offset", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        const regex = stubs.pcre2_compile_stub_bc(0, 0, "([a-z]+)");
        const subject = "foo.bar.baz.quux";
        const subject_start = 3;
        const ovec = [0, 0, 0, 0];

        stubs.pcre2_match_stub_bc(
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

        stubs.pcre2_match_stub_bc(
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

        stubs.pcre2_match_stub_bc(
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
            stubs.pcre2_match_stub_bc(0, regex, ovec[2], 0, subject, ovec, 0, 0)
        ).toThrow(NotFoundError);
    });

    test("pcre_get_stringnumber works with named capture groups", async () => {
        await libpcre2Promise;
        const stubs = require("../libpcre2");
        stubs.pcre2_ocaml_init();
        const regex = stubs.pcre2_compile_stub_bc(
            0,
            0,
            "(?<numbers>[0-9]+)(?<letters>[a-z]+)"
        );
        const subject = "123abc";

        stubs.pcre2_match_stub_bc(0, regex, 0, 0, subject, [0, 0, 0, 0], 0, 0);

        expect(stubs.pcre2_substring_number_from_name_stub_bc(regex, "numbers")).toEqual(1);
        expect(stubs.pcre2_substring_number_from_name_stub_bc(regex, "letters")).toEqual(2);

        expect(() => stubs.pcre2_substring_number_from_name_stub_bc(regex, "foobar")).toThrow(
            "invalid argument"
        );
    });
});
