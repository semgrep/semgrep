const semgrepBashFactory = require("../build/semgrep-bash");
var semgrepBash;

beforeAll(async () => {
  semgrepBash = await semgrepBashFactory();
});

test("parses some bash", () => {});
