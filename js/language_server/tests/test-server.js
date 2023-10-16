const { LSFactory } = require("../dist/index.cjs");
const { readFile } = require("fs/promises");
async function main() {
  const file = await readFile("tests/stdin", "utf8");
  const lines = file.toString().split("\n");
  // remove last line if empty
  if (lines[lines.length - 1] === "") {
    lines.pop();
  }
  var index = 0;
  const ls = await LSFactory();
  try {
    ls.setReadLine(() => {
      return lines[index++];
    });
    ls.setReadExactly(() => {
      return lines[index++];
    });

    await ls.start();
  } catch (e) {}
}

main();
