const { LSFactory } = require("../dist/index.cjs");
const { readFile } = require("fs/promises");

const readBytes = (count) => {
  const fd = process.stdin.fd;
  const buffer = Buffer.allocUnsafe(count);
  for (let i = 0; i < count; ) {
    let result = 0;
    try {
      result = fs.readSync(fd, buffer, i, count - i);
    } catch (error) {
      if (error.code === "EAGAIN") {
        // when there is nothing to read at the current time (Unix)
        //TODO: it is good to slow down loop here or do other tasks in meantime, e.g. async sleep
        continue;
      }
      throw error;
    }
    if (result === 0) {
      throw new Error("Input stream reading error."); // consider to use your own solution on this case
    }
    i += result;
  }
  return buffer;
};
const readLine = () => {
  const bytes = [];
  for (let byte = readBytes(1)[0]; byte !== 10; byte = readBytes(1)[0]) {
    bytes.push(byte);
  }
  return Buffer.from(bytes).toString();
};
const readExactly = (count) => {
  const buffer = readBytes(count);
  if (buffer.length < count) {
    throw new Error("Unexpected end of input stream.");
  }
  return buffer;
};

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

    const a = await ls.start();
  } catch (e) {}
}

main();
