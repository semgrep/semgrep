const { LSFactory } = require("../dist/index.cjs");
async function main() {
  const ls = await LSFactory();
  try {
    await ls.start();
  } catch (e) {}
}

main();
