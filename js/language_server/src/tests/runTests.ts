import * as path from "path";
import find = require("find-process");

import { runTests } from "@vscode/test-electron";

async function go() {
  const extensionDevelopmentPath = path.resolve(__dirname, "..");
  const extensionTestsPath = __dirname;
  let extensionTestsEnv: NodeJS.ProcessEnv | undefined = undefined;
  if (process.platform === "linux" && !process.env["DISPLAY"]) {
    let display: string | undefined;
    const processes = await find("name", "/usr/bin/Xvfb");
    for (const item of processes) {
      if (item.name !== "Xvfb") {
        continue;
      }
      if (item.cmd !== undefined && item.cmd.length > 0) {
        display = item.cmd.split(" ")[1];
      }
    }
    if (display !== undefined) {
      extensionTestsEnv = { DISPLAY: display };
    }
  }
  await runTests({
    extensionDevelopmentPath,
    extensionTestsPath,
    extensionTestsEnv,
  });
}

go();
