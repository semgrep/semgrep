const { spawn } = require("child_process");
const { JSONRPCClient } = require("json-rpc-2.0");

const LSClientFactory = () => {
  const server = spawn("node", ["./tests/test-server.js"]);
  server.stderr.on("data", (data) => {
    console.log(`stderr: ${data}`);
  });
  const client = new JSONRPCClient((jsonRPCRequest) => {
    const content = JSON.stringify(jsonRPCRequest);
    const contentLength = Buffer.byteLength(content, "utf8");
    server.stdin.write(`Content-Length: ${contentLength}\r\n\r\n${content}`);
    return new Promise((resolve, reject) => {
      server.stdout.once("data", (data) => {
        const content = data.toString().split("\r\n\r\n")[1];
        const obj = JSON.parse(content);
        resolve(client.receive(obj));
      });
    });
  });
  const exitPromise = new Promise((resolve, reject) => {
    server.on("close", (code) => {
      console.log(`child process exited with code ${code}`);
      resolve(code);
    });
  });
  return { client, exitPromise };
};

describe("language server", () => {
  test("starts and stops", async () => {
    const { client, exitPromise } = LSClientFactory();
    const result = await client.request("initialize", {
      processId: 1234,
      clientInfo: { name: "Visual Studio Code", version: "1.68.1" },
      locale: "en-us",
      rootPath: "",
      rootUri: "",
      workspaceFolders: [],
      initializationOptions: {
        scan: {
          configuration: [],
          exclude: [],
          include: [],
          jobs: 1,
          maxMemory: 0,
          maxTargetBytes: 0,
          onlyGitDirty: true,
          ci: false,
        },
      },
      capabilities: {},
    });
    expect(result.serverInfo.name).toEqual("Semgrep");
    await client.notify("exit");
    const exitCode = await exitPromise;
    expect(exitCode).toBe(0);
  });
});
