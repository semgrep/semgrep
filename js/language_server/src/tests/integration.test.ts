import { LSFactory } from "../lsp";
import * as assert from "assert";
import * as lsclient from "vscode-languageclient/node";
import * as path from "path";
import * as vscode from "vscode";

const EXPECTED_CAPABILITIES = {
  capabilities: {
    codeActionProvider: true,
    hoverProvider: true,
    textDocumentSync: {
      change: 1,
      openClose: true,
      save: true,
    },
    workspace: {
      fileOperations: {
        didCreate: {
          filters: [
            {
              pattern: {
                glob: "**/*",
              },
            },
          ],
        },
        didDelete: {
          filters: [
            {
              pattern: {
                glob: "**/*",
              },
            },
          ],
        },
        didRename: {
          filters: [
            {
              pattern: {
                glob: "**/*",
              },
            },
          ],
        },
      },
      workspaceFolders: {
        changeNotifications: true,
        supported: true,
      },
    },
  },
  serverInfo: {
    name: "Semgrep",
    version: "0.0.0",
  },
};
const FIXTURES = path.join(__dirname, "../../../fixtures");

suite("Server Features", () => {
  test("Basic Run", async () => {
    const { handleClientMessage, setWriteRef } = await LSFactory();
    const initializeParams = {
      capabilities: {},
      initializationOptions: {
        scan: {
          configuration: [path.join(FIXTURES, "rules.yaml")],
        },
      },
      clientInfo: {
        name: "test",
        version: "0.0.1",
      },
    };
    const promise = new Promise((resolve, reject) => {
      reject;
      setWriteRef((packet: any) => {
        resolve(JSON.parse(packet));
      });
    });
    var response = await handleClientMessage({
      jsonrpc: "2.0",
      id: 0,
      method: "initialize",
      params: initializeParams,
    });
    response.serverInfo.version = "0.0.0";
    assert.deepEqual(response, EXPECTED_CAPABILITIES);
    await handleClientMessage({ jsonrpc: "2.0", method: "initialized" });
    const result: any = await promise;
    assert.equal(result["method"], "window/workDoneProgress/create");
  }).timeout(100000);
});

suite("Server e2e", async () => {
  const documentSelector: lsclient.DocumentSelector = [{ language: "python" }];
  const serverModule = path.join(__dirname, "../server.js");
  const serverOptions: lsclient.ServerOptions = {
    run: { module: serverModule, transport: lsclient.TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: lsclient.TransportKind.ipc,
      options: { execArgv: ["--nolazy", "--inspect=6014"] },
    },
  };
  const clientOptions: lsclient.LanguageClientOptions = {
    documentSelector,
    synchronize: {},
    initializationOptions: {
      scan: {
        configuration: [path.join(FIXTURES, "rules.yaml")],
        onlyGitDirty: false,
      },
    },
    workspaceFolder: {
      index: 0,
      name: "fixtures",
      uri: vscode.Uri.parse(FIXTURES),
    },
    middleware: {},
  };
  (clientOptions as { $testMode?: boolean }).$testMode = true;
  const client = new lsclient.LanguageClient(
    "test svr",
    "Test Language Server",
    serverOptions,
    clientOptions
  );
  client.registerProposedFeatures();

  test("Initialization", async () => {
    await client.start();

    var initializeResult = client.initializeResult;
    if (initializeResult?.serverInfo?.version) {
      initializeResult.serverInfo.version = "0.0.0";
    }
    assert.deepEqual(initializeResult, EXPECTED_CAPABILITIES);
    console.log(initializeResult);
    const result: any = await client.sendRequest("semgrep/loginStatus", {});
    assert.ok(result["loggedIn"] !== undefined);
    const promise = new Promise((resolve, reject) => {
      client.onNotification("window/workDoneProgress/create", (params) => {
        resolve(params);
      });
      reject;
    });
    await client.sendNotification("initialized", {});
    const result2: any = await promise;
    assert.ok(result2["token"]);
  }).timeout(100000);

  test("Diagnostics", async () => {
    await vscode.workspace.openTextDocument(path.join(FIXTURES, "test.py"));
    const promise = new Promise((resolve, reject) => {
      client.onNotification("textDocument/publishDiagnostics", (params) => {
        resolve(params);
      });
      reject;
    });

    const result: any = await promise;
    assert.equal(result?.diagnostics?.length, 1);
  }).timeout(100000);
});
