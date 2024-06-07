/*
 * This file connects LSP.js and VSCode's LSP api together
 *
 * We could manage all IO related things, like reading from stdin and writing to stdout, here
 * but since we're already in JS lang, let's just use Microsofts Official TM LSP library.
 *
 * this allows us to use this file through node-ipc which is the defacto way to do this stuff
 *
 */
import { LSFactory, LS } from "./semgrep-lsp-bindings";

import { createConnection, Connection } from "vscode-languageserver/node";

const connection: Connection = createConnection();
var server: LS | undefined = undefined;
var idCntr = 0;
connection.onInitialize(async (params) => {
  // this is kind of gross to do this here, but it's simple.
  server = await LSFactory();
  // Tell the LS to use this function to write to stdout
  server.setWriteRef((json) => {
    // Parse JSON from ocaml code
    const packet = JSON.parse(json);
    // extract method and params since that's what the LSP expects
    const method = packet.method;
    const params = packet.params;
    // Send the notification to the client
    connection.sendNotification(method, params);
  });
  // surely there's a better way
  if (!params.initializationOptions) {
    params.initializationOptions = {};
  }
  if (!params.initializationOptions.scan) {
    params.initializationOptions.scan = {};
  }
  const debugTrace = params.initializationOptions?.trace?.server === "verbose";
  server.setTracing(debugTrace);
  // Force 1 job and no timeout
  // since these require unix primitives
  params.initializationOptions.scan.jobs = 1;
  params.initializationOptions.scan.timeout = 0;
  const response = await server.handleClientMessage({
    jsonrpc: "2.0",
    method: "initialize",
    params: params,
    id: idCntr++,
  });
  return response;
});

connection.onRequest(async (method, params) => {
  const json = { jsonrpc: "2.0", method: method, params: params, id: idCntr++ };
  if (server) {
    const response = await server.handleClientMessage(json);
    return response;
  }
  return;
});

connection.onNotification(async (method, params) => {
  const json = { jsonrpc: "2.0", method: method, params: params };
  if (server) {
    await server.handleClientMessage(json);
  }
});

connection.onShutdown(async () => {
  if (server) {
    await server.handleClientMessage({
      jsonrpc: "2.0",
      method: "shutdown",
      params: null,
      id: idCntr++,
    });
  }
});

// Let's go!
connection.listen();
