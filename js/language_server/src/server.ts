import { LSFactory, LS } from "./lsp";

import { createConnection, Connection } from "vscode-languageserver/node";

const connection: Connection = createConnection();
var server: LS | undefined = undefined;
var idCntr = 0;
connection.onInitialize(async (params) => {
  server = await LSFactory();
  server.setWriteRef((json) => {
    const packet = JSON.parse(json);
    const method = packet.method;
    const params = packet.params;
    connection.sendNotification(method, params);
  });
  // surely there's a better way
  if (!params.initializationOptions) {
    params.initializationOptions = {};
  }
  if (!params.initializationOptions.scan) {
    params.initializationOptions.scan = {};
  }
  // Force 1 job
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

connection.onNotification((method, params) => {
  const json = { jsonrpc: "2.0", method: method, params: params };
  if (server) {
    server.handleClientMessage(json);
  }
});

connection.onShutdown(() => {
  if (server) {
    server.handleClientMessage({
      jsonrpc: "2.0",
      method: "shutdown",
      params: null,
      id: idCntr++,
    });
  }
});
connection.listen();
