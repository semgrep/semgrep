const LSWasm = require("./language-server-wasm");

export interface LS {
  handleClientMessage: (packet: any) => Promise<any | undefined>;
  setWriteRef: (func: (packet: any) => void) => void;
}

declare global {
  var LibPcreModule: any;
  var LibPcre2Module: any;
}

export const LSFactory = async () => {
  const loadedLSWasm = await LSWasm();
  // libpcre regrettably must be global because semgrep eagerly compiles regexes
  globalThis.LibPcreModule = loadedLSWasm;
  globalThis.LibPcre2Module = loadedLSWasm;
  const { init, handleClientMessage, setWriteRef } = require("./Main.bc");
  init(loadedLSWasm);
  const clientMessageHandler = async (packet: any) => {
    const json = JSON.stringify(packet);
    const result = await handleClientMessage(json);
    if (result) {
      // Parse LSP Response and get result
      return JSON.parse(result)["result"];
    }
    return undefined;
  };

  return {
    handleClientMessage: clientMessageHandler,
    setWriteRef: setWriteRef,
  };
};
