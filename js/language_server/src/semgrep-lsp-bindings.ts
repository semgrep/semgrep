const LSWasm = require("./language-server-c-bindings");

export interface LS {
  handleClientMessage: (packet: any) => Promise<any | undefined>;
  setWriteRef: (func: (packet: any) => void) => void;
}

declare global {
  var LibPcreModule: any;
}

export const LSFactory = async () => {
  const loadedYamlPCRE = await LSWasm();
  // libpcre regrettably must be global because semgrep eagerly compiles regexes
  globalThis.LibPcreModule = loadedYamlPCRE;
  const { init, handleClientMessage, setWriteRef } = require("./Main.bc");
  init(loadedYamlPCRE);
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
