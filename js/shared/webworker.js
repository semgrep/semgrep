import { ParserFactory } from "./entrypoint";

ParserFactory().then((ParserModule) => {
  onmessage = (message) => {
    const func = ParserModule[message.data[0]];
    if (typeof func !== "function") {
      throw new Error("No such function: " + message.data[0]);
    }

    postMessage(func.apply(this, message.data.slice(1)));
  };

  postMessage("initialized");
});
