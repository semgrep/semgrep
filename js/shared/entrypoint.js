import ParserModuleFactory from "./semgrep-parser";

const ParserFactory = async () => {
    globalThis.ParserModule = await ParserModuleFactory();
    return require("./Parser.bc");
}

export default ParserFactory;