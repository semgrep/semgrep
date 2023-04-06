import LibYamlFactory from "../libyaml/libyaml";

export const EngineFactory = async () => {
  globalThis.LibYamlModule = await LibYamlFactory();
  return require("./Main.bc");
};
