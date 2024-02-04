#!/usr/bin/env bash
set -eu

WASM=$1
SEMGREP_LANG=$2

cat <<EOF
const {
  createParser,
} = require("../../../../_build/default/js/languages/${SEMGREP_LANG}/Parser.bc");

EOF

if [[ "${WASM}" != "" ]]; then
cat <<EOF
import WasmFactory from "../dist/${WASM}";

const { getDirname } = require("cross-dirname");
export const ParserFactory = async (wasmUri) => {
  if (!wasmUri) {
    wasmUri = getDirname() + "${WASM}.wasm";
  }
  const wasm = await WasmFactory({'locateFile': (uri) => uri === "${WASM}.wasm" ? wasmUri : uri});
  return createParser(wasm);
};
EOF
else
cat <<EOF
export const ParserFactory = async () => {
  return createParser(null);
};
EOF
fi
