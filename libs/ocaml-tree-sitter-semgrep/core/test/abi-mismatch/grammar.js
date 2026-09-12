/*
  Minimal grammar used only to test ABI-version-mismatch handling.

  The happy path (this grammar parsing 'hello') is checked by the normal
  parse-examples run. The failure path -- a parser whose compiled grammar
  advertises an ABI version outside the runtime's supported range -- is
  forced and checked by ./check-test-output.
*/
module.exports = grammar({
  name: 'abi_mismatch',
  rules: {
    program: $ => 'hello'
  }
});
