/**
 * @file HCL grammar for tree-sitter
 * @license Apache-2.0
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const make_grammar = require('./make_grammar');

module.exports = make_grammar('hcl');
