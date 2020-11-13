'use strict';

const fs = require('fs');
const {VM, NodeVM} = require('vm2');

// ruleid:vm2-context-injection
async function test1(input) {
  code = `
    console.log("Hello world")
  `;

  const sandbox = {
    setTimeout,
    watch: input
  };

  return new VM({timeout: 40 * 1000, sandbox}).run(code);
}

// ruleid:vm2-context-injection
function test2(input) {
  const sandbox = {
    setTimeout,
    input
  };

  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
  return nodeVM.run('console.log("Hello world")')
}

// ok
async function okTest1() {
  code = `
    console.log("Hello world")
  `;

  const sandbox = {
    setTimeout,
    fs
  };

  return new VM({timeout: 40 * 1000, sandbox}).run(code);
}

// ok
function okTest2() {
  const sandbox = {
    setTimeout,
    fs
  };

  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
  return nodeVM.run('console.log("Hello world")')
}