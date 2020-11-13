'use strict';

const fs = require('fs');
const {VM, NodeVM} = require('vm2');


async function test1(code, input) {
  code = `
    console.log(${input})
  `;

  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

// ruleid: vm2-code-injection
  return new VM({
    timeout: 40 * 1000,
    sandbox
  }).run(code);
}

function test2(input) {
  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

// ruleid: vm2-code-injection
  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
  return nodeVM.run('console.log(' + input + ')')
}

function test3(input) {
  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
// ruleid: vm2-code-injection
  const script = new VMScript(`console.log(${input})`)
  return nodeVM.run(script)
}

async function okTest1(code) {
  code = `
    console.log("Hello world")
  `;

  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

  return new VM({
    timeout: 40 * 1000,
    sandbox
  }).run(code);
}

function okTest2() {
  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
  return nodeVM.run('console.log("Hello world")')
}

function okTest3() {
  const sandbox = {
    setTimeout,
    fs: {
      watch: fs.watch
    }
  };

  const nodeVM = new NodeVM({timeout: 40 * 1000, sandbox});
  const script = new VMScript('console.log("Hello world")')
  return nodeVM.run(script)
}
