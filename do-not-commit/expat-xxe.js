function test1(input) {
    // ruleid: expat-xxe
    var expat = require('node-expat')
    var parser = new expat.Parser('UTF-8')
    parser.parse(input)
}

function test2(input) {
    // ruleid: expat-xxe
    const {Parser} = require('node-expat')
    const parser = new Parser('UTF-8')
    parser.write(input)
}

function okTest3() {
    // ok
    var expat = require('node-expat')
    var parser = new expat.Parser('UTF-8')
    parser.parse("safe input")
}

function okTest4() {
    // ok
    const {Parser} = require('node-expat')
    const parser = new Parser('UTF-8')
    const x = "safe input"
    parser.write(x)
}

function okTest5(input) {
    // ok
    const {Parser} = require('some-other-module')
    const parser = new Parser('UTF-8')
    parser.write(input)
}
