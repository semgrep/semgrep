const vm = require('vm')

let ctrl1 = function test1(req,res) {
// ruleid:express-vm-runincontext-context-injection
    var input = req.query.something || ''
    var sandbox = {
        foo: input
    }
    vm.createContext(sandbox)
    vm.runInContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
    res.send('hello world')
}
app.get('/', ctrl1)

app.get('/', (req,res) => {
// ruleid:express-vm-runincontext-context-injection
    var sandbox = {
        foo: req.query.userInput
    }
    vm.createContext(sandbox)
    vm.runInContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
    res.send('hello world')
})

// ok
function testOk1(userInput) {
    var sandbox = {
        foo: 1
    }
    vm.createContext(sandbox)
    vm.runInContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
}

var ctrl2 = null;
ctrl2 = function test2(req,res) {
// ruleid:express-vm-runinnewcontext-context-injection
    var input = req.query.something || ''
    var sandbox = {
        foo: input
    }
    vm.runInNewContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
    res.send('hello world')
}
app.get('/', ctrl2)


app.get('/', function (req,res) {
// ruleid:express-vm-runinnewcontext-context-injection
    var sandbox = {
        foo: req.query.userInput
    }
    vm.runInNewContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
    res.send('hello world')
})

// ok
app.get('/', function testOk1(userInput) {
    var sandbox = {
        foo: 1
    }
    vm.runInNewContext('safeEval(orderLinesData)', sandbox, { timeout: 2000 })
    res.send('hello world')
})

app.get('/', function(req,res) {
// ruleid:express-vm-code-injection
    const code = `
        var x = ${req.query.userInput};
    `
    vm.runInThisContext(code)
    res.send('hello world')
})

// ok
app.get('/', function okTest3(req,res) {
    const code = `
        var x = 1;
    `
    vm.runInThisContext(code)
    res.send('hello world')
})

app.get('/', function test4(req,res) {
    const parsingContext = vm.createContext({name: 'world'})
// ruleid:express-vm-code-injection
    const code = `return 'hello ' + ${req.query.userInput}`
    let fn = vm.compileFunction(code, [], { parsingContext })
    res.send('hello world')
})

// ok
app.get('/', function okTest4(req,res) {
    const parsingContext = vm.createContext({name: 'world'})
    const code = `return 'hello ' + name`
    const fn = vm.compileFunction(code, [], { parsingContext })
})

app.get('/', (req,res) => {
// ruleid:express-vm-compilefunction-context-injection
    const context = vm.createContext({name: req.query.userInput})
    let code = `return 'hello ' name`
    const fn = vm.compileFunction(code, [], { parsingContext: context })
    res.send('hello world')
})

// ok
app.get('/', function okTest5(req, res) {
    const parsingContext = vm.createContext({name: 'world'})
    const code = `return 'hello ' + name`
    const fn = vm.compileFunction(code, [], { parsingContext })
    res.send('hello world')
})

app.get('/', function (req,res) {
// ruleid:express-vm-code-injection
    const script = new vm.Script(`
        function add(a, b) {
          return a + ${req.query.userInput};
        }

        const x = add(1, 2);
    `);

    script.runInThisContext();
    res.send('hello world')
})

//ok
app.get('/', function okTest6(req, res) {
    const script = new vm.Script(`
        function add(a, b) {
          return a + b;
        }

        const x = add(1, 2);
    `);

    script.runInThisContext();
    res.send('hello world')
})
