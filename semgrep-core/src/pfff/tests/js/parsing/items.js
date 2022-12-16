// when have this if stmt, this cause the parsing to fail on the next if stmt
if (vueVersion && vueVersion !== packageVersion) {
}

// note that an if can have an else, so to reduce
// to a statement the parser looks ahead the next token (here 'if')
// to decide to reduce or shift, so it "consunes" the next
// token before the next round ...

// How to know which token stream to build then for the next round?
// There is no clear API for that in Parsing but because we build
// a concrete syntax tree, the tokens are in the AST so we just need
// to look if the current token is in it or not.

if (process.env.NODE_ENV === 'production') {
    module.exports = require('./build.prod.js');
} else {
    module.exports = require('./build.dev.js');
}

// same issue with catch, looking ahead for 'finally'

try { } catch(x) { }

if(true) { } else { }
