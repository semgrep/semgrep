/*
  In some cases, 'token()' produces a node and in other cases it doesn't.

  We work around this in module Name_pattern.ml.
*/
module.exports = grammar({
  name: 'inline_token',
  rules: {
    program: $ => choice(
      token('hello'),                    // ok
      token(seq('good', 'bye')),         // no node
      token(choice('a', 'b')),           // no node
      //token(repeat('c')),              // memory explosion on any input
      token(repeat1('d')),               // no node
      //token(optional('e')),            // memory explosion on any input
      token(prec.dynamic(0, 'f')),       // ok
      token(prec(0, 'g')),               // ok
      token(prec.left(0, 'h')),          // ok
      token(prec.right(0, 'i')),         // ok
      token(prec.dynamic(0, seq('j'))),  // no node
      token(token('k')),                 // no node

      token.immediate('l'),                   // ok
      token.immediate(seq('m', 'm')),         // no node
      token.immediate(choice('n', 'o')),      // no node
      //token.immediate(repeat('p')),         // memory explosion on any input
      token.immediate(repeat1('q')),          // no node
      //token.immediate(optional('r')),       // memory explosion on any input
      token.immediate(prec.dynamic(0, 's')),  // ok
      token.immediate(prec(0, 't')),          // ok
      token.immediate(prec.left(0, 'u')),     // ok
      token.immediate(prec.right(0, 'v')),    // ok
      token.immediate(prec.dynamic(0, seq('w'))),  // no node
      token.immediate(token.immediate('x')),  // no node

      token(token.immediate('y')),            // no node
      token.immediate(token('z')),            // no node

      field('foo1', token('field1')),         // ok
      field('foo2', token(seq('field2'))),    // no node
    ),
  }
});
