/*
  @flow

  Flow is an extension of JavaScript, like TypeScript but lighter.
  - source files normally use the '.js' extension.
  - the string '@flow' in a comment seems to indicate the syntax is Flow
    rather than standard JavaScript.

  Article about Flow syntax extensions:
    https://zhenyong.github.io/flowtype/docs/syntax.html

  Here's a request to publish the grammar:
    https://github.com/facebook/flow/issues/3429
*/
export type Config = {
  silent: boolean;
}
