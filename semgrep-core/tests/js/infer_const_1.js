// https://github.com/returntocorp/semgrep/issues/554

//ERROR:
var foo = 'sensitive_var=test1';

//ERROR:
var foo = 'otherdata=foo&sensitive_var=test-2&moredata=bar';

var LITERAL = 'test-3'
//ERROR:
var foo = `otherdata=foo&sensitive_var=${LITERAL}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL + '&moredata=bar';

const LITERAL = 'test-4'
//ERROR:
var foo = `otherdata=foo&sensitive_var=${LITERAL}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL + '&moredata=bar';

let LITERAL = 'test-4'
//ERROR:
var foo = `&otherdata=foo&sensitive_var=${LITERAL}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL + '&moredata=bar';
