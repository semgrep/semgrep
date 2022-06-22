// https://github.com/returntocorp/semgrep/issues/554

//ERROR:
var foo = 'sensitive_var=test1';

//ERROR:
var foo = 'otherdata=foo&sensitive_var=test-2&moredata=bar';

var LITERAL1 = 'test-3'
//ERROR:
var foo = `otherdata=foo&sensitive_var=${LITERAL1}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL1 + '&moredata=bar';

const LITERAL2 = 'test-4'
//ERROR:
var foo = `otherdata=foo&sensitive_var=${LITERAL2}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL2 + '&moredata=bar';

let LITERAL3 = 'test-4'
//ERROR:
var foo = `&otherdata=foo&sensitive_var=${LITERAL3}&moredata=bar`;
//ERROR:
var foo = 'otherdata=foo&sensitive_var=' + LITERAL3 + '&moredata=bar';
