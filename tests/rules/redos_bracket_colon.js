// this function was in a query-builder.js file and the regexp
// below starting with [: used to cause the redos analyzer
// to throw an exn.
// See https://linear.app/semgrep/issue/SAF-1693/scan-failures-with-failure-lexing-empty-token-on-a-few-projects
// for more context

function(str) {
    return (str) ? str.replace(/(\\)?([:.\[\],])/g,
            function( $0, $1, $2 ) { return $1 ? $0 : '\\' + $2; }) : str;
}
