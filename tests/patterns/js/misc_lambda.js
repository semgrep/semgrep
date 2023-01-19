var foo = "bar";

// not a Lambda
function foo () {
    console.log(foo);
}

// not allowed by Ast_js_build
//function () {
//    console.log(foo);
//}

// this used to be not parsed as a Lambda, even though it looks like, because
// this common idiom was handled specially by Ast_js_build, but
// we don't this anymore.
//ERROR: match
const a = function () {
    console.log(foo);
};

// this is finally parsed as a Lambda
var b = foo(
   //ERROR: match
   function () { console.log("here"); }
);
