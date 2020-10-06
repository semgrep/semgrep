var foo = "bar";

// not a Lambda
function foo () {
    console.log(foo);
}

// not allowed by Ast_js_build
//function () {
//    console.log(foo);
//}

// still not parsed as a Lambda, even though it looks like, because
// this common idiom is handled specially by Ast_js_build
const a = function () {
    console.log(foo);
};

// this is finally parsed as a Lambda
var b = foo(
   //ERROR: match
   function () { console.log("here"); }
);
