
function foo(...args) {
  console.log("hello world");
}

foo();

function bar(x?) { }
function qux(x=42) { }

var f: (x?:number,...y:Array<number>)=>void;
