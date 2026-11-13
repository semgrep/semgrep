// `using` and `as` are contextual keywords and valid variable names
// (semgrep/semgrep#12075). A parse gap here used to swallow the
// following statement, so `eval(...)` was silently missed.
const using = /x(.*)/.exec(process.argv[2]);
if (using) console.log(using[1] ?? '');
//ERROR:
eval(process.argv[2]);

let as = -1;
f(x ?? '');
//ERROR:
eval(x);

const using2 = [1];
f(using2.length);
//ERROR:
eval(x);

let as2 = -1;
as2 = 1;
//ERROR:
eval(x);
