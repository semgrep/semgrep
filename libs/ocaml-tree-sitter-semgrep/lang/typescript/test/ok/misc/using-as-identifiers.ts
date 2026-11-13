// `using` and `as` are contextual keywords and valid variable names.
// They must parse as ordinary identifiers (semgrep/semgrep#12075).
// TypeScript always; JavaScript when the file also contains `??`.
const using = /x(.*)/.exec(process.argv[2]);
if (using) console.log(using[1] ?? '');
eval(process.argv[2]);

let as = -1;
f(x ?? '');
eval(x);

const using2 = [1];
f(using2.length);
eval(x);

let as2 = -1;
as2 = 1;
eval(x);

using;
using[1];
using.length;
let satisfies = 1;
using res = getResource();
await using res2 = getResource();
export using res3 = getResource();
for (using item of items) {
  console.log(item);
}
