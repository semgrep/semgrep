for (xx of yy) {
}

for (var xx of yy) {
}


var ui32 = new Uint32Array(4).fill(42);
var e = Buffer.from(ui32);
for (var {index, value} of e.entries()) {
  assert.strictEqual(value, ui32[index]);
}
