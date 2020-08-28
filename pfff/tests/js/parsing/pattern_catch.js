try {
  throw foo;
} catch ({ message }) {
  console.log(message);
}

try {
  throw foo;
} catch (message) {
  console.log(message);
}
