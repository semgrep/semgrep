let { a, b = 2 } = foo;

function prettierError(err) {
  let { details = '', origin } = err;
}

const { object = {}, property = {} } = name;

let [/*match*/, theirName, myName = theirName] = tokens;
