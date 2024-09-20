const { ok } = require('assert');
const jwt = require('jsonwebtoken');

token = getToken();
// ruleid: jwt-decode-without-verify
if (jwt.decode(token, true).param === true) {
  console.log('token is valid');
}

function ok(token, key) {
  jwt.verify(token, key);
  // ok: jwt-decode-without-verify
  if (jwt.decode(token, true).param === true) {
    console.log('token is valid');
  }
}

const ok2 = (token, key) => {
  jwt.verify(token, key);
  // ok: jwt-decode-without-verify
  if (jwt.decode(token, true).param === true) {
    console.log('token is valid');
  }
};

function bad_different_token(token, key) {
    token2 = getToken();
    jwt.verify(token2, key);

    // ruleid: jwt-decode-without-verify
    if (jwt.decode(token, true).param === true) {
      console.log('token is valid');
    }
}
