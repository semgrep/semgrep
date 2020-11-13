// ruleid: jwt-decode-without-verify
const jwt = require('jsonwebtoken');

if (jwt.decode(token, true).param === true) {
  console.log('token is valid');
}
