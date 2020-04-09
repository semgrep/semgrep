//ERROR: match
const jwt = require('jsonwebtoken');
const secret = "hardcoded-secret";
const auth = () => {
    jwt.verify(token, secret, function (err, decoded) {
        if (err) {
            reject(err);
        } else {
                resolve(decoded);
        }
    });
}
