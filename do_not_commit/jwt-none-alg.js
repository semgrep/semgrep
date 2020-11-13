function verifyJwt() {
	// ruleid: jwt-none-alg
	let jwt = require("jsonwebtoken");
	let secret = 'some-secret';
	jwt.verify('token-here', secret, { algorithms: ['RS256', 'none'] }, function(err, payload) {
		console.log(payload);
	});
}

// ok
const jwt = require("jsonwebtoken");
const secret = 'some-secret';
const payload = jwt.verify('token-here', secret, { algorithms: ['RS256', 'HS256'] });