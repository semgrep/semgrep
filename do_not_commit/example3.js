// ruleid: hardcoded-jwt-secret
const jwt = require('jsonwebtoken')

const jwtSign = (payload = { id: 1 }) =>
  jwt.sign(payload, 'hardcoded-secret')

const jwtVerify = req => () => new Promise((resolve, reject) => {
  const token = req.headers['x-access-token']
  if (!token) {
    resolve(false)
  }
  jwt.verify(token, 'hardcoded-secret', (err, decoded) => {
    if (err) {
      resolve(false)
    }
    resolve(decoded)
  })
})

export default {jwtSign, jwtVerify}
