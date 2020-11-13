const config = require('./config')

function example1() {
// ruleid: hardcoded-jwt-secret
  const jsonwt = require('jsonwebtoken')
  const payload = {foo: 'bar'}
  const secret = 'shhhhh'
  const token1 = jsonwt.sign(payload, secret)
}

function example2() {
// ruleid: hardcoded-jwt-secret
  const jsonwt = require('jsonwebtoken')
  const payload = {foo: 'bar'}
  const token2 = jsonwt.sign(payload, 'some-secret')
}

function example3() {
// ok
  const jsonwt = require('jsonwebtoken')
  const payload = {foo: 'bar'}
  const token3 = jsonwt.sign(payload, config.secret)
}

function example4() {
// ok
  const jsonwt = require('jsonwebtoken')
  const payload = {foo: 'bar'}
  const secret2 = config.secret
  const token4 = jsonwt.sign(payload, secret2)
}

function example5() {
// ok
  const jsonwt = require('jsonwebtoken')
  const payload = {foo: 'bar'}
  const secret3 = process.env.SECRET || 'fallback-secret'
  const token5 = jsonwt.sign(payload, secret3)
}
