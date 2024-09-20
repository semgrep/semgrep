const express = require('express')
const app = express()
const port = 3000
const { Sequelize } = require('sequelize');
const sequelize = new Sequelize('sqlite::memory:')
const util = require('util')

app.get('/test', (req, res) => {
  //ruleid: taint-test
  res.send("SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'")
})

app.get('/test1', (req, res) => {
  //ruleid: taint-test
  await res.send("SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'")
})

