const express = require('express')
const app = express()
const port = 3000
const { Sequelize } = require('sequelize');
const sequelize = new Sequelize('sqlite::memory:')
const util = require('util')

app.get('/test', (req, res) => {
  //ERROR:
  res.send("SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'")
})

app.get('/test1', (req, res) => {
  //ERROR:
  await res.send("SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'")
})

