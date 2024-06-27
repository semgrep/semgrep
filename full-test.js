const express = require('express')
const app = express()
const port = 3000
const { Sequelize } = require('sequelize');
const sequelize = new Sequelize('sqlite::memory:')
const util = require('util')

app.get('/test', (req, res) => {
  // ruleid: tainted-sql-string
  const query = "SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'"
  const [results, metadata] = await sequelize.query(query);
  res.send(results)
})

app.get('/test1', (req, res) => {
  // ruleid: tainted-sql-string
  const [results, metadata] = await sequelize.query("SELECT * FROM `users`" + " WHERE id = '" + req.query.message + "'");
  res.send(results)
})

app.get('/test2', (req, res) => {
  // ruleid: tainted-sql-string
  let query = `SELECT * FROM users WHERE id = '${req.query.message}'`
  const [results, metadata] = await sequelize.query(query);
  res.send(results)
})

app.get('/test3', (req, res) => {
  let query = "SELECT * FROM `users` WHERE id = '"
  // ruleid: tainted-sql-string
  query = query.concat(req.query.message)
  query = query.concat("'")
  const [results, metadata] = await sequelize.query(query);
  res.send(results)
})

app.get('/test4', (req, res) => {
  // ruleid: tainted-sql-string
  const query = util.format("SELECT * FROM users WHERE id = '%s'", req.query.message)
  const [results, metadata] = await sequelize.query(query);
  res.send(results)
})

app.get('/ok', async (req, res) => {
    // ok: tainted-sql-string
    res.send("message: " + req.query.message);
})

app.post('/ok2', async (req, res) => {
    // ok: tainted-sql-string
    res.send(`message: ${req.query.message}`);
})

app.post('/ok3', async (req, res) => {
    // ok: tainted-sql-string
    var data = "message: " + req.query.message;
    res.send(data);
})

app.post('/ok4', async (req, res) => {
    var data = "message: "
    // ok: tainted-sql-string
    data = data.concat(req.query.message)
    res.send(data);
})

app.listen(port, () => console.log(`Example app listening at http://localhost:${port}`))

var async = require('async')

// this does NOT work
app.get('/ok', async (req, res) => {
  const someTaintedData = req.params; // tainted
  // ruleid: tainted-sql-string
  const firstTaintedString = `SELECT * FROM ${someTaintedData}`
  async.parallel({
    somefunc(para_cb) {
      // below sql injection isn't detected
      // ruleid: tainted-sql-string
      const secondTaintedString = `SELECT * FROM ${someTaintedData}`
      para_cb()
      return
    }
  })
})