// this does NOT work
app.get('/ok', async (req, res) => {
  const tainted = req.params;
  async.parallel({
    somefunc(cb) {
      // below sql injection isn't detected
      // ruleid: tainted-sql-string
      return `SELECT * FROM ${tainted}`
    }
  })
})

// this seems to work
app.get('/ok', async (req, res) => {
  const tainted = req.params;
  async.parallel([
    (cb) => {
      return `SELECT * FROM ${tainted}`
    }
  ])
})
