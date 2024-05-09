app.get('/ok', async (req, res) => {
  const tainted = req.params;
  async.parallel({
    somefunc(cb) {
      // ruleid: test
      return `SELECT * FROM ${tainted}`
    }
  })
})

// this seems to work
app.get('/ok', async (req, res) => {
  const tainted = req.params;
  async.parallel([
      (cb) => {
      // ruleid: test
      return `SELECT * FROM ${tainted}`
    }
  ])
})
