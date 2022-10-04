// https://github.com/returntocorp/semgrep/issues/4509

app.get('/api/v1/users/filter', (req, res) => {
    var where = ""
    var query ="SELECT * FROM users WHERE ";
    var data = []
  
    for (var i = 0; i < Object.keys(req.query).length; i++) {
      param = req.query[Object.keys(req.query)[i]]
      where += (param+"=?")
      data.push(req.query[param]);
    }
    query+=where.join(" AND ");
    // ruleid: test
    db.query(query, data, (err,rows) => {
      if(err) return res.send(JSON.stringify({}));
      return res.send(JSON.stringify(rows));
    });  
  });
  
  app.get('/api/v1/users/filter2', (req, res) => {
    var where = ""
    var query ="SELECT * FROM users WHERE ";
    var data = []
  
    for (var param in req.query) {
      where += (param+"=?")
      data.push(req.query[param]);
    }
    query+=where.join(" AND ");
    // ruleid: test
    db.query(query, data, (err,rows) => {
      if(err) return res.send(JSON.stringify({}));
      return res.send(JSON.stringify(rows));
    });  
  });
  
  app.get('/api/v1/users/filter2', (req, res) => {
    var where = [];
    var query ="SELECT * FROM users WHERE ";
    var data = []
  
    for (var param in req.query) {
      where.push(param+"=?")
      data.push(req.query[param]);
    }
    query+=where.join(" AND ");
    // ruleid: test
    db.query(query, data, (err,rows) => {
      if(err) return res.send(JSON.stringify({}));
      return res.send(JSON.stringify(rows));
    });  
  });