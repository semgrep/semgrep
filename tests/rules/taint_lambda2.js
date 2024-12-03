let db;
foo(() => {
  db = client();
});

bar((req, res) => {
  // ruleid: test
  db.sink(req.query.id);
});
