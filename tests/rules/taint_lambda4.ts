function test1(req: Request) {
  //ruleid: test
  sink(req.query + foobar);
};

function test2() {
  return (req: Request) => {
    //ruleid: test
    sink(req.query + foobar);
  };
};
