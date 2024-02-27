function test() {
  const { body: { param } } = tainted
  // ruleid: test
  sink(param)
}
