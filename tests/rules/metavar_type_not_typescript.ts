function displayMessage(req: Request, res: Response) {
  const message = req.query.message;

  // ruleid: no-direct-response-write
  res.send(`<h1>${message}</h1>`);

  // ok: no-direct-response-write
  res.send(`<h1>constant string</h1>`);
}
