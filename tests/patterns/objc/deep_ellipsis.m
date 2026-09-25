void f(NSURLRequest *req, Logger *log) {
  //ERROR:
  [log record:[[req url] absoluteString]];

  [log record:@"static"];
}
