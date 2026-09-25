void check(NSString *host, NSString *title) {
  //ERROR:
  BOOL a = [host containsString:@".example.com"];

  BOOL b = [host hasSuffix:@".example.com"];
}
