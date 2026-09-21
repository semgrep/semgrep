void handle(UIApplication *app, NSURL *u, BOOL ok) {
  if (ok) {
    //ERROR:
    [app openURL:u options:@{} completionHandler:nil];
  }
  while (ok) {
    //ERROR:
    [app openURL:u options:@{} completionHandler:nil];
  }
  for (int i = 0; i < 3; i++) {
    //ERROR:
    [app openURL:u options:@{} completionHandler:nil];
  }
  @try {
    //ERROR:
    [app openURL:u options:@{} completionHandler:nil];
  } @catch (NSException *e) {
  }
}
