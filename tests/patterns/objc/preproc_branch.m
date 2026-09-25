void handle(UIApplication *app, NSURL *u, NSURL *v) {
#if DEBUG
  //ERROR:
  [app openURL:u options:@{} completionHandler:nil];
#else
  //ERROR:
  [app openURL:v options:@{} completionHandler:nil];
#endif
}
