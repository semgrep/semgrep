void handle(UIApplication *app, NSURL *u) {
  //ERROR:
  [app openURL:u options:@{} completionHandler:nil];

  [app openURL:u options:@{}];
}
