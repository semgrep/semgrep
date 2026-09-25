void handle(UIApplication *app, NSURL *u) {
  //ERROR:
  [app openURL:u options:@{} completionHandler:nil];

  [app canOpenURL:u];
}
